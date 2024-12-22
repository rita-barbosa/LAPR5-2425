import { Service, Inject } from 'typedi';
import IMedicalRecordService from "./IServices/IMedicalRecordService";
import config from "../../config";
import { Result } from '../core/logic/Result';
import { IMedicalRecordDTO } from '../dto/IMedicalRecordDTO';
import { MedicalRecord } from '../domain/medicalRecord';
import { MedicalRecordMap } from '../mappers/MedicalRecordMap';
import IMedicalRecordRepo from './IRepos/IMedicalRecordRepo';
import { MedicalConditionId } from '../domain/medicalConditionId';
import { AllergyCode } from '../domain/allergyCode';
import { IMedicalRecordQueryFilterParameters } from '../dto/IMedicalRecordQueryFilterParameters';

@Service()
export default class MedicalRecordService implements IMedicalRecordService {
    constructor(
        @Inject(config.repos.medicalRecord.name) private medicalRecordRepo: IMedicalRecordRepo,
        @Inject('logger') private logger,
    ) { }


    async createMedicalRecord(medicalRecordDTO: IMedicalRecordDTO): Promise<Result<IMedicalRecordDTO>> {
        try {

            const medicalRecord = await this.medicalRecordRepo.findByDomainId(medicalRecordDTO.id);

            if (medicalRecord != null) {
                throw new Error("Already exists a Patient Medical Record with that ID!");
            }

            const medicalRecordOrError = await MedicalRecord.create(medicalRecordDTO);
            if (medicalRecordOrError.isFailure) {
                return Result.fail<IMedicalRecordDTO>(medicalRecordOrError.errorValue());
            }

            const medicalRecordResult = medicalRecordOrError.getValue();

            await this.medicalRecordRepo.save(medicalRecordResult);

            const medicalRecordDTOResult = MedicalRecordMap.toDTO(medicalRecordResult) as IMedicalRecordDTO;
            return Result.ok<IMedicalRecordDTO>(medicalRecordDTOResult)
        } catch (e) {
            throw e;
        }
    }

    async updateMedicalRecord(medicalRecordDTO: IMedicalRecordDTO): Promise<Result<IMedicalRecordDTO>> {
        try {
            const medicalRecord = await this.medicalRecordRepo.findByDomainId(medicalRecordDTO.id);

            if (medicalRecord === null) {
                return Result.fail<IMedicalRecordDTO>("Patient Medical Record not found!");
            }

            else {
                const medicalConditionObjects = (medicalRecordDTO.medicalConditions || []).map(
                    (condition) => new MedicalConditionId(condition.toString())
                );

                const allergyObjects = (medicalRecordDTO.allergies || []).map(
                    (allergy) => new AllergyCode(allergy.toString())
                );

                medicalRecord.changeMedicalConditions(medicalConditionObjects);
                medicalRecord.changeAllergies(allergyObjects);
                medicalRecord.changeDescription(medicalRecordDTO.description);

                console.log("\nTHE FIRST TIME I SEE ALLERGIES:", medicalRecord.allergies);
                await this.medicalRecordRepo.save(medicalRecord);

                const medicalRecordDTOResult = MedicalRecordMap.toDTO(medicalRecord) as IMedicalRecordDTO;
                return Result.ok<IMedicalRecordDTO>(medicalRecordDTOResult);
            }
        } catch (e) {
            throw e;
        }
    }

    async getAllMedicalRecords(): Promise<Result<IMedicalRecordDTO[]>> {
        try {
            const records = await this.medicalRecordRepo.findAll();

            


            if (records === null || records.length == 0) {
            return Result.fail<IMedicalRecordDTO[]>("Medical Records not found");
            }
            else {

                const recordsListDTOResult = records.map((record) => MedicalRecordMap.toDTO(record) as IMedicalRecordDTO);
                // // console.log(recordsListDTOResult[0].allergies)
            return Result.ok<IMedicalRecordDTO[]>( recordsListDTOResult )
            }
        } catch (e) {
            throw e;
        }
    }

    async getMedicalRecordsByFilters(filters: IMedicalRecordQueryFilterParameters): Promise<Result<IMedicalRecordDTO[]>> {
        try {
            const records = await this.medicalRecordRepo.findAllByParameters(filters);
            if (records.length == 0) {
            return Result.fail<IMedicalRecordDTO[]>("Medical records not found");
            }
    
            let medicalRecordsDtoList: IMedicalRecordDTO[] = [];
    
            for(var i = 0; i < records.length; i++){
            const allergyDTO = MedicalRecordMap.toDTO(records.at(i)) as IMedicalRecordDTO;
            medicalRecordsDtoList.push(allergyDTO);
            }
    
            return Result.ok<IMedicalRecordDTO[]>(medicalRecordsDtoList);
        } catch (error) {
            throw new Error(`Failed to fetch medical records: ${error.message}`);
        }
    }
}
