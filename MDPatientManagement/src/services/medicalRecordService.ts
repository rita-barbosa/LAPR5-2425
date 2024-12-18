import { Service, Inject } from 'typedi';
import IMedicalRecordService from "./IServices/IMedicalRecordService";
import config from "../../config";
import { Result } from '../core/logic/Result';
import { IMedicalRecordDTO } from '../dto/IMedicalRecordDTO';
import { MedicalRecord } from '../domain/medicalRecord';
import { MedicalRecordMap } from '../mappers/MedicalRecordMap';
import IMedicalRecordRepo from './IRepos/IMedicalRecordRepo';

@Service()
export default class MedicalRecordService implements IMedicalRecordService {
    constructor(
        @Inject(config.repos.medicalRecord.name) private medicalRecordRepo: IMedicalRecordRepo,
        @Inject('logger') private logger,
    ) { }


    async createMedicalRecord(medicalRecordDTO: IMedicalRecordDTO): Promise<Result<IMedicalRecordDTO>> {
        try {
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
}