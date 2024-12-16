import { Inject, Service } from "typedi";
import IMedicalRecordService from "./IServices/IMedicalConditionService";
import config from "../../config";
import { Result } from "../core/logic/Result";
import IMedicalConditionDTO from "../dto/IMedicalConditionDTO";
import { MedicalCondition } from "../domain/medicalCondition";
import { MedicalConditionId } from "../domain/medicalConditionId";
import IMedicalConditionRepo from "./IRepos/IMedicalConditionRepo";
import { MedicalConditionMap } from "../mappers/MedicalConditionMap";
import IMedicalConditionService from "./IServices/IMedicalConditionService";


@Service()
export default class MedicalConditionService implements IMedicalConditionService {
  constructor(
      @Inject(config.repos.medicalCondition.name) private medicalConditionRepo : IMedicalConditionRepo
  ) {}

  public async createMedicalCondition(medicalConditionDTO: IMedicalConditionDTO): Promise<Result<IMedicalConditionDTO>> {
    try {
        const medicalConditionOrError = await MedicalCondition.create( medicalConditionDTO );
  
        if (medicalConditionOrError.isFailure) {
          return Result.fail<IMedicalConditionDTO>(medicalConditionOrError.errorValue());
        }
  
        const medicalConditionResult = medicalConditionOrError.getValue();
  
        await this.medicalConditionRepo.save(medicalConditionResult);
  
        const medicalConditionDTOResult = MedicalConditionMap.toDTO( medicalConditionResult ) as IMedicalConditionDTO;
        return Result.ok<IMedicalConditionDTO>( medicalConditionDTOResult )
      } catch (e) {
        throw e;
      }
    }

}
