import { Result } from "../../core/logic/Result";
import IMedicalConditionDTO from "../../dto/IMedicalConditionDTO";

export default interface IMedicalConditionService  {
  createMedicalCondition(roleDTO: IMedicalConditionDTO): Promise<Result<IMedicalConditionDTO>>;
}
