import { Result } from "../../core/logic/Result";
import { IAllergyDTO } from "../../dto/IAllergyDTO";
import { IAllergyQueryFilterParameters } from "../../dto/IAllergyQueryFilterParameters";
import { IAllergyUpdateDTO } from "../../dto/IAllergyUpdateDTO";

export default interface IAllergyService  {
  getAllergyByCode(arg0: string): Result<IAllergyDTO> | PromiseLike<Result<IAllergyDTO>>;
  createAllergy(allergyDTO: IAllergyDTO): Promise<Result<IAllergyDTO>>;
  updateAllergy(allergyDTO: IAllergyUpdateDTO): Promise<Result<IAllergyDTO>>;
  getAllAllergies(): Promise<Result<IAllergyDTO[]>>;
  getAllergiesByFilters(arg0: IAllergyQueryFilterParameters): Result<IAllergyDTO[]> | PromiseLike<Result<IAllergyDTO[]>>;

}
