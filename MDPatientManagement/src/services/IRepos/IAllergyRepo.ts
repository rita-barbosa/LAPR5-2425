import { Repo } from "../../core/infra/Repo";
import { Allergy } from "../../domain/allergy";
import { AllergyCode } from "../../domain/allergyCode";

export default interface IAllergyRepo extends Repo<Allergy> {
	save(allergy: Allergy): Promise<Allergy>;
	findByCode(code : string | AllergyCode) : Promise<Allergy>;
	findAll() : Promise<Allergy[]>
}
  