import { Service, Inject } from 'typedi';
import config from '../../config';

import IAllergyService from './IServices/IAllergyService';
import { AllergyMap } from '../mappers/AllergyMap';
import { IAllergyDTO } from '../dto/IAllergyDTO';
import { IAllergyUpdateDTO } from '../dto/IAllergyUpdateDTO';

import IAllergyRepo from './IRepos/IAllergyRepo';
import { Allergy } from '../domain/allergy';

import { Result } from "../core/logic/Result";

@Service()
export default class AllergyService implements IAllergyService{
  constructor(
      @Inject(config.repos.allergy.name) private allergyRepo : IAllergyRepo,
      @Inject('logger') private logger,
  ) {}


  async createAllergy(allergyDTO: IAllergyDTO): Promise<Result<IAllergyDTO>> {
    try {

      const allergyOrError = await Allergy.create( allergyDTO );
      if (allergyOrError.isFailure) {
        return Result.fail<IAllergyDTO>(allergyOrError.errorValue());
      }

      const allergyResult = allergyOrError.getValue();

      await this.allergyRepo.save(allergyResult);

      const allergyDTOResult = AllergyMap.toDTO( allergyResult ) as IAllergyDTO;
      return Result.ok<IAllergyDTO>( allergyDTOResult )
    } catch (e) {
      throw e;
    }
  }


  async updateAllergy(allergyDTO: IAllergyUpdateDTO): Promise<Result<IAllergyDTO>> {
    try {
          const allergy = await this.allergyRepo.findByCode(allergyDTO.code);
    
          if (allergy === null) {
            return Result.fail<IAllergyDTO>("Allergy not found");
          }
          else {
            allergy.changeDesignation(allergyDTO.designation)
            allergy.changeDescription(allergyDTO.description);
            await this.allergyRepo.save(allergy);
    
            const allergyDTOResult = AllergyMap.toDTO( allergy ) as IAllergyDTO;
            return Result.ok<IAllergyDTO>( allergyDTOResult )
            }
        } catch (e) {
          throw e;
        }
  }

  async getAllergyByCode(code: string): Promise<Result<IAllergyDTO>> {
    try {
      const allergy = await this.allergyRepo.findByCode(code);
      if (!allergy) {
        return Result.fail<IAllergyDTO>("Allergy not found");
      }

      const allergyDTO = AllergyMap.toDTO(allergy) as IAllergyDTO;
      return Result.ok<IAllergyDTO>(allergyDTO);
    } catch (error) {
      throw new Error(`Failed to fetch allergy: ${error.message}`);
    }
  }


  async getAllAllergies(): Promise<Result<IAllergyDTO[]>> {
    try {
      const allergies = await this.allergyRepo.findAll();

      if (allergies === null || allergies.length == 0) {
        return Result.fail<IAllergyDTO[]>("Allergies not found");
      }
      else {
        const allergiesListDTOResult = allergies.map((allergy) => AllergyMap.toDTO(allergy) as IAllergyDTO);
        return Result.ok<IAllergyDTO[]>( allergiesListDTOResult )
        }
    } catch (e) {
      throw e;
    }
  }

}
