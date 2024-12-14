import { Service, Inject } from 'typedi';

import { Document, FilterQuery, Model } from 'mongoose';
import { IAllergyPersistence } from '../dataschema/IAllergyPersistence';

import IAllergyRepo from '../services/IRepos/IAllergyRepo';
import { Allergy } from '../domain/allergy';
import { AllergyCode } from '../domain/allergyCode';
import { AllergyMap } from '../mappers/AllergyMap';

@Service()
export default class AllergyRepo implements IAllergyRepo {
  private models: any;

  constructor(
    @Inject('allergySchema') private allergyschema : Model<IAllergyPersistence & Document>,
    @Inject('logger') private logger
  ) { }



  async exists(allergy : Allergy): Promise<boolean> {
    const idX = allergy.code instanceof AllergyCode ? (<AllergyCode>allergy.code).toValue() : allergy.code;

    const query = { code: idX};
    const allergyDocument = await this.allergyschema.findOne( query as FilterQuery<IAllergyPersistence & Document>);

    return !!allergyDocument === true;
  }


  private createBaseQuery (): any {
    return {
      where: {},
    }
  }

  public async save(allergy: Allergy): Promise<Allergy> {

    const query = { code : allergy.code.toString() };
    const allergyDocument = await this.allergyschema.findOne(query);
    
    try {
      if (allergyDocument === null) {
        const rawAllergy: any = AllergyMap.toPersistence(allergy);
  
        const allergyCreated = await this.allergyschema.create(rawAllergy);
        const allergyDomain = AllergyMap.toDomain(allergyCreated);
  
        return allergyDomain;
      } else {
        console.log("Updating existing allergy");

        allergyDocument.designation = allergy.designation.value; 
        allergyDocument.description = allergy.description ? allergy.description.value : '';
  
        await allergyDocument.save();
  
        return allergy;
      }
    } catch (err) {
      console.error('Error saving allergy:', err);
      throw err;
    }
  }
  
  public async findByCode(code: string | AllergyCode): Promise<Allergy> {
    const idX = code instanceof AllergyCode ? (<AllergyCode>code).toValue() : code;

    const query = { code : idX }; 
    const allergyRecord = await this.allergyschema.findOne( query );

    if( allergyRecord != null) {
      return AllergyMap.toDomain(allergyRecord);
    }
    else
      return null;
  }

  public async findAll(): Promise<Allergy[]> {
    try {
      const allergyRecords = await this.allergyschema.find({});
      const allergyList = await Promise.all(
        allergyRecords.map(async (record) => await AllergyMap.toDomain(record))
      );

      return allergyList;
    } catch (error) {
      this.logger.error("Error fetching all allergies:", error);
    }
  }
  
}