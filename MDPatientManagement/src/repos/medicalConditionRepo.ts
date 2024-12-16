import { Service, Inject } from 'typedi';
import { Document, FilterQuery, Model } from 'mongoose';
import IMedicalConditionRepo from '../services/IRepos/IMedicalConditionRepo';
import { IMedicalConditionPersistence } from '../dataschema/IMedicalConditionPersistence';
import { MedicalCondition } from '../domain/medicalCondition';
import { MedicalConditionId } from '../domain/medicalConditionId';
import { MedicalConditionMap } from '../mappers/MedicalConditionMap';

@Service()
export default class MedicalConditionRepo implements IMedicalConditionRepo {
  private models: any;

  constructor(
    @Inject('medicalConditionSchema') private medicalConditionSchema : Model<IMedicalConditionPersistence & Document>,
  ) {}

  private createBaseQuery (): any {
    return {
      where: {},
    }
  }

  public async exists(medicalCondition: MedicalCondition): Promise<boolean> {
    
    const idX = medicalCondition.id instanceof MedicalConditionId ? (<MedicalConditionId>medicalCondition.id).toValue() : medicalCondition.id;

    const query = { domainId: idX}; 
    const medicalConditionDocument = await this.medicalConditionSchema.findOne( query as FilterQuery<IMedicalConditionPersistence & Document>);

    return !!medicalConditionDocument === true;
  }

  public async save(medicalCondition: MedicalCondition): Promise<MedicalCondition> {
    const query = { domainId: medicalCondition.medicalConditionId.toString() };

    const medicalConditionDocument = await this.medicalConditionSchema.findOne(query);

    try {
        if (medicalConditionDocument !== null) {
            throw new Error(`Medical condition with ID ${medicalCondition.medicalConditionId} already exists.`);
        }

        const rawMedicalCondition: any = MedicalConditionMap.toPersistence(medicalCondition);

        const medicalConditionCreated = await this.medicalConditionSchema.create(rawMedicalCondition);

        return MedicalConditionMap.toDomain(medicalConditionCreated);
    } catch (err) {
        throw err;
    }
  }


  public async findByDomainId (medicalConditionId: MedicalConditionId | string): Promise<MedicalCondition> {
    const query = { domainId: medicalConditionId};
    const medicalConditionRecord = await this.medicalConditionSchema.findOne( query as FilterQuery<IMedicalConditionPersistence & Document> );

    if( medicalConditionRecord != null) {
      return MedicalConditionMap.toDomain(medicalConditionRecord);
    }
    else
      return null;
  }
}