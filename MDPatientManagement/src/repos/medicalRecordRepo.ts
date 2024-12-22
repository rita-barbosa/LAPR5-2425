import { Inject, Service } from "typedi";
import IMedicalRecordRepo from "../services/IRepos/IMedicalRecordRepo";
import { IMedicalRecordPersistence } from "../dataschema/IMedicalRecordPersistence";
import { Document, FilterQuery, Model, Types } from "mongoose";
import { MedicalRecordMap } from "../mappers/MedicalRecordMap";
import { MedicalRecord } from "../domain/medicalRecord";
import { MedicalRecordId } from "../domain/medicalRecordId";
import { IMedicalRecordQueryFilterParameters } from "../dto/IMedicalRecordQueryFilterParameters";

@Service()
export default class MedicalRecordRepo implements IMedicalRecordRepo {
  private models: any;

  constructor(
    @Inject('medicalRecordSchema') private medicalRecordSchema: Model<IMedicalRecordPersistence & Document>,
    @Inject('logger') private logger
  ) { }

  public async save(medicalRecord: MedicalRecord): Promise<MedicalRecord> {

    const query = { domainId: medicalRecord.id.toString() };
    const medicalRecordDocument = await this.medicalRecordSchema.findOne(query);

    try {
      if (medicalRecordDocument === null) {
        const rawMedicalRecord: any = MedicalRecordMap.toPersistence(medicalRecord);

        const medicalRecordCreated = await this.medicalRecordSchema.create(rawMedicalRecord);

        return MedicalRecordMap.toDomain(medicalRecordCreated);
      } else {
        console.log("Updating existing medical record!");

        medicalRecordDocument.medicalConditions = medicalRecord.medicalConditions;
        medicalRecordDocument.allergies = medicalRecord.allergies;
        medicalRecordDocument.description = medicalRecord.description ? medicalRecord.description : '';

        await medicalRecordDocument.save();

        return medicalRecord;
      }
    } catch (err) {
      throw err;
    }
  }

  public async exists(medicalRecord: MedicalRecord): Promise<boolean> {

    const idX = medicalRecord.id instanceof MedicalRecordId ? (<MedicalRecordId>medicalRecord.id).toValue() : medicalRecord.id;

    const query = { domainId: idX };
    const medicalRecordDocument = await this.medicalRecordSchema.findOne(query as FilterQuery<IMedicalRecordPersistence & Document>);

    return !!medicalRecordDocument === true;
  }

  public async findByDomainId(medicalRecordId: MedicalRecordId | string): Promise<MedicalRecord> {
    const query = { domainId: medicalRecordId };
    const medicalRecordRecord = await this.medicalRecordSchema.findOne(query as FilterQuery<IMedicalRecordPersistence & Document>);

    if (medicalRecordRecord != null) {
      return MedicalRecordMap.toDomain(medicalRecordRecord);
    }
    else
      return null;
  }

    public async findAll(): Promise<MedicalRecord[]> {
        try {
          const medicalRecordRecords = await this.medicalRecordSchema.find({});
          const medicalRecordList = await Promise.all(
            medicalRecordRecords.map(async (record) => await MedicalRecordMap.toDomain(record))
          );
    
          return medicalRecordList;
        } catch (error) {
          this.logger.error("Error fetching all medical record:", error);
        }
      }

      public async findAllByParameters(
        filters: IMedicalRecordQueryFilterParameters
      ): Promise<MedicalRecord[]> {
        const medicalRecordsList: (IMedicalRecordPersistence & Document<any, any, any> & {
          _id: Types.ObjectId;
        })[] = [];

        // Use Promise.all for parallel queries
        const queryPromises = filters.queryfilters.map(filter => {
          const query: any = {};

          if (filter.allergyDesignation?.length > 0) {
            query.allergyDesignation = filter.allergyDesignation;
          }

          if (filter.medicalConditionDesignation?.length > 0) {
            query.medicalConditionDesignation = filter.medicalConditionDesignation;
          }

          console.log('QUERY', query)

          // Return the query promise
          return this.medicalRecordSchema.find(query).exec();
        });

        // Wait for all promises to resolve
        const results = await Promise.all(queryPromises);

        // Flatten the results into one array
        results.forEach(records => {
          medicalRecordsList.push(...records);
        });

        // Map the records to their domain representation
        return medicalRecordsList.length > 0
          ? medicalRecordsList.map(MedicalRecordMap.toDomain)
          : [];
      }

}
