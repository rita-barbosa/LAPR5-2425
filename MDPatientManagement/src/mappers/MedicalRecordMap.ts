import { Document, Model } from "mongoose";
import { Mapper } from "../core/infra/Mapper";
import { IMedicalRecordPersistence } from "../dataschema/IMedicalRecordPersistence";
import { MedicalRecord } from "../domain/medicalRecord";
import { IMedicalRecordDTO } from "../dto/IMedicalRecordDTO";

export class MedicalRecordMap extends Mapper<MedicalRecord> {

    public static toDTO(medicalRecord: MedicalRecord): IMedicalRecordDTO {
        // console.log(medicalRecord)
        return {
            id: medicalRecord.id.toString(),
            medicalRecordNumber: medicalRecord.props.medicalRecordNumber.value,
            medicalConditions: medicalRecord.medicalConditions.map(condition => condition.toString()),
            allergies: medicalRecord.allergies.map(allergy => allergy.toString()),
            description: medicalRecord.description
        } as IMedicalRecordDTO;
    }


    public static toPersistence(medicalRecord: MedicalRecord): any {
        return { 
            domainId: medicalRecord.id.toString(),
            id: medicalRecord.id.toString(),
            medicalRecordNumber: medicalRecord.props.medicalRecordNumber.value,
            medicalConditions: medicalRecord.medicalConditions,
            allergies: medicalRecord.allergies,
            description: medicalRecord.description
        }
    }

    public static toDomain(medicalRecord: any | Model<IMedicalRecordPersistence & Document>): MedicalRecord {
        const medicalRecordOrError = MedicalRecord.create(
            medicalRecord
        );

        medicalRecordOrError.isFailure ? console.log(medicalRecordOrError.error) : '';

        return medicalRecordOrError.isSuccess ? medicalRecordOrError.getValue() : null;
    }
}