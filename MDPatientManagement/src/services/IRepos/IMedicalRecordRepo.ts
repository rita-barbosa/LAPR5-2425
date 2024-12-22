import { Repo } from "../../core/infra/Repo";
import { Result } from "../../core/logic/Result";
import { MedicalRecord } from "../../domain/medicalRecord";
import { MedicalRecordId } from "../../domain/medicalRecordId";
import { IMedicalRecordQueryFilterParameters } from "../../dto/IMedicalRecordQueryFilterParameters";

export default interface IMedicalRecordRepo extends Repo<MedicalRecord> {
    findAllByParameters(filters: IMedicalRecordQueryFilterParameters): Promise<MedicalRecord[]>;
    save(medicalRecord: MedicalRecord): Promise<MedicalRecord>;
    findByDomainId (medicalRecordId: MedicalRecordId | string): Promise<MedicalRecord>;
    findAll() : Promise<MedicalRecord[]>
}