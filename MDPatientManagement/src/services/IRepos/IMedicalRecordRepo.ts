import { Repo } from "../../core/infra/Repo";
import { MedicalRecord } from "../../domain/medicalRecord";
import { MedicalRecordId } from "../../domain/medicalRecordId";

export default interface IMedicalREcordRepo extends Repo<MedicalRecord> {
    save(medicalRecord: MedicalRecord): Promise<MedicalRecord>;
    findByDomainId (medicalRecordId: MedicalRecordId | string): Promise<MedicalRecord>;
    findAll() : Promise<MedicalRecord[]>
}