export interface IMedicalRecordDTO {
    id: string; //ver aqui como fazer, talvez melhor remover isto
    medicalRecordNumber: string;
    medicalConditions?: string[];
    allergies?: string[];
    description?: string;
}