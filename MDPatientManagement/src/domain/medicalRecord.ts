import { AggregateRoot } from "../core/domain/AggregateRoot";
import { UniqueEntityID } from "../core/domain/UniqueEntityID";
import { Result } from "../core/logic/Result";
import { IMedicalRecordDTO } from "../dto/IMedicalRecordDTO";
import { AllergyCode } from "./allergyCode";
import { MedicalConditionId } from "./medicalConditionId";
import { MedicalRecordId } from "./medicalRecordId";
import { MedicalRecordNumber } from "./medicalRecordNumber";

interface MedicalRecordProps {
    medicalRecordNumber: MedicalRecordNumber;
    medicalConditions?: MedicalConditionId[];
    allergies?: AllergyCode[];
    description?: string;
}

//adicionar medical record ao patient no backoffice

//mandar um token quando passo deste para o backoffice, mandar o mesmo que se recebe inicialmente no pedido


//colocar cena do auth em todos os requests do novo modulo
export class MedicalRecord extends AggregateRoot<MedicalRecordProps> {

    get medicalRecordId(): MedicalRecordId {
        return this.id;
    }

    get patientMedicalRecordNumber(): MedicalRecordNumber {
        return this.props.medicalRecordNumber;
    }

    set medicalRecordNumber(value: MedicalRecordNumber) {
        this.props.medicalRecordNumber = value;
    }

    get description(): string {
        return this.props.description;
    }

    set description(value: string) {
        this.props.description = value;
    }

    get medicalConditions(): MedicalConditionId[] {
        return this.props.medicalConditions;
    }

    set medicalConditions(values: MedicalConditionId[]) {
        this.props.medicalConditions = values;
    }


    get allergies(): AllergyCode[] {
        return this.props.allergies;
    }

    set allergies(values: AllergyCode[]) {
        this.props.allergies = values;
    }

    public static create(medicalRecordDTO: IMedicalRecordDTO): Result<MedicalRecord> {
        const idDto = medicalRecordDTO.id;
        const medicalRecordNumberDto = medicalRecordDTO.medicalRecordNumber;
        const medicalConditionsDto = medicalRecordDTO.medicalConditions;
        const allergiesDto = medicalRecordDTO.allergies;
        const descriptionDto = medicalRecordDTO.description;

        if (!!idDto === false || idDto.length === 0) {
            return Result.fail<MedicalRecord>('Must provide an id.')
        } else {
            const medicalRecordNumberResult = MedicalRecordNumber.create({
                medicalRecordNumber: medicalRecordNumberDto,
            });

            if (medicalRecordNumberResult.isFailure) {
                return Result.fail<MedicalRecord>(medicalRecordNumberResult.error);
            }

            const medicalRecordNumberObject = medicalRecordNumberResult.getValue();

            const medicalConditionObjects = medicalConditionsDto.map(cond => new MedicalConditionId(cond));
            const allergyObjects = allergiesDto.map(allergy => new AllergyCode(allergy));
            const medicalRecord = new MedicalRecord({ medicalRecordNumber: medicalRecordNumberObject, medicalConditions: medicalConditionObjects, allergies: allergyObjects, description: descriptionDto }, new MedicalRecordId(idDto));
            return Result.ok<MedicalRecord>(medicalRecord)
        }
    }

}