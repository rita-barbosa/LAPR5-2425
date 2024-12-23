import { UniqueEntityID } from "../core/domain/UniqueEntityID";

export class MedicalConditionId extends UniqueEntityID {
    public constructor (id: string) {
        super(id);
      }

      public get id(): string {
        return this.id.toString();
    }
}