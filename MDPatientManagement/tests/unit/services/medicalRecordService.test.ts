import "reflect-metadata";
import { expect } from "chai";
import { it } from 'mocha';
import sinon from "sinon";
import { Container } from "typedi";
import { IMedicalRecordDTO } from "../../../src/dto/IMedicalRecordDTO";
import IMedicalRecordRepo from "../../../src/services/IRepos/IMedicalRecordRepo";
import MedicalRecordService from "../../../src/services/medicalRecordService";
import { MedicalRecord } from "../../../src/domain/medicalRecord";
import { MedicalRecordMap } from "../../../src/mappers/MedicalRecordMap";
import IMedicalConditionRepo from "../../../src/services/IRepos/IMedicalConditionRepo";
import IAllergyRepo from "../../../src/services/IRepos/IAllergyRepo";

describe("MedicalRecordService Unit Tests", function () {
  const sandbox = sinon.createSandbox();
  this.timeout(5000);

  beforeEach(() => {
    Container.reset();

    const mockLogger = {
      info: sandbox.stub(),
      error: sandbox.stub(),
      warn: sandbox.stub(),
      debug: sandbox.stub(),
    };
    Container.set("logger", mockLogger);

    let medicalRecordSchemaInstance = require("../../../src/persistence/schemas/medicalRecordSchema").default;
    Container.set("medicalRecordSchema", medicalRecordSchemaInstance);

    let medicalRecordRepoClass = require("../../../src/repos/medicalRecordRepo").default;
    let medicalRecordRepoInstance = Container.get(medicalRecordRepoClass);
    Container.set("MedicalRecordRepo", medicalRecordRepoInstance);
  });

  afterEach(() => {
    sandbox.restore();
    sinon.restore();
  });

  it("should fetch all medical records successfully", async () => {
    const medicalRecords = [
      {
        id: "1234", medicalRecordNumber: "MR-001", medicalConditions: ["Condition1"], allergies: ["Allergy1"], description: "Description1"
      },
      {
        id: "5678", medicalRecordNumber: "MR-002", medicalConditions: ["Condition2"], allergies: ["Allergy2"], description: "Description2"
      },
    ];

    const medicalRecordDTOs: IMedicalRecordDTO[] = medicalRecords.map((record) => ({
      id: record.id,
      medicalRecordNumber: record.medicalRecordNumber,
      medicalConditions: record.medicalConditions,
      allergies: record.allergies,
      description: record.description,
    }));

    const medicalRecordRepoInstance = Container.get("MedicalRecordRepo") as IMedicalRecordRepo;
    const medicalConditionRepoInstance = Container.get("MedicalConditionRepo") as IMedicalConditionRepo;
    const allergyRepoInstance = Container.get("AllergyRepo") as IAllergyRepo;

    sinon.stub(medicalRecordRepoInstance, "findAll").resolves(medicalRecords);

    sinon.stub(MedicalRecordMap, "toDTO").callsFake((record) => ({
      id: record.id,
      medicalRecordNumber: record.medicalRecordNumber,
      medicalConditions: record.medicalConditions,
      allergies: record.allergies,
      description: record.description,
    }));

    const service = new MedicalRecordService(medicalRecordRepoInstance as IMedicalRecordRepo, medicalConditionRepoInstance as IMedicalConditionRepo, allergyRepoInstance as IAllergyRepo);

    const result = await service.getAllMedicalRecords();

    expect(result.isSuccess).to.be.true;
    expect(result.getValue()).to.deep.equal(medicalRecordDTOs);
  });

  it("should return failure if no medical records are found", async () => {
    const medicalRecordRepoInstance = Container.get("MedicalRecordRepo") as IMedicalRecordRepo;
    const medicalConditionRepoInstance = Container.get("MedicalConditionRepo") as IMedicalConditionRepo;
    const allergyRepoInstance = Container.get("AllergyRepo") as IAllergyRepo;

    sinon.stub(medicalRecordRepoInstance, "findAll").resolves([]);

    const service = new MedicalRecordService(medicalRecordRepoInstance as IMedicalRecordRepo, medicalConditionRepoInstance as IMedicalConditionRepo, allergyRepoInstance as IAllergyRepo);

    const result = await service.getAllMedicalRecords();

    expect(result.isFailure).to.be.true;
    expect(result.errorValue()).to.equal("Medical Records not found");
  });

  it("should throw an error if fetching all medical records fails", async () => {
    const medicalRecordRepoInstance = Container.get("MedicalRecordRepo") as IMedicalRecordRepo;
    const medicalConditionRepoInstance = Container.get("MedicalConditionRepo") as IMedicalConditionRepo;
    const allergyRepoInstance = Container.get("AllergyRepo") as IAllergyRepo;

    sinon.stub(medicalRecordRepoInstance, "findAll").rejects(new Error("Database error"));

    const service = new MedicalRecordService(medicalRecordRepoInstance as IMedicalRecordRepo, medicalConditionRepoInstance as IMedicalConditionRepo, allergyRepoInstance as IAllergyRepo);

    try {
      await service.getAllMedicalRecords();
      throw new Error("Expected method to throw, but it didn't.");
    } catch (error) {
      expect(error.message).to.equal("Database error");
    }
  });
});
