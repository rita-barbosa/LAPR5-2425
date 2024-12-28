import "reflect-metadata";
import { expect } from "chai";
import { it } from 'mocha';
import sinon from "sinon";
import { Container } from "typedi";
import IMedicalConditionDTO from "../../../src/dto/IMedicalConditionDTO";
import IMedicalConditionRepo from "../../../src/services/IRepos/IMedicalConditionRepo";
import MedicalConditionService from "../../../src/services/medicalConditionService";
import { MedicalCondition } from "../../../src/domain/medicalCondition";
import { MedicalConditionMap } from "../../../src/mappers/MedicalConditionMap";

describe("MedicalConditionService Unit Tests", function () {
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

    let medicalConditionSchemaInstance = require("../../../src/persistence/schemas/medicalConditionSchema").default;
    Container.set("medicalConditionSchema", medicalConditionSchemaInstance);

    let medicalConditionRepoClass = require("../../../src/repos/medicalConditionRepo").default;
    let medicalConditionRepoInstance = Container.get(medicalConditionRepoClass);
    Container.set("MedicalConditionRepo", medicalConditionRepoInstance);
  });

  afterEach(() => {
    sandbox.restore();
    sinon.restore();
  });

  it("should create a medical condition successfully", async () => {
    const medicalConditionDTO: IMedicalConditionDTO = {
      id: 'FB70.0',
      designation: 'valid designation',
      description: 'valid description',
      symptoms: ['Symptom 1', 'Symptom 2']
    };

    sinon.stub(MedicalCondition, "create").resolves({
      isFailure: false,
      getValue: () => (medicalConditionDTO),
    });

    sinon.stub(MedicalConditionMap, "toDTO").returns(medicalConditionDTO);

    let medicalConditionRepoInstance = Container.get("MedicalConditionRepo") as IMedicalConditionRepo;
    sinon.stub(medicalConditionRepoInstance, "save").resolves(medicalConditionDTO);

    const service = new MedicalConditionService(medicalConditionRepoInstance as IMedicalConditionRepo);
    // Act
    const result = await service.createMedicalCondition(medicalConditionDTO);

    // Assert
    expect(result.isSuccess).to.be.true;
    expect(result.getValue()).to.deep.equal(medicalConditionDTO);
  });

  it("should return failure if medical condition create fails", async () => {
    const medicalConditionDTO: IMedicalConditionDTO = {
      id: 'FB70.0',
      designation: 'valid designation',
      description: 'valid description',
      symptoms: ['Symptom 1', 'Symptom 2']
    };

    sinon.stub(MedicalCondition, "create").resolves({
      isFailure: true,
      errorValue: sinon.stub().returns("Creation error")
    });

    let medicalConditionRepoInstance = Container.get("MedicalConditionRepo") as IMedicalConditionRepo;
    const service = new MedicalConditionService(medicalConditionRepoInstance as IMedicalConditionRepo);
    // Act
    const result = await service.createMedicalCondition(medicalConditionDTO);

    // Assert
    expect(result.isFailure).to.be.true;
    expect(result.errorValue()).to.equal("Creation error");
  });

  it("should throw an error if save fails", async () => {
    const medicalConditionDTO: IMedicalConditionDTO = {
      id: 'FB70.0',
      designation: 'valid designation',
      description: 'valid description',
      symptoms: ['Symptom 1', 'Symptom 2']
    };

    sinon.stub(MedicalCondition, "create").resolves({
      isFailure: false,
      getValue: () => (medicalConditionDTO),
    });

    sinon.stub(MedicalConditionMap, "toDTO").returns(medicalConditionDTO);

    let medicalConditionRepoInstance = Container.get("MedicalConditionRepo") as IMedicalConditionRepo;
    sinon.stub(medicalConditionRepoInstance, "save").rejects(new Error("Save error"));

    const service = new MedicalConditionService(medicalConditionRepoInstance as IMedicalConditionRepo);
    // Act
    try {
      await service.createMedicalCondition(medicalConditionDTO);
      throw new Error("Expected method to throw, but it didn't.");
    } catch (error) {
      expect(error.message).to.equal("Save error");
    }
  });
});
