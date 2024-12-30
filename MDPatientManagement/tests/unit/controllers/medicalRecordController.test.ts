import "reflect-metadata";
import { describe, it, beforeEach, afterEach } from "mocha";
import sinon from "sinon";
import { Container } from "typedi";
import { Result } from "../../../src/core/logic/Result";
import MedicalRecordController from "../../../src/controllers/MedicalRecordController";
import IMedicalRecordService from "../../../src/services/IServices/IMedicalRecordService";
import { IMedicalRecordDTO } from "../../../src/dto/IMedicalRecordDTO";
import { Request, Response, NextFunction } from "express";

describe("MedicalRecordController Unit Tests", function () {
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

  const mockMedicalRecordRepo = {
    getAll: sandbox.stub().resolves([]),
  };
  Container.set("MedicalRecordRepo", mockMedicalRecordRepo);

    let medicalRecordServiceClass = require("../../../src/services/medicalRecordService").default;
    let medicalRecordServiceInstance = Container.get(medicalRecordServiceClass);
    Container.set("MedicalRecordService", medicalRecordServiceInstance);
  });

  afterEach(() => {
    sandbox.restore();
    sinon.restore();
  });

  it("should return all medical records successfully", async () => {
    const mockRecords: IMedicalRecordDTO[] = [
      {
        id: "1234", medicalRecordNumber: "MR-001", medicalConditions: ["Condition1"], allergies: ["Allergy1"], description: "Description1"
      },
    ];

    const req: Partial<Request> = {};
    const res: Partial<Response> = {
      status: sinon.stub().returnsThis(),
      json: sinon.stub(),
    };
    const next: Partial<NextFunction> = sinon.stub();

    const service = Container.get<IMedicalRecordService>("MedicalRecordService") as IMedicalRecordService;
    sinon.stub(service, "getAllMedicalRecords").resolves(Result.ok(mockRecords));

    const controller = new MedicalRecordController(service);

    // Act
    await controller.getAllMedicalRecords(req as Request, res as Response, next as NextFunction);

    // Assert
    sinon.assert.calledWith(res.json as sinon.SinonStub, mockRecords);
  });

  it("should return 402 if no medical records are found", async () => {
    const req: Partial<Request> = {};
    const res: Partial<Response> = {
      status: sinon.stub().returnsThis(),
      send: sinon.stub(),
    };
    const next: Partial<NextFunction> = sinon.stub();

    const service = Container.get<IMedicalRecordService>("MedicalRecordService") as IMedicalRecordService;
    sinon.stub(service, "getAllMedicalRecords").resolves(Result.fail("No records found"));

    const controller = new MedicalRecordController(service);

    // Act
    await controller.getAllMedicalRecords(req as Request, res as Response, next as NextFunction);

    // Assert
    sinon.assert.calledWith(res.status as sinon.SinonStub, 402);
    sinon.assert.calledOnce(res.send as sinon.SinonStub);
  });

  it("should handle unexpected errors in getAllMedicalRecords", async () => {
    const req: Partial<Request> = {};
    const res: Partial<Response> = {};
    const next: Partial<NextFunction> = sinon.stub();

    const service = Container.get<IMedicalRecordService>("MedicalRecordService") as IMedicalRecordService;
    sinon.stub(service, "getAllMedicalRecords").throws(new Error("Unexpected error"));

    const controller = new MedicalRecordController(service);

    // Act
    await controller.getAllMedicalRecords(req as Request, res as Response, next as NextFunction);

    // Assert
    sinon.assert.calledOnce(next as sinon.SinonStub);
  });
});
