import "reflect-metadata";
import { it } from 'mocha';
import sinon from "sinon";
import { Container } from "typedi";
import { Response, Request, NextFunction } from 'express';
import MedicalRecordController from "../../src/controllers/MedicalRecordController";
import IMedicalRecordService from "../../src/services/IServices/IMedicalRecordService";
import { IMedicalRecordDTO } from "../../src/dto/IMedicalRecordDTO";
import { MedicalRecordMap } from "../../src/mappers/MedicalRecordMap";

describe("MedicalRecord Controller + Service Integration Tests", function () {
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

        let medicalRecordServiceClass = require("../../src/services/medicalRecordService").default;
        let medicalRecordServiceInstance = Container.get(medicalRecordServiceClass);
        Container.set("MedicalRecordService", medicalRecordServiceInstance);
    });

    afterEach(() => {
        sandbox.restore();
        sinon.restore();
    });

    it("should return all medical records successfully", async () => {
        const medicalRecordDTOList: IMedicalRecordDTO[] = [
            {
                id: "1234",
                medicalRecordNumber: "MR-001",
                medicalConditions: ["Condition1"],
                allergies: ["Allergy1"],
                description: "Description1"
            },
            {
                id: "1235",
                medicalRecordNumber: "MR-002",
                medicalConditions: ["Condition2"],
                allergies: ["Allergy2"],
                description: "Description2"
            }
        ];

        const req: Partial<Request> = {};
        const res: Partial<Response> = {
            status: sinon.stub().returnsThis(),
            json: sinon.spy(),
        };
        const next: Partial<NextFunction> = sinon.spy();

        const medicalRecordServiceInstance = Container.get("MedicalRecordService") as IMedicalRecordService;
        sinon.stub(medicalRecordServiceInstance, "getAllMedicalRecords").resolves({
            isFailure: false,
            getValue: () => medicalRecordDTOList
        });

        sinon.stub(MedicalRecordMap, "toDTO").callsFake((record) => record);

        const ctrl = new MedicalRecordController(medicalRecordServiceInstance);

        // Act
        await ctrl.getAllMedicalRecords(<Request>req, <Response>res, <NextFunction>next);

        // Assert
        sinon.assert.calledOnce(res.json);
        sinon.assert.calledWith(res.json, sinon.match(medicalRecordDTOList));
    });

    it("should handle error when no medical records are found", async () => {
        const req: Partial<Request> = {};
        const res: Partial<Response> = {
            status: sinon.stub().returnsThis(),
            send: sinon.spy(),
        };
        const next: Partial<NextFunction> = sinon.spy();

        const medicalRecordServiceInstance = Container.get("MedicalRecordService") as IMedicalRecordService;
        sinon.stub(medicalRecordServiceInstance, "getAllMedicalRecords").resolves({
            isFailure: true,
            getValue: () => []
        });

        const ctrl = new MedicalRecordController(medicalRecordServiceInstance);

        // Act
        await ctrl.getAllMedicalRecords(<Request>req, <Response>res, <NextFunction>next);

        // Assert
        sinon.assert.calledOnce(res.status as sinon.SinonStub);
        sinon.assert.calledWith(res.status as sinon.SinonStub, 402);
        sinon.assert.calledOnce(res.send);
    });
});
