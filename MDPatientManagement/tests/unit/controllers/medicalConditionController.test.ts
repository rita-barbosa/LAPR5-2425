import "reflect-metadata";
import { it } from 'mocha';
import sinon from "sinon";
import { Container } from "typedi";
import IMedicalConditionDTO from "../../../src/dto/IMedicalConditionDTO";
import MedicalConditionController from "../../../src/controllers/medicalConditionController";
import IMedicalConditionService from "../../../src/services/IServices/IMedicalConditionService";
import { Result } from "../../../src/core/logic/Result";
import { Response, Request, NextFunction } from 'express';

describe("MedicalConditionController Unit Tests", function () {
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

        // let medicalConditionSchemaInstance = require("../../../src/persistence/schemas/medicalConditionSchema").default;
        // Container.set("medicalConditionSchema", medicalConditionSchemaInstance);

        // let medicalConditionRepoClass = require("../../../src/repos/medicalConditionRepo").default;
        // let medicalConditionRepoInstance = Container.get(medicalConditionRepoClass);
        // Container.set("MedicalConditionRepo", medicalConditionRepoInstance);

        let medicalConditionServiceClass = require("../../../src/services/medicalConditionService").default;
        let medicalConditionServiceInstance = Container.get(medicalConditionServiceClass);
        Container.set("MedicalConditionService", medicalConditionServiceInstance);
    });

    afterEach(() => {
        sandbox.restore();
        sinon.restore();
    });

    it("should create a medical condition successfully with medicalConditionService stub", async () => {
        const medicalConditionDTO: IMedicalConditionDTO = {
            id: 'FB70.0',
            designation: 'valid designation',
            description: 'valid description',
            symptoms: ['Symptom 1', 'Symptom 2']
        };

        let body = medicalConditionDTO;
        let req: Partial<Request> = {};
        req.body = body;
        let res: Partial<Response> = {
            json: sinon.spy()
        };
        let next: Partial<NextFunction> = () => { };

        let medicalConditionServiceInstance = Container.get("MedicalConditionService") as IMedicalConditionService;
        sinon.stub(medicalConditionServiceInstance, "createMedicalCondition").returns(Result.ok<IMedicalConditionDTO>(medicalConditionDTO));

        const ctrl = new MedicalConditionController(medicalConditionServiceInstance as IMedicalConditionService);
        // Act
        await ctrl.createMedicalCondition(<Request>req, <Response>res, <NextFunction>next);

        // Assert
        sinon.assert.calledOnce(res.json);
        sinon.assert.calledWith(res.json, sinon.match(medicalConditionDTO));
    });
});
