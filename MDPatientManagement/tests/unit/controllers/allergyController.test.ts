import "reflect-metadata";
import { it } from 'mocha';
import sinon from "sinon";
import { Container } from "typedi";
import { IAllergyDTO } from "../../../src/dto/IAllergyDTO";
import AllergyController from "../../../src/controllers/allergyController";
import { IAllergyQueryFilterParametersDTO } from "../../../src/dto/IAllergyQueryFilterParametersDTO";
import { Response, Request, NextFunction } from 'express';
import IAllergyService from "../../../src/services/IServices/IAllergyService";
import { Result } from "../../../src/core/logic/Result";

describe("AllergyController Unit Tests", function () {
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

        let allergySchemaInstance = require("../../../src/persistence/schemas/allergySchema").default;
        Container.set("allergySchema", allergySchemaInstance);

        let allergyRepoClass = require("../../../src/repos/allergyRepo").default;
        let allergyRepoInstance = Container.get(allergyRepoClass);
        Container.set("AllergyRepo", allergyRepoInstance);

        let allergyServiceClass = require("../../../src/services/allergyService").default;
        let allergyServiceInstance = Container.get(allergyServiceClass);
        Container.set("AllergyService", allergyServiceInstance);
    });

    afterEach(() => {
        sandbox.restore();
        sinon.restore();
    });
    it("should return allergies when found", async function () {
        const filter: IAllergyQueryFilterParametersDTO = {
            queryfilters: [
                {
                    code: 'BZ02.2', // Optional: provide the allergy code if needed
                }
            ]
        };
        const req: Partial<Request> = {
            body: filter,
        };
        const res: Partial<Response> = {
            json: sinon.spy(),
            status: sinon.stub().returnsThis() // Mock `status` method and chain it
        };
        const next: Partial<NextFunction> = sinon.spy();
    
        const allergiesDto: IAllergyDTO[] = [
            {
                code: 'BZ02.2',
                designation: 'AllergyDesignation1',
                description: 'AllergyDescription1',
            }
        ];
    
        const allergyServiceInstance = Container.get("AllergyService") as IAllergyService;
        sinon.stub(allergyServiceInstance, "getAllergiesByFilters").returns(Result.ok<IAllergyDTO[]>(allergiesDto));
    
        const ctrl = new AllergyController(allergyServiceInstance as IAllergyService);
    
        await ctrl.getAllergiesByFilter(<Request>req, <Response>res, <NextFunction>next);
    
        // Assert that `res.json` is called with the correct allergies
        sinon.assert.calledOnce(res.json as sinon.SinonSpy);
        sinon.assert.calledWith(res.json as sinon.SinonSpy, allergiesDto);
    });
    
});
