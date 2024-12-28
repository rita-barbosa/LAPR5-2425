import "reflect-metadata";
import { expect } from "chai";
import { it } from 'mocha';
import sinon from "sinon";
import { Container } from "typedi";
import { IAllergyDTO } from "../../../src/dto/IAllergyDTO";
import IAllergyRepo from "../../../src/services/IRepos/IAllergyRepo";
import AllergyService from "../../../src/services/allergyService";
import { AllergyMap } from "../../../src/mappers/AllergyMap";
import { IAllergyQueryFilterParametersDTO } from "../../../src/dto/IAllergyQueryFilterParametersDTO";

describe("AllergyService Unit Tests", function () {
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
    });

    afterEach(() => {
        sandbox.restore();
        sinon.restore();
    });

    it("should return allergies when found", async function () {
        const filter: IAllergyQueryFilterParametersDTO = {
            queryfilters: [
                {
                    code: 'BZ02.2',       // Optional: provide the allergy code if needed
                }
            ]
        };
        const allergies = [
            {
                code: 'BZ02.2',
                designation: 'AllergyDesignation1',
                description: 'AllergyDescription1'
            }
        ];

        const allergiesDto: IAllergyDTO[] = [
            {
                code: 'BZ02.2',
                designation: 'AllergyDesignation1',
                description: 'AllergyDescription1'
            }
        ];

        let allergyRepoInstance = Container.get("AllergyRepo") as IAllergyRepo;
        sinon.stub(allergyRepoInstance, "findAllByParameters").resolves(allergies);

        sinon.stub(AllergyMap, "toDTO").returns(allergiesDto);

        const service = new AllergyService(allergyRepoInstance as IAllergyRepo);
        const result = await service.getAllergiesByFilters(filter);

        expect(result.isSuccess).to.be.true;
        expect(result.getValue()).to.have.lengthOf(1);
    });

    it("should return a failure if no allergies are found", async function () {
        const filter: IAllergyQueryFilterParametersDTO = {
            queryfilters: [
                {
                    code: 'BZ02.2',
                }
            ]
        };
        let allergyRepoInstance = Container.get("AllergyRepo") as IAllergyRepo;
        sinon.stub(allergyRepoInstance, "findAllByParameters").resolves([]);

        const service = new AllergyService(allergyRepoInstance as IAllergyRepo);
        const result = await service.getAllergiesByFilters(filter);

        expect(result.isSuccess).to.be.false;
        expect(result.errorValue()).to.equal("Allergy not found");
    });

    it("should throw an error if there is an exception in the repository", async function () {
        const filter: IAllergyQueryFilterParametersDTO = { queryfilters: [] };
        let allergyRepoInstance = Container.get("AllergyRepo") as IAllergyRepo;
        sinon.stub(allergyRepoInstance, "findAllByParameters").rejects(new Error("Database error"));

        try {
            const service = new AllergyService(allergyRepoInstance as IAllergyRepo);
            await service.getAllergiesByFilters(filter);
            expect.fail("Expected error to be thrown");
        } catch (error) {
            expect(error.message).to.equal("Failed to fetch allergy: Database error");
        }
    });

});
