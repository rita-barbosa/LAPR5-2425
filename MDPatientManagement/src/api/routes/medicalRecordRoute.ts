import { Router } from "express";
import IMedicalRecordController from "../../controllers/IControllers/IMedicalRecordController";
import config from "../../../config";
import { Container } from 'typedi';
import { celebrate, Joi } from "celebrate";


const route = Router();

export default (app: Router) => {
    app.use('/medicalRecord', route);

    const ctrl = Container.get(config.controllers.medicalRecord.name) as IMedicalRecordController;

    route.post('/create',
        celebrate({
            body: Joi.object({
                id: Joi.string().required(),
                medicalRecordNumber: Joi.string().required(),
                medicalConditions: Joi.array(),
                allergies: Joi.array(),
                description: Joi.string(),
            })
        }),
        (req, res, next) => ctrl.createMedicalRecord(req, res, next));

    route.patch('/update',
        celebrate({
            body: Joi.object({
                id: Joi.string().required(),
                medicalRecordNumber: Joi.string().required(),
                medicalConditions: Joi.array(),
                allergies: Joi.array(),
                description: Joi.string(),
            })
        }),
        (req, res, next) => ctrl.updateMedicalRecord(req, res, next));
    

};