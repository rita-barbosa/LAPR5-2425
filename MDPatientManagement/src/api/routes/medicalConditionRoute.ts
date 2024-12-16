import { Router } from 'express';
import { celebrate, Joi } from 'celebrate';
import { Container } from 'typedi';
import config from "../../../config";
import IMedicalConditionController from '../../controllers/IControllers/IMedicalConditionController';

const route = Router();

export default (app: Router) => {
  app.use('/medicalCondition', route);

  const ctrl = Container.get(config.controllers.medicalCondition.name) as IMedicalConditionController;

  route.post('/create',
    celebrate({
      body: Joi.object({
        id: Joi.string().required(),
        designation: Joi.string().required(),
        description: Joi.string().required(),
        symptoms: Joi.array().required(),
      })
    }),
    (req, res, next) => ctrl.createMedicalCondition(req, res, next) );

};