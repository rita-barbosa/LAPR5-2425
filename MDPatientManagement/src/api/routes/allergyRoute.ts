import { Router } from 'express';
import { celebrate, Joi } from 'celebrate';

import { Container } from 'typedi';
import IAllergyController from '../../controllers/IControllers/IAllergyController';

import config from "../../../config";

const route = Router();

export default (app: Router) => {
  app.use('/Allergy', route);

  const ctrl = Container.get(config.controllers.allergy.name) as IAllergyController;

  route.post('/create-allergy',
    celebrate({
      body: Joi.object({
        code : Joi.string().required(),
        designation : Joi.string().required(),
        description : Joi.string()
      })
    }),
    (req, res, next) => ctrl.createAllergy(req, res, next) );

  route.patch('/update-allergy',
    celebrate({
      body: Joi.object({
        code: Joi.string().required(),
        designation : Joi.string().required(),
        description: Joi.string()
      }),
    }),
    (req, res, next) => ctrl.updateAllergy(req, res, next) );

    route.get('/get-all-allergies',
      (req, res, next) => ctrl.getAllAllergies(req, res, next) );

    route.post('/get-allergies-filtered',
      (req, res, next) => ctrl.getAllergiesByFilter(req, res, next) );

    route.get('/get-allergy-by-code',
      celebrate({
        body: Joi.object({
          code: Joi.string().required()
        }),
      }),
      (req, res, next) => ctrl.getAllergyByCode(req, res, next) );
};