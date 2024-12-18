import { Router } from 'express';
import { celebrate, Joi } from 'celebrate';

import { Container } from 'typedi';
import IRoleController from '../../controllers/IControllers/IRoleController';

import config from "../../../config";
import middlewares from '../middlewares';
import isAuth from '../middlewares/isAuth';
import isAuthz from '../middlewares/isAuthz';

const route = Router();

export default (app: Router) => {
  app.use('/roles', route);

  app.use(function (err, req, res, next) {  /// YOU GUYS NEED TO ADD THIS
    if (err.name === "UnauthorizedError") {
      res.status(401).send("Invalid or missing token.");
    } else {
      next(err);
    }
  });

  const ctrl = Container.get(config.controllers.role.name) as IRoleController;

  route.post('',
    middlewares.isAuth,                /// YOU GUYS NEED TO ADD THIS
    middlewares.isAuthz(["Admin"]),    /// YOU GUYS NEED TO ADD THIS
    celebrate({
      body: Joi.object({
        name: Joi.string().required()
      })
    }),
    (req, res, next) => ctrl.createRole(req, res, next) );

  route.put('',
    celebrate({
      body: Joi.object({
        id: Joi.string().required(),
        name: Joi.string().required()
      }),
    }),
    (req, res, next) => ctrl.updateRole(req, res, next) );
};
