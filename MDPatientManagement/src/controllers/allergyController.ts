import { Request, Response, NextFunction } from 'express';
import { Inject, Service } from 'typedi';
import config from "../../config";


import { Result } from "../core/logic/Result";
import { BaseController } from '../core/infra/BaseController';
import IAllergyController from './IControllers/IAllergyController';
import IAllergyService from '../services/IServices/IAllergyService';
import { IAllergyDTO } from '../dto/IAllergyDTO';
import { IAllergyUpdateDTO } from '../dto/IAllergyUpdateDTO';

@Service()
export default class AllergyController /*extends BaseController*/ implements IAllergyController  {
  
  // protected executeImpl(): Promise<void | any> {
  //   throw new Error('Method not implemented.');
  // }
  
  constructor(
      @Inject(config.services.allergy.name) private allergyServiceInstance : IAllergyService
  ) {
    //super();
  }

  public async createAllergy(req: Request, res: Response, next: NextFunction) {
    try {
      const allergyOrError = await this.allergyServiceInstance.createAllergy(req.body as IAllergyDTO) as Result<IAllergyDTO>;
    
      if (allergyOrError.isFailure) {
        return res.status(402).send();
      }

      const allergyDTO = allergyOrError.getValue();
      return res.json( allergyDTO ).status(201);
    }
    catch (e) {
      return next(e);
    }
  }


  public async updateAllergy(req: Request, res: Response, next: NextFunction) {
    try {
      const allergyOrError = await this.allergyServiceInstance.updateAllergy(req.body as IAllergyUpdateDTO) as Result<IAllergyDTO>;

      if (allergyOrError.isFailure) {
        return res.status(404).send();
      }

      const allergyDTO = allergyOrError.getValue();
      return res.status(201).json( allergyDTO );
    }
    catch (e) {
      return next(e);
    }
  };

  public async getAllergyByCode(req: Request, res: Response, next: NextFunction) {
    try {
      const allergyOrError = await this.allergyServiceInstance.getAllergyByCode(req.body as string) as Result<IAllergyDTO>;

      if (allergyOrError.isFailure) {
        return res.status(404).send();
      }

      const allergyDTO = allergyOrError.getValue();
      return res.status(201).json( allergyDTO );
    }
    catch (e) {
      return next(e);
    }
  }


  public async getAllAllergies(req: Request, res: Response, next: NextFunction) {
    try {
      const allergyOrError = await this.allergyServiceInstance.getAllAllergies() as Result<IAllergyDTO[]>;

      if (allergyOrError.isFailure) {
        return res.status(404).send();
      }

      const allergyDTO = allergyOrError.getValue();
      return res.status(201).json( allergyDTO );
    }
    catch (e) {
      return next(e);
    }
  }

}