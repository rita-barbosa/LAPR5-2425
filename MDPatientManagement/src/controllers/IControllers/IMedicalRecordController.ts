import { Request, Response, NextFunction } from 'express';

export default interface IMedicalConditionController {
    createMedicalRecord(req: Request, res: Response, next: NextFunction);
}