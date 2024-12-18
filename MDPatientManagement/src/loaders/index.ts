import expressLoader from './express';
import dependencyInjectorLoader from './dependencyInjector';
import mongooseLoader from './mongoose';
import Logger from './logger';

import config from '../../config';

export default async ({ expressApp }) => {
  const mongoConnection = await mongooseLoader();
  Logger.info('✌️ DB loaded and connected!');

  const userSchema = {
    // compare with the approach followed in repos and services
    name: 'userSchema',
    schema: '../persistence/schemas/userSchema',
  };

  const roleSchema = {
    // compare with the approach followed in repos and services
    name: 'roleSchema',
    schema: '../persistence/schemas/roleSchema',
  };

  const medicalConditionSchema = {
    // compare with the approach followed in repos and services
    name: 'medicalConditionSchema',
    schema: '../persistence/schemas/medicalConditionSchema',
  };

  const allergySchema = {
    // compare with the approach followed in repos and services
    name: 'allergySchema',
    schema: '../persistence/schemas/allergySchema',
  };

  const medicalRecordSchema = {
    // compare with the approach followed in repos and services
    name: 'medicalRecordSchema',
    schema: '../persistence/schemas/medicalRecordSchema',
  }

  const allergyController = {
    name: config.controllers.allergy.name,
    path: config.controllers.allergy.path
  }

  const roleController = {
    name: config.controllers.role.name,
    path: config.controllers.role.path
  }


  const roleRepo = {
    name: config.repos.role.name,
    path: config.repos.role.path
  }

  const userRepo = {
    name: config.repos.user.name,
    path: config.repos.user.path
  }

  const allergyRepo = {
    name: config.repos.allergy.name,
    path: config.repos.allergy.path
  }

  const allergyService = {
    name: config.services.allergy.name,
    path: config.services.allergy.path
  }

  const roleService = {
    name: config.services.role.name,
    path: config.services.role.path
  }

  const medicalConditionController = {
    name: config.controllers.medicalCondition.name,
    path: config.controllers.medicalCondition.path
  }

  const medicalConditionService = {
    name: config.services.medicalCondition.name,
    path: config.services.medicalCondition.path
  }

  const medicalConditionRepo = {
    name: config.repos.medicalCondition.name,
    path: config.repos.medicalCondition.path
  }

  const MedicalRecordController = {
    name: config.controllers.medicalRecord.name,
    path: config.controllers.medicalRecord.path
  }

  const MedicalRecordService = {
    name: config.controllers.medicalRecord.name,
    path: config.controllers.medicalRecord.path
  }

  const MedicalRecordRepo = {
    name: config.controllers.medicalRecord.name,
    path: config.controllers.medicalRecord.path
  }



  await dependencyInjectorLoader({
    mongoConnection,
    schemas: [
      userSchema,
      roleSchema,
      allergySchema,
      roleSchema,
      medicalConditionSchema,
      medicalRecordSchema
    ],
    controllers: [
      roleController,
      allergyController,
      roleController,
      medicalConditionController,
      MedicalRecordController
    ],
    repos: [
      roleRepo,
      userRepo,
      allergyRepo,
      userRepo,
      medicalConditionRepo,
      MedicalRecordRepo
    ],
    services: [
      roleService,
      allergyService,
      roleService,
      medicalConditionService,
      MedicalRecordService
    ]
  });
  Logger.info('✌️ Schemas, Controllers, Repositories, Services, etc. loaded');

  await expressLoader({ app: expressApp });
  Logger.info('✌️ Express loaded');
};
