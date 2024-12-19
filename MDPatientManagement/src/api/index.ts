import { Router } from 'express';
import auth from './routes/userRoute';
import user from './routes/userRoute';
import role from './routes/roleRoute';
import medicalCondition from './routes/medicalConditionRoute';
import allergy from './routes/allergyRoute';
import medicalRecord from './routes/medicalRecordRoute';

export default () => {
	const app = Router();

	medicalCondition(app);
	auth(app);
	user(app);
	role(app);
	allergy(app);
	medicalRecord(app);

	return app
}

