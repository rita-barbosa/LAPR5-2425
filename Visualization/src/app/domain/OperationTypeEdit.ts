import { Phase } from "./Phase";
import { RequiredStaff } from "./RequiredStaff";

export interface OperationTypeEdit {
    id? : string;
    name?: string;
    estimatedDuration?: number; 
    status?: boolean;        
    requiredStaff?: RequiredStaff[]; 
    phases?: Phase[];  
}