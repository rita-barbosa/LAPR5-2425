import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { MenuComponent } from './components/menu/menu.component';
import { LoginComponent } from './components/login/login.component';
import { AdminComponent } from './components/admin/admin.component';
import { CubeComponent } from './components/cube/cube.component';
import { CursoAngularComponent } from './components/curso-angular/curso-angular.component';
import { CursoDetailComponent } from './components/curso-detail/curso-detail.component';
import { TestModuleComponent } from './modules/test-module/test-module.component';
import { StaffComponent } from './components/staff/staff.component';
import { PatientComponent } from './components/patient/patient.component';
import { CreatePatientProfileComponent } from './components/admin/create-patient-profile/create-patient-profile.component';
import { CreateStaffProfileComponent } from './components/admin/create-staff-profile/create-staff-profile.component';
import { UpdateProfileComponent } from './components/patient/update-profile/update-profile.component';
import { DoctorComponent } from './components/staff/doctor/doctor.component';
import { ListOperationRequestComponent } from './components/staff/doctor/list-operation-request/list-operation-request.component';
// import { EditPatientProfileComponent } from './components/admin/edit-patient-profile/edit-patient-profile.component';
import { CreateOperationRequestComponent } from './components/staff/doctor/create-operation-request/create-operation-request.component';
import { HospitalSimulationComponent } from './hospital-simulation/hospital-simulation.component';
import { CreateOperationTypeComponent } from './components/admin/create-operation-type/create-operation-type.component';
import { RegistrationComponent } from './user/registration/registration.component';
import { ActivationComponent } from './user/verify/verify.component';
import { ListOperationTypeComponent } from './components/admin/list-operation-type/list-operation-type.component';
import { ListStaffProfiles } from './components/admin/list-staff-profiles/list-staff-profiles.component';


export const routes: Routes = [
  { path: '', component: MenuComponent, pathMatch: 'full' },
  { path: 'login', component: LoginComponent },
  { path: 'menu', component: MenuComponent },
  { path: 'admin', component: AdminComponent },
  { path: 'create-patient-profile', component: CreatePatientProfileComponent },
  { path: 'create-staff-profile', component: CreateStaffProfileComponent },
  { path: 'staff', component: StaffComponent },
  { path: 'user/registration', component: RegistrationComponent},
  { path: 'user/verify', component: ActivationComponent},
  { path: 'doctor', component: DoctorComponent },
  { path: 'list-operation-request', component: ListOperationRequestComponent },
  { path: 'patient', component: PatientComponent },
  { path: 'update-patient-profile', component: UpdateProfileComponent },
  // { path: 'edit-patient-profile', component: EditPatientProfileComponent },
  { path: 'create-operation-request', component: CreateOperationRequestComponent },
  { path: 'list-operation-type', component: ListOperationTypeComponent},
  { path: 'cube', component: CubeComponent },
  { path: 'curso-angular', component: CursoAngularComponent },
  { path: 'curso-detail', component: CursoDetailComponent },
  { path: 'hospital-simulation', component: HospitalSimulationComponent },
  { path: 'create-operation-type', component: CreateOperationTypeComponent },
  { path: 'list-staff-profiles', component: ListStaffProfiles },
  { path: '**', redirectTo: '' }
];

@NgModule({
  imports: [RouterModule.forRoot(routes)],  // Use HttpClientModule here
  exports: [RouterModule],
})
export class AppRoutingModule { }
