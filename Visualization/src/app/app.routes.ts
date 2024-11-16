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

export const routes: Routes = [
  { path: '', component: MenuComponent, pathMatch: 'full' },
  { path: 'login', component: LoginComponent},
  { path: 'menu', component: MenuComponent },
  { path: 'admin', component: AdminComponent},
  { path: 'create-patient-profile', component: CreatePatientProfileComponent},
  { path: 'create-staff-profile', component: CreateStaffProfileComponent},
  { path: 'staff', component: StaffComponent},
  { path: 'patient', component: PatientComponent},
  { path: 'cube', component: CubeComponent },
  { path: 'curso-angular', component: CursoAngularComponent},
  { path: 'curso-detail', component: CursoDetailComponent},
  { path: '**', redirectTo: '' }
];

@NgModule({
  imports: [RouterModule.forRoot(routes)],  // Use HttpClientModule here
  exports: [RouterModule],
})
export class AppRoutingModule { }
