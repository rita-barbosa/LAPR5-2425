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


 export const routes: Routes = [
  { path: '', component: MenuComponent, pathMatch: 'full' },
  { path: 'login', component: LoginComponent},
  { path: 'menu', component: MenuComponent },
  { path: 'admin', component: AdminComponent},
  { path: 'staff', component: StaffComponent},
  { path: 'patient', component: PatientComponent},
 // Redirect to the cube component on app load
//  { path: '', redirectTo: '/cube', pathMatch: 'full' },
 // Route for the cube component
 { path: 'cube', component: CubeComponent },
 { path: 'curso-angular', component: CursoAngularComponent},
 { path: 'curso-detail', component: CursoDetailComponent},
//  { path: '', redirectTo: '', pathMatch: 'full' },
  { path: '**', redirectTo: '' }
 ];

 @NgModule({
    imports: [RouterModule.forRoot(routes),
      TestModuleComponent
    ],
    exports: [RouterModule],
  })
  export class AppRoutingModule { }
