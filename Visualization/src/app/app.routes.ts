import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { MenuComponent } from './components/menu/menu.component';
import { CubeComponent } from './components/cube/cube.component';
import { CursoAngularComponent } from './components/curso-angular/curso-angular.component';
import { CursoDetailComponent } from './components/curso-detail/curso-detail.component';
import { TestModuleComponent } from './modules/test-module/test-module.component';


 export const routes: Routes = [
  { path: 'menu', component: MenuComponent },
 // Redirect to the cube component on app load
 { path: '', redirectTo: '/cube', pathMatch: 'full' },
 // Route for the cube component
 { path: 'cube', component: CubeComponent },
 { path: 'curso-angular', component: CursoAngularComponent},
 { path: 'curso-detail', component: CursoDetailComponent},
 ];

 @NgModule({
    imports: [RouterModule.forRoot(routes),
      TestModuleComponent
    ],
    exports: [RouterModule],
  })
  export class AppRoutingModule { }
