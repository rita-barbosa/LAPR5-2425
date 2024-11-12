import { Component } from '@angular/core';
import { RouterOutlet } from '@angular/router';
import { RouterLink } from '@angular/router';
// import { LoginComponent } from './components/login/login.component';
// import { CubeComponent } from './components/cube/cube.component';
// import { CursoAngularComponent } from './components/curso-angular/curso-angular.component';
// import { MenuComponent } from "./components/menu/menu.component";

@Component({
  selector: 'app-root',
  standalone: true,
  // imports: [RouterLink, RouterOutlet, CubeComponent, CursoAngularComponent, MenuComponent],
  imports: [RouterOutlet, RouterLink],
  templateUrl: './app.component.html',
  styleUrl: './app.component.css'
})
export class AppComponent {
}
