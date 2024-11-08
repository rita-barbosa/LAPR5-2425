import { Component } from '@angular/core';
import { RouterOutlet } from '@angular/router';
import { CursoAngularComponent } from './components/curso-angular/curso-angular.component';
import { MenuComponent } from "./components/menu/menu.component";

@Component({
  selector: 'app-root',
  standalone: true,
  imports: [RouterOutlet, CursoAngularComponent, MenuComponent],
  templateUrl: './app.component.html',
  styleUrl: './app.component.css'
})
export class AppComponent {
  title = 'Visualization';
  bHeight = 100;
  bWidth = 100;

  addProduct() {
    console.log('add product');
  }
}
