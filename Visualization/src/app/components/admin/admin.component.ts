import { Component } from '@angular/core';
import { MenuComponent } from '../menu/menu.component';
import { AppComponent } from '../../app.component';
import { RouterLink, Router } from '@angular/router';
import { SideBarAdminComponent } from "../side-bar-admin/side-bar-admin.component";

@Component({
  selector: 'app-admin',
  standalone: true,
  // imports: [MenuComponent, AppComponent],
  imports: [RouterLink, SideBarAdminComponent],
  templateUrl: './admin.component.html',
  styleUrl: './admin.component.css'
})
export class AdminComponent {
  // constructor(private router: Router) {}
  // goBackToMenu() {
  //   this.router.navigate(['/']);
  // }
}
