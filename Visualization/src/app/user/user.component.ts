import { Component } from '@angular/core';
import { RegistrationComponent } from './registration/registration.component';

@Component({
  selector: 'app-user',
  standalone: true,
  imports: [RegistrationComponent],
  templateUrl: './user.component.html',
  styleUrl: './user.component.css'
})
export class UserComponent {

}
