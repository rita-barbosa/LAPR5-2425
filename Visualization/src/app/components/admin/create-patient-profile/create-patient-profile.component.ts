import { Component } from '@angular/core';
import { SideBarAdminComponent } from "../../side-bar-admin/side-bar-admin.component";
import { PatientService } from '../../../services/patient.service';
import { MessageModule } from '../../../modules/message-module/message-module.module';
import { MessageComponent } from '../../message/message.component';
import { HttpClientModule } from '@angular/common/http';

@Component({
  selector: 'app-create-patient-profile',
  standalone: true,
  imports: [SideBarAdminComponent,MessageComponent,HttpClientModule],
  templateUrl: './create-patient-profile.component.html',
  styleUrl: './create-patient-profile.component.css'
})
export class CreatePatientProfileComponent {
  isSubmitted = false;
  constructor(private service: PatientService){}
  create(firstName:string,lastName:string,phone:string,email:string,address:string,emergencyContact:string,gender:string,dateBirth:string): void{
   this.service.CreatePatientProfile(firstName,lastName,phone,email,address,emergencyContact,gender,dateBirth);
   this.isSubmitted=true;
  }

  clearForm() {
    // Reset form fields and allow editing again
    this.isSubmitted = false;
    // Optionally, clear input fields as well (if needed)
    document.querySelectorAll('input').forEach((input: HTMLInputElement) => {
      input.value = '';
    });
  }

}

