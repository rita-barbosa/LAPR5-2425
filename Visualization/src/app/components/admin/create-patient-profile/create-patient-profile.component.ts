import { Component } from '@angular/core';
import { SideBarAdminComponent } from "../../side-bar-admin/side-bar-admin.component";
import { PatientService } from '../../../services/patient.service';
import { MessageComponent } from '../../message/message.component';
import { FormsModule } from '@angular/forms';
import { CommonModule } from '@angular/common';

@Component({
  selector: 'app-create-patient-profile',
  standalone: true,
  imports: [SideBarAdminComponent, MessageComponent,FormsModule,CommonModule],
  templateUrl: './create-patient-profile.component.html',
  styleUrls: ['./create-patient-profile.component.css']
})
export class CreatePatientProfileComponent {
  isSubmitted = false;
  patient = {
    firstName: '',
    lastName: '',
    phone: '',
    emergencyContact: '',
    email: '',
    address: '',
    gender: '',
    dateBirth: ''
  };

  constructor(private service: PatientService) { }

  onSubmit(form: any): void {
    this.isSubmitted = true;
    if (form.valid) {
      this.service.CreatePatientProfile(
        this.patient.firstName,
        this.patient.lastName,
        this.patient.phone,
        this.patient.email,
        this.patient.address,
        this.patient.emergencyContact,
        this.patient.gender,
        this.patient.dateBirth
      );
      console.log('Form is invalid');
    } else {
      console.log('Form is invalid');
      this.isSubmitted = false;
    }
  }

  clearForm() {
    this.isSubmitted = false;
    this.patient = {
      firstName: '',
      lastName: '',
      phone: '',
      emergencyContact: '',
      email: '',
      address: '',
      gender: '',
      dateBirth: ''
    };
  }
}
