import { Component, ViewChild } from '@angular/core';
import { NgForm } from '@angular/forms';
import { SideBarAdminComponent } from "../sidebar-admin/side-bar-admin.component";
import { PatientService } from '../../../services/patient.service';
import { MessageComponent } from '../../message/message.component';
import { FormsModule } from '@angular/forms';
import { CommonModule } from '@angular/common';

@Component({
  selector: 'app-create-patient-profile',
  standalone: true,
  imports: [SideBarAdminComponent, MessageComponent, FormsModule, CommonModule],
  templateUrl: './create-patient-profile.component.html',
  styleUrls: ['./create-patient-profile.component.css']
})
export class CreatePatientProfileComponent {
  @ViewChild('patientForm') patientForm!: NgForm;

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

  onSubmit(form: NgForm): void {
    this.isSubmitted = true;
    if (form.valid) {
      this.service.createPatientProfile(
        this.patient.firstName,
        this.patient.lastName,
        this.patient.phone,
        this.patient.email,
        this.patient.address,
        this.patient.emergencyContact,
        this.patient.gender,
        this.patient.dateBirth
      );
    } else {
      this.isSubmitted = false;
    }
  }

  clearForm(): void {
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
    if (this.patientForm) {
      this.patientForm.resetForm();
    }
    const inputs = document.querySelectorAll('.input-field input');
    inputs.forEach(input => {
      input.classList.remove('invalid-placeholder');
    });
  }
}

