import { Component, OnInit } from '@angular/core';
import { FormsModule } from '@angular/forms';
import { CommonModule } from '@angular/common';
import { TableModule } from 'primeng/table';
import { MessageComponent } from '../../message/message.component';

import { SideBarAdminComponent } from '../sidebar-admin/side-bar-admin.component';
import { PatientService } from '../../../services/patient.service';
import { PatientWithId } from '../../../domain/patient-with-id';
import { PatientQueryParameters } from '../../../domain/patient-query-parameters';
import { PatientListingFilterParameters } from '../../../domain/patient-listing-filter-parameters';


@Component({
  selector: 'app-list-patient-profiles',
  standalone: true,
  imports: [SideBarAdminComponent, CommonModule, TableModule, FormsModule, MessageComponent],
  templateUrl: './list-patient-profiles.component.html',
  styleUrls: ['./list-patient-profiles.component.css']
})
export class ListPatientProfiles implements OnInit {
  patientList: PatientWithId[] = [];
  selectedPatient!: PatientWithId;
  fullPatient!: PatientWithId;
  detailsVisible: boolean = false;

  specializations: string[] = [];
  functions: string[] = ['Doctor', 'Intern', 'Nurse', 'Assistant'];
  queryFiltersList: PatientListingFilterParameters[] = [];

  constructor(private service: PatientService) {}

  ngOnInit(): void {
    this.addFilter();
    this.fetchPatients();
  }

  fetchPatients(): void {
    const queryParameters: PatientQueryParameters = {
      queryfilters: this.queryFiltersList
    };

    this.service.getPatientsByFilters(queryParameters).subscribe({
      next: (data) => {
        this.patientList = data;
      },
      error: (error) => {
        console.error('Error fetching patient profiles:', error);
      }
    });
  }

  applyFilters(): void {
    this.fetchPatients();
  }

  addFilter(): void {
    this.queryFiltersList.push({
      firstName : '',
      lastName : '',
      email : '',
      gender : '',
      dateBirth : '',
      medicalRecordNumber : ''
    });
  }

  removeFilter(index: number): void {
    this.queryFiltersList.splice(index, 1);
  }

  toggleDetails(patient : PatientWithId): void {
    console.log(patient);
    this.service.getPatientById(patient.patientId).subscribe({
      next: (fullPatientInfo: PatientWithId) => {
        this.fullPatient = fullPatientInfo;
        this.detailsVisible = true;
      },
      error: (error: any) => {
        console.error('Error fetching patient profile details:', error);
      }
    });
  }

  closeDetails(): void {
    this.detailsVisible = false;
  }

  editPatientProfile(patient : PatientWithId): void {
    // Implement edit logic
  }

  deletePatientProfile(patient : PatientWithId): void {
    // Implement delete logic
  }

}
