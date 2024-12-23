import { ComponentFixture, TestBed } from '@angular/core/testing';
import { PatientService } from 'src/app/services/patient.service';
import { PatientWithId } from 'src/app/domain/patient-with-id';
import { SideBarDoctorComponent } from '../sidebar-doctor/side-bar-doctor.component';
import { CommonModule } from '@angular/common';
import { TableModule } from 'primeng/table';
import { FormsModule, NgForm } from '@angular/forms';
import { MessageComponent } from 'src/app/components/message/message.component';
import { ListPatientProfilesWithMedicalRecord } from './list-patient-profiles-with-medical-records.component';
import { ActivatedRoute } from '@angular/router';
import { of } from 'rxjs';

describe('ListPatientProfilesWithMedicalRecord', () => {
  let component: ListPatientProfilesWithMedicalRecord;
  let fixture: ComponentFixture<ListPatientProfilesWithMedicalRecord>;
  let patientService: PatientService;



});
