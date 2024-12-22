import { Component, OnInit, ViewChild } from '@angular/core';
import { FormsModule, NgForm } from '@angular/forms';
import { CommonModule } from '@angular/common';
import { TableModule } from 'primeng/table';
import { PatientService } from 'src/app/services/patient.service';
import { PatientWithId } from 'src/app/domain/patient-with-id';
import { PatientQueryParameters } from 'src/app/domain/patient-query-parameters';
import { MessageComponent } from 'src/app/components/message/message.component';
import { SideBarDoctorComponent } from '../sidebar-doctor/side-bar-doctor.component';
import { AllergyMedicalConditionFilterParameters } from 'src/app/domain/allergy-medical-condition-filter-parameters';
import { PatientListingFilterParameters } from 'src/app/domain/patient-listing-filter-parameters';
import { MedicalRecordComplete } from 'src/app/domain/MedicalRecordComplete';
import { MedicalCondition } from 'src/app/domain/MedicalCondition';
import { Allergy } from 'src/app/domain/Allergy';
import { DropdownModule } from 'primeng/dropdown';
import { AllergyService } from 'src/app/services/allergy.service';
import { MedicalConditionService } from 'src/app/services/medical-condition.service';



@Component({
  selector: 'app-list-patient-profiles-with-medical-records',
  standalone: true,
  imports: [CommonModule, TableModule, MessageComponent, SideBarDoctorComponent, FormsModule, DropdownModule],
  providers : [MessageComponent],
  templateUrl: './list-patient-profiles-with-medical-records.component.html',
  styleUrls: ['./list-patient-profiles-with-medical-records.component.css'],
})
export class ListPatientProfilesWithMedicalRecord implements OnInit {
  @ViewChild('medicalRecordForm') medicalRecordForm!: NgForm;

  medicalRecord : MedicalRecordComplete = {
    id : '',
    medicalRecordNumber : '',
    medicalConditions : [],
    allergies: [],
    description : ''
  };

  isSubmitted = false;
  patientList: PatientWithId[] = [];
  selectedPatient!: PatientWithId;
  fullPatient!: PatientWithId;
  fullPatientMedicalRecord!: MedicalRecordComplete;
  showMedicalRecord: boolean = false;

  medicalRecords: MedicalRecordComplete[] = [];
  medicalRecordNumbers: string[] = [];

  filterParameters: AllergyMedicalConditionFilterParameters = {
    filters: [],
  };

  selectedMedicalCondition: string = '';

  currentMedicalCondition = { id: '', designation: '', description: '', symptoms: '' };
  filteredConditions: string[] = [];
  allMedicalConditionsDesignations: string[] = [];
  allAllergiesDesignations: string[] = [];

  allFullAllergies : Allergy[] = [];
  allFullMedicalConditions : MedicalCondition[] = [];


  patientListMock: PatientWithId[] = [
    {
      name: 'John Doe',
      phone: '555-123-4567',
      email: 'john.doe@example.com',
      address: '123 Elm Street, Springfield, IL',
      dateBirth: '1985-05-15',
      patientId: '1'
    },
    {
      name: 'Jane Smith',
      phone: '555-987-6543',
      email: 'jane.smith@example.com',
      address: '456 Maple Avenue, Denver, CO',
      dateBirth: '1990-03-20',
      patientId: '2'
    },
    {
      name: 'Alice Johnson',
      phone: '555-456-7890',
      email: 'alice.johnson@example.com',
      address: '789 Pine Road, Austin, TX',
      dateBirth: '1978-11-05',
      patientId: '3'
    },
    {
      name: 'Robert Brown',
      phone: '555-789-1234',
      email: 'robert.brown@example.com',
      address: '321 Oak Lane, Seattle, WA',
      dateBirth: '2000-01-12',
      patientId: '4'
    }
  ];


  medicalRecordsMock: MedicalRecordComplete[] = [
    {
      id: '1',
      medicalRecordNumber: '202412000001',
      medicalConditions: [
        { id: 'FB70.0', designation: 'Low Back Pain', description: 'Pain in the lower back region.', symptoms: 'Sharp pain, stiffness, difficulty standing.' }
      ],
      allergies: [
        { code: 'BZ02.2', designation: 'Peanut Allergy', description: 'Manifests as rashes and bloating.' }
      ],
      description: 'Patient with chronic lower back pain and peanut allergy.'
    },
    {
      id: '2',
      medicalRecordNumber: '202412000002',
      medicalConditions: [
        { id: 'FB71.0', designation: 'Asthma', description: 'Chronic respiratory condition.', symptoms: 'Shortness of breath, wheezing, coughing.' }
      ],
      allergies: [
        { code: 'BZ03.4', designation: 'Pollen Allergy', description: 'Leads to sneezing and watery eyes.' }
      ],
      description: 'Asthma patient with seasonal pollen allergy.'
    },
    {
      id: '3',
      medicalRecordNumber: '202412000003',
      medicalConditions: [
        { id: 'FB72.0', designation: 'Hypertension', description: 'High blood pressure.', symptoms: 'Headaches, shortness of breath, nosebleeds.' }
      ],
      allergies: [
        { code: 'BZ05.1', designation: 'Dust Allergy', description: 'Causes sneezing and skin irritation.' }
      ],
      description: 'Patient with high blood pressure and mild dust allergy.'
    },
    {
      id: '4',
      medicalRecordNumber: '202412000004',
      medicalConditions: [
        { id: 'FB73.0', designation: 'Diabetes', description: 'Chronic condition affecting blood sugar.', symptoms: 'Frequent urination, increased thirst, weight loss.' }
      ],
      allergies: [],
      description: 'Diabetic patient with no known allergies.'
    }
  ];


  constructor(private service: PatientService, private allergyService :AllergyService , private medicalConditionService : MedicalConditionService, private message : MessageComponent) {}

  // ngOnInit(): void {
  //   this.addFilter();
  // this.fetchAllergies();
  // this.fetchMedicalConditions();
  //   this.fetchPatients();
  //   this.fetchPatientsMedicalRecords();
  //   this.filteredConditions = [...this.allMedicalConditions];
  // }

  ngOnInit(): void {
    this.addFilter();
    this.fetchAllergies();
    this.fetchMedicalConditions();
  this.patientList = this.patientListMock;
  this.medicalRecords = this.medicalRecordsMock;
}

fetchMedicalConditions() {
  this.medicalConditionService.getAllMedicalConditions().subscribe({
    next: (data) => {
      this.allFullMedicalConditions = data;
      this.allMedicalConditionsDesignations = this.allFullMedicalConditions.map(condition => {
        return condition.designation;
      });
    },
    error: (error) => console.error('Error fetching medical conditions:', error),
  });
}


  fetchAllergies() {
    this.allergyService.getAllAllergies().subscribe({
      next: (data) =>  {
        this.allFullAllergies = data;
        this.allAllergiesDesignations = this.allFullAllergies.map(allergy => {
          return allergy.designation;
        });
      },
      error: (error) => console.error('Error fetching allergies:', error),
    });
  }


  fetchPatients(): void {
    const emptyFilter: PatientListingFilterParameters = {
      firstName: "",
      lastName: "",
      email: "",
      gender: "",
      dateBirth: "",
      medicalRecordNumber: ""
    };

    const queryParameters: PatientQueryParameters = {
      queryfilters: [emptyFilter] // Single empty filter
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


  fetchPatientsMedicalRecords(): void {
    this.service.getAllMedicalRecords().subscribe({
      next: (data) => (this.medicalRecords = data),
      error: (error) => console.error('Error fetching medical records:', error),
    });
    console.log(this.medicalRecords)
  }

  applyFilters(): void {
    this.fetchFilteredPatientMedicalRecords();
    this.fetchPatientProfilesByMedicalRecordNumber();
  }


  fetchFilteredPatientMedicalRecords(): void {
    this.service.getFilteredMedicalRecords(this.filterParameters).subscribe({
      next: (records) => {
        this.medicalRecords = records;
      },
      error: (error) => {
        console.error('Error fetching medical records:', error);
      },
    });

    this.medicalRecordNumbers = this.medicalRecords.map(record => record.medicalRecordNumber);

  }

  fetchPatientProfilesByMedicalRecordNumber(): void {
    const patientQueryParameters: PatientQueryParameters = {
      queryfilters: this.medicalRecordNumbers.map((recordNumber) => ({
        firstName: '',
        lastName: '',
        email: '',
        gender: '',
        dateBirth: '',
        medicalRecordNumber: recordNumber,
      }))
    };

    this.service.getPatientsByFilters(patientQueryParameters).subscribe({
      next: (patients) => {
        this.patientList = patients;
      },
      error: (error) => {
        console.error('Error fetching patient profiles:', error);
      },
    });

  }

  addFilter(): void {
    this.filterParameters.filters.push({ allergyDesignation: '', medicalConditionDesignation: '' });
  }

  removeFilter(index: number): void {
    this.filterParameters.filters.splice(index, 1);
  }

  toggleMedicalRecord(patient: PatientWithId): void {
        this.fullPatient = patient;
        this.fullPatientMedicalRecord = this.medicalRecords.find(record => record.id === patient.patientId)!;
        this.medicalRecord = this.fullPatientMedicalRecord;
        this.showMedicalRecord = true;
  }

  closeDetails(): void {
    this.showMedicalRecord = false;
  }

  filterConditions(): void {
    this.filteredConditions = this.allMedicalConditionsDesignations.filter((condition) =>
      condition.toLowerCase().includes(this.currentMedicalCondition.designation.toLowerCase())
    );
  }

  addMedicalCondition(inputValue: string): void {
    console.log('Input Value:', inputValue);

    if (inputValue.trim()) {
      const fullCondition = this.allFullMedicalConditions.find(
        (condition) => condition.designation.toLowerCase() === inputValue.toLowerCase()
      );

      if (fullCondition) {
        const conditionExists = this.medicalRecord.medicalConditions.some(
          (condition) => condition.designation.toLowerCase() === fullCondition.designation.toLowerCase()
        );

        if (!conditionExists) {
          this.medicalRecord.medicalConditions.push({ ...fullCondition });
        } else {
          console.warn('Condition already exists in the list.');
          this.message.messageService.add('Condition already exists in the list.')
        }
      } else {
        console.warn('Condition not found in data.');
        this.message.messageService.add('Condition not found in data.')
      }
    }
  }


  removeMedicalCondition(index: number): void {
    this.medicalRecord.medicalConditions.splice(index, 1);
  }


  removeAllergy(index: number): void {
    this.medicalRecord.allergies.splice(index, 1);
  }


  onSubmit(form: NgForm): void {
    this.isSubmitted = true;
    if (form.valid) {
      this.service.updateMedicalRecord(
        this.medicalRecord.id,
        this.medicalRecord.medicalRecordNumber,
        this.medicalRecord.medicalConditions,
        this.medicalRecord.allergies,
        this.medicalRecord.description || ''
      );
    } else {
      this.isSubmitted = false;
    }
  }

  clearForm(): void {
    this.isSubmitted = false;

    this.medicalRecord = {
      id : '',
      medicalRecordNumber : '',
      medicalConditions : [],
      allergies: [],
      description : ''
    };
    if (this.medicalRecordForm) {
      this.medicalRecordForm.resetForm();
    }
  }
}
