import { ComponentFixture, TestBed } from '@angular/core/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { ListPatientProfilesWithMedicalRecord } from './list-patient-profiles-with-medical-records.component';
import { PatientService } from 'src/app/services/patient.service';
import { AllergyService } from 'src/app/services/allergy.service';
import { MedicalConditionService } from 'src/app/services/medical-condition.service';
import { of } from 'rxjs';
import { FormsModule } from '@angular/forms';
import { PatientWithId } from 'src/app/domain/patient-with-id';
import { ActivatedRoute } from '@angular/router';

describe('ListPatientProfilesWithMedicalRecord', () => {
  let component: ListPatientProfilesWithMedicalRecord;
  let fixture: ComponentFixture<ListPatientProfilesWithMedicalRecord>;
  let mockPatientService: jasmine.SpyObj<PatientService>;
  let mockAllergyService: jasmine.SpyObj<AllergyService>;
  let mockMedicalConditionService: jasmine.SpyObj<MedicalConditionService>;

  beforeEach(async () => {
    mockPatientService = jasmine.createSpyObj('PatientService', [
      'getPatientsByFilters',
      'getAllMedicalRecords',
      'getFilteredMedicalRecords',
      'updateMedicalRecord',
    ]);
    mockAllergyService = jasmine.createSpyObj('AllergyService', ['getAllAllergies']);
    mockMedicalConditionService = jasmine.createSpyObj('MedicalConditionService', ['getAllMedicalConditions']);

    const mockPatients: PatientWithId[] = [
      {
        name: 'John Doe',
        phone: '123456789',
        email: 'john.doe@example.com',
        address: '123 Main St',
        dateBirth: '1990-01-01',
        patientId: '1',
      },
    ];

    const mockMedicalRecords = [
      {
        id: '1',
        medicalRecordNumber: 'MR001',
        medicalConditions: [],
        allergies: [],
        description: 'No issues.',
      },
    ];

    const mockAllergies = [
      { code: 'A01', designation: 'Peanut Allergy', description: 'Severe allergy to peanuts' },
      {code: 'BZ05.3', designation: 'Pollen Allergy', description: 'Mucus.'},
    ];

    const mockConditions = [
      { id: 'FB70.0', designation: 'Low back pain', description: 'Pain that occurs in the lower back, commonly due to strain, injury, or degenerative changes in the spine.', symptoms: 'Sharp or Dull Back Pain","Muscle Spasms","Stiffness","Difficulty Standing or Walking' },
      { id: 'FA24.0', designation: 'Fracture of femur', description: 'A break or crack in the femur bone, typically resulting from high-energy trauma or falls.', symptoms: 'Severe Pain in the Thigh","Swelling","Inability to Bear Weight","Bruising' },
    ];

    mockPatientService.getPatientsByFilters.and.returnValue(of(mockPatients));
    mockPatientService.getAllMedicalRecords.and.returnValue(of(mockMedicalRecords));
    mockAllergyService.getAllAllergies.and.returnValue(of(mockAllergies));
    mockMedicalConditionService.getAllMedicalConditions.and.returnValue(of(mockConditions));

    const activatedRouteMock = {
      snapshot: {
        paramMap: {
          get: jasmine.createSpy('get').and.returnValue(null),
        },
      },
    };

    await TestBed.configureTestingModule({
      imports: [ListPatientProfilesWithMedicalRecord, HttpClientTestingModule, FormsModule],
      providers: [
        { provide: PatientService, useValue: mockPatientService },
        { provide: AllergyService, useValue: mockAllergyService },
        { provide: MedicalConditionService, useValue: mockMedicalConditionService },
        { provide: ActivatedRoute, useValue: activatedRouteMock },
      ],
    }).compileComponents();

    fixture = TestBed.createComponent(ListPatientProfilesWithMedicalRecord);
    component = fixture.componentInstance;
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should fetch patients on initialization', () => {
    const mockPatients: PatientWithId[] = [
      {
        name: 'John Doe',
        phone: '123456789',
        email: 'john.doe@example.com',
        address: '123 Main St',
        dateBirth: '1990-01-01',
        patientId: '202412000001',
      },
      {
        name: 'Jane Smith',
        phone: '987654321',
        email: 'jane.smith@example.com',
        address: '456 Elm St',
        dateBirth: '1985-05-15',
        patientId: '202412000002',
      },
    ];

    mockPatientService.getPatientsByFilters.and.returnValue(of(mockPatients));

    component.ngOnInit();

    expect(mockPatientService.getPatientsByFilters).toHaveBeenCalled();
    expect(component.patientList).toEqual(mockPatients);
  });

  it('should fetch medical records on initialization', () => {
    const mockMedicalRecords = [
      {
        id: '1',
        medicalRecordNumber: 'MR001',
        medicalConditions: [],
        allergies: [],
        description: 'No issues.',
      },
    ];

    mockPatientService.getAllMedicalRecords.and.returnValue(of(mockMedicalRecords));

    component.ngOnInit();

    expect(mockPatientService.getAllMedicalRecords).toHaveBeenCalled();
    expect(component.medicalRecords).toEqual(mockMedicalRecords);
  });

  it('should fetch medical conditions on initialization', () => {
    const mockConditions = [
      { id: 'FB70.0', designation: 'Low back pain', description: 'Pain that occurs in the lower back, commonly due to strain, injury, or degenerative changes in the spine.', symptoms: 'Sharp or Dull Back Pain","Muscle Spasms","Stiffness","Difficulty Standing or Walking' },
      { id: 'FA24.0', designation: 'Fracture of femur', description: 'A break or crack in the femur bone, typically resulting from high-energy trauma or falls.', symptoms: 'Severe Pain in the Thigh","Swelling","Inability to Bear Weight","Bruising' },
    ];

    mockMedicalConditionService.getAllMedicalConditions.and.returnValue(of(mockConditions));

    component.ngOnInit();

    expect(mockMedicalConditionService.getAllMedicalConditions).toHaveBeenCalled();
    expect(component.allMedicalConditionsDesignations).toEqual(['Low back pain', 'Fracture of femur']);
  });

  it('should toggle medical record visibility', () => {
    const mockPatient: PatientWithId = {
      name: 'John Doe',
      phone: '123456789',
      email: 'john.doe@example.com',
      address: '123 Main St',
      dateBirth: '1990-01-01',
      patientId: '202412000001',
    };

    const mockMedicalRecord = {
      id: '202412000001',
      medicalRecordNumber: 'MR001',
      medicalConditions: [],
      allergies: [],
      description: 'No issues.',
    };

    component.medicalRecords = [mockMedicalRecord];

    component.toggleMedicalRecord(mockPatient);

    expect(component.fullPatient).toEqual(mockPatient);
    expect(component.fullPatientMedicalRecord).toEqual(mockMedicalRecord);
    expect(component.showMedicalRecord).toBeTrue();
  });

});
