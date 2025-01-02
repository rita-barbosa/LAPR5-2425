import { ComponentFixture, TestBed } from '@angular/core/testing';
import { CreatePatientProfileComponent } from './create-patient-profile.component';
import { PatientService } from '../../../services/patient.service';
import { ActivatedRoute } from '@angular/router';
import { NgForm } from '@angular/forms';
import { of } from 'rxjs';

describe('CreatePatientProfileComponent', () => {
  let component: CreatePatientProfileComponent;
  let fixture: ComponentFixture<CreatePatientProfileComponent>;
  let mockPatientService: jasmine.SpyObj<PatientService>;

  beforeEach(async () => {
    mockPatientService = jasmine.createSpyObj('PatientService', ['createPatientProfile']);

    await TestBed.configureTestingModule({
      imports: [CreatePatientProfileComponent],
      providers: [
        { provide: PatientService, useValue: mockPatientService },
        {
          provide: ActivatedRoute,
          useValue: {
            snapshot: { params: {} },
            paramMap: of(),
          },
        },
      ],
    }).compileComponents();

    fixture = TestBed.createComponent(CreatePatientProfileComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create the component', () => {
    expect(component).toBeTruthy();
  });

  it('should call PatientService.createPatientProfile with correct parameters on valid form submit', () => {
    // Arrange
    component.patient = {
      firstName: 'John',
      lastName: 'Doe',
      phone: '1234567890',
      emergencyContact: '0987654321',
      email: 'john.doe@example.com',
      address: '123 Main St',
      gender: 'Male',
      dateBirth: '2000-01-01',
      medicalConditions : [],
      allergies: [],
      description: ''
    };

    const form = {
      valid: true,
      resetForm: jasmine.createSpy(),
    } as unknown as NgForm;

    // Act
    component.onSubmit(form);

    // Assert
    expect(mockPatientService.createPatientProfile).toHaveBeenCalledWith(
      'John',
      'Doe',
      '1234567890',
      'john.doe@example.com',
      '123 Main St',
      '0987654321',
      'Male',
      '2000-01-01',
      [],
      [],
      ''
    );
  });

  it('should not call PatientService.createPatientProfile on invalid form submit', () => {
    // Arrange
    const form = { valid: false } as unknown as NgForm;

    // Act
    component.onSubmit(form);

    // Assert
    expect(mockPatientService.createPatientProfile).not.toHaveBeenCalled();
  });

  it('should reset the form and fields when clearForm is called', () => {
    // Arrange
    component.patient = {
      firstName: 'John',
      lastName: 'Doe',
      phone: '1234567890',
      emergencyContact: '0987654321',
      email: 'john.doe@example.com',
      address: '123 Main St',
      gender: 'Male',
      dateBirth: '2000-01-01',
      medicalConditions : [],
      allergies: [],
      description: ''
    };
    spyOn(component.patientForm, 'resetForm');

    // Act
    component.clearForm();

    // Assert
    expect(component.isSubmitted).toBeFalse();
    expect(component.patient).toEqual({
      firstName: '',
      lastName: '',
      phone: '',
      emergencyContact: '',
      email: '',
      address: '',
      gender: '',
      dateBirth: '',
      medicalConditions : [],
      allergies: [],
      description: ''
    });
    expect(component.patientForm.resetForm).toHaveBeenCalled();
  });

  it('should remove invalid-placeholder class from input fields in clearForm', () => {
    // Arrange
    const mockInput = document.createElement('input');
    mockInput.classList.add('invalid-placeholder');
  
    const mockNodeList = {
      forEach: (callback: (el: Element) => void) => callback(mockInput),
      length: 1,
      item: (index: number) => (index === 0 ? mockInput : null),
      [0]: mockInput,
    } as unknown as NodeListOf<HTMLInputElement>;
  
    spyOn(document, 'querySelectorAll').and.returnValue(mockNodeList);
  
    // Act
    component.clearForm();
  
    // Assert
    expect(mockInput.classList.contains('invalid-placeholder')).toBeFalse();
  });
  

});
