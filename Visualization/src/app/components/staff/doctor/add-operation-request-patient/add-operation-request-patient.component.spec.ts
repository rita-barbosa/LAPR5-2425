import { ComponentFixture, TestBed } from '@angular/core/testing';

import { AddOperationRequestPatientComponent } from './add-operation-request-patient.component';

describe('AddOperationRequestPatientComponent', () => {
  let component: AddOperationRequestPatientComponent;
  let fixture: ComponentFixture<AddOperationRequestPatientComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [AddOperationRequestPatientComponent]
    })
    .compileComponents();

    fixture = TestBed.createComponent(AddOperationRequestPatientComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
