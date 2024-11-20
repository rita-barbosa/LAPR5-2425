import { ComponentFixture, TestBed } from '@angular/core/testing';

import { EditPatientProfileComponent } from './edit-patient-profile.component';

describe('EditPatientProfileComponent', () => {
  let component: EditPatientProfileComponent;
  let fixture: ComponentFixture<EditPatientProfileComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [EditPatientProfileComponent]
    })
    .compileComponents();

    fixture = TestBed.createComponent(EditPatientProfileComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
