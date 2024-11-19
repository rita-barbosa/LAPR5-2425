import { ComponentFixture, TestBed } from '@angular/core/testing';

import { VerifyStaffComponent } from './verify-staff.component';

describe('VerifyStaffComponent', () => {
  let component: VerifyStaffComponent;
  let fixture: ComponentFixture<VerifyStaffComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [VerifyStaffComponent]
    })
    .compileComponents();

    fixture = TestBed.createComponent(VerifyStaffComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
