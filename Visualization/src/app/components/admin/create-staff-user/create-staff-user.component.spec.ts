import { ComponentFixture, TestBed } from '@angular/core/testing';

import { CreateStaffUserComponent } from './create-staff-user.component';

describe('CreateStaffUserComponent', () => {
  let component: CreateStaffUserComponent;
  let fixture: ComponentFixture<CreateStaffUserComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [CreateStaffUserComponent]
    })
    .compileComponents();

    fixture = TestBed.createComponent(CreateStaffUserComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
