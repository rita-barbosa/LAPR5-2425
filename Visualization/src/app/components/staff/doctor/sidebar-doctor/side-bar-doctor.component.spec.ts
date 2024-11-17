import { ComponentFixture, TestBed } from '@angular/core/testing';

import { SideBarDoctorComponent } from './side-bar-doctor.component';

describe('SideBarDoctorComponent', () => {
  let component: SideBarDoctorComponent;
  let fixture: ComponentFixture<SideBarDoctorComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [SideBarDoctorComponent]
    })
    .compileComponents();

    fixture = TestBed.createComponent(SideBarDoctorComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
