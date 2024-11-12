import { ComponentFixture, TestBed } from '@angular/core/testing';

import { SideBarStaffComponent } from './side-bar-staff.component';

describe('SideBarStaffComponent', () => {
  let component: SideBarStaffComponent;
  let fixture: ComponentFixture<SideBarStaffComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [SideBarStaffComponent]
    })
    .compileComponents();

    fixture = TestBed.createComponent(SideBarStaffComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
