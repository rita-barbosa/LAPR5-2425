import { ComponentFixture, TestBed } from '@angular/core/testing';

import { SideBarPatientComponent } from './side-bar-patient.component';

describe('SideBarPatientComponent', () => {
  let component: SideBarPatientComponent;
  let fixture: ComponentFixture<SideBarPatientComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [SideBarPatientComponent]
    })
    .compileComponents();

    fixture = TestBed.createComponent(SideBarPatientComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
