import { ComponentFixture, TestBed } from '@angular/core/testing';

import { HospitalSimulationComponent } from './hospital-simulation.component';

describe('HospitalSimulationComponent', () => {
  let component: HospitalSimulationComponent;
  let fixture: ComponentFixture<HospitalSimulationComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [HospitalSimulationComponent]
    })
    .compileComponents();

    fixture = TestBed.createComponent(HospitalSimulationComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
