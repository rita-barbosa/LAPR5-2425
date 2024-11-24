import { ComponentFixture, TestBed } from '@angular/core/testing';

import { HospitalSimulationComponent } from './hospital-simulation.component';
import { RoomService } from '../services/room.service';
import { of } from 'rxjs';
import { ActivatedRoute } from '@angular/router';

describe('HospitalSimulationComponent', () => {
  let component: HospitalSimulationComponent;
  let fixture: ComponentFixture<HospitalSimulationComponent>;
  const MockRoomService = jasmine.createSpyObj('RoomService', ['getRoomsSchedule']);
  MockRoomService.getRoomsSchedule.and.returnValue(of([]));

  beforeEach(async () => {
    const activatedRouteMock = {
      snapshot: {
        paramMap: {
          get: jasmine.createSpy('get').and.returnValue(null),
        },
      },
    };

    await TestBed.configureTestingModule({
      imports: [HospitalSimulationComponent],
      providers: [
        { provide: RoomService, useValue: MockRoomService },
        { provide: ActivatedRoute, useValue: activatedRouteMock }
      ]
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
