import { ComponentFixture, TestBed } from '@angular/core/testing';
import { of, throwError } from 'rxjs';  // Import RxJS utilities to mock observables
import { CreateSurgeryAppointmentComponent } from './create-surgery-appointment.component';
import { SurgeryAppointmentService } from 'src/app/services/surgery-appointment.service';
import { FormsModule, NgForm } from '@angular/forms';
import { CommonModule } from '@angular/common';
import { ActivatedRoute, Router } from '@angular/router';
import { TableModule } from 'primeng/table';
import { RoomService } from 'src/app/services/room.service';
import { OperationRequestService } from 'src/app/services/operation-request.service';
import { StaffService } from 'src/app/services/staff.service';

// Mock dos Serviços
const surgeryAppointmentServiceMock = {
  createSurgeryAppointment: jasmine.createSpy('createSurgeryAppointment').and.returnValue(of({})),
};

const roomServiceMock = {
  getAllRooms: jasmine.createSpy('getAllRooms').and.returnValue(of([])),
};

const operationRequestServiceMock = {
  getAllOperationRequests: jasmine.createSpy('getAllOperationRequests').and.returnValue(of([])),
};

const staffServiceMock = {
  getAllActiveStaffProfiles: jasmine.createSpy('getAllActiveStaffProfiles').and.returnValue(of([])),
};

const routerMock = {
  navigate: jasmine.createSpy('navigate'),
};

const activatedRouteMock = {
  snapshot: {
    paramMap: {
      get: jasmine.createSpy('get').and.returnValue(null),
    },
  },
}

describe('CreateSurgeryAppointmentComponent', () => {
  let component: CreateSurgeryAppointmentComponent;
  let fixture: ComponentFixture<CreateSurgeryAppointmentComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [FormsModule, CommonModule, TableModule],
      declarations: [],
      providers: [
        { provide: SurgeryAppointmentService, useValue: surgeryAppointmentServiceMock },
        { provide: RoomService, useValue: roomServiceMock },
        { provide: OperationRequestService, useValue: operationRequestServiceMock },
        { provide: StaffService, useValue: staffServiceMock },
        { provide: ActivatedRoute, useValue: activatedRouteMock },
      ],
    }).compileComponents();

    fixture = TestBed.createComponent(CreateSurgeryAppointmentComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create the component', () => {
    expect(component).toBeTruthy();
  });
});
