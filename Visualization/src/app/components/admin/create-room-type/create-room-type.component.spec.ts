import { TestBed, ComponentFixture } from '@angular/core/testing';
import { CreateRoomTypeComponent } from './create-room-type.component';
import { RouterTestingModule } from '@angular/router/testing';
import { FormsModule } from '@angular/forms';
import { MessageComponent } from '../../message/message.component';
import { SideBarAdminComponent } from '../sidebar-admin/side-bar-admin.component';
import { RoomTypeService } from 'src/app/services/room-type.service';

describe('CreateRoomTypeComponent', () => {
  let component: CreateRoomTypeComponent;
  let fixture: ComponentFixture<CreateRoomTypeComponent>;
  let mockRoomTypeService: jasmine.SpyObj<RoomTypeService>;

  beforeEach(async () => {
    mockRoomTypeService = jasmine.createSpyObj('RoomTypeService', ['createRoomType']);
    
    await TestBed.configureTestingModule({
      imports: [
        CreateRoomTypeComponent,
        RouterTestingModule,
        FormsModule,
        MessageComponent,
        SideBarAdminComponent
      ],
      providers: [
        { provide: RoomTypeService, useValue: mockRoomTypeService }
      ]
    }).compileComponents();

    fixture = TestBed.createComponent(CreateRoomTypeComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});

