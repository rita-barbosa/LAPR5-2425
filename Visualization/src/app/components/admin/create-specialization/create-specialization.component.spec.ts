import { TestBed, ComponentFixture } from '@angular/core/testing';
import { CreateSpecializationComponent } from './create-specialization.component';
import { RouterTestingModule } from '@angular/router/testing';
import { FormsModule } from '@angular/forms';
import { MessageComponent } from '../../message/message.component';
import { SideBarAdminComponent } from '../sidebar-admin/side-bar-admin.component';
import { SpecializationService } from 'src/app/services/specialization.service';

describe('CreateSpecializationComponent', () => {
  let component: CreateSpecializationComponent;
  let fixture: ComponentFixture<CreateSpecializationComponent>;
  let mockSpecializationService: jasmine.SpyObj<SpecializationService>;

  beforeEach(async () => {
    mockSpecializationService = jasmine.createSpyObj('SpecializationService', ['createSpecialization']);
    
    await TestBed.configureTestingModule({
      imports: [
        CreateSpecializationComponent,
        RouterTestingModule,
        FormsModule,
        MessageComponent,
        SideBarAdminComponent
      ],
      providers: [
        { provide: SpecializationService, useValue: mockSpecializationService }
      ]
    }).compileComponents();

    fixture = TestBed.createComponent(CreateSpecializationComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});

