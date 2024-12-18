import { TestBed, ComponentFixture } from '@angular/core/testing';
import { ListSpecializationComponent } from './list-specialization.component';
import { RouterTestingModule } from '@angular/router/testing';
import { FormsModule } from '@angular/forms';
import { MessageComponent } from '../../message/message.component';
import { SideBarAdminComponent } from '../sidebar-admin/side-bar-admin.component';
import { SpecializationService } from 'src/app/services/specialization.service';
import { CommonModule } from '@angular/common';
import { TableModule } from 'primeng/table';
import { ActivatedRoute } from '@angular/router';
import { of } from 'rxjs';  // Import 'of' to return observables

describe('ListSpecializationComponent', () => {
  let component: ListSpecializationComponent;
  let fixture: ComponentFixture<ListSpecializationComponent>;

  beforeEach(async () => {
    const mockSpecializationService = jasmine.createSpyObj('SpecializationService', [
      'getSpecializationsByFilters',
      'getSpecializationById',
      'updateSpecialization',
      'deleteSpecializationById'
    ]);

    // Mock 'getSpecializationsByFilters' to return an observable with mock data
    mockSpecializationService.getSpecializationsByFilters.and.returnValue(of([]));

    const activatedRouteMock = {
      snapshot: {
        paramMap: {
          get: jasmine.createSpy('get').and.returnValue(null),
        },
      },
    };

    await TestBed.configureTestingModule({
      imports: [
        CommonModule, TableModule, FormsModule, MessageComponent, SideBarAdminComponent, RouterTestingModule
      ],
      providers: [
        { provide: SpecializationService, useValue: mockSpecializationService },
        { provide: ActivatedRoute, useValue: activatedRouteMock }
      ]
    }).compileComponents();

    fixture = TestBed.createComponent(ListSpecializationComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
