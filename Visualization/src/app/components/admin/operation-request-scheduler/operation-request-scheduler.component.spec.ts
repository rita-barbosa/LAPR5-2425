import { ComponentFixture, TestBed } from '@angular/core/testing';
import { OperationRequestScheduler } from './operation-request-scheduler.component';


describe('ListStaffProfiles', () => {
  let component: OperationRequestScheduler;
  let fixture: ComponentFixture<OperationRequestScheduler>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [OperationRequestScheduler]
    })
    .compileComponents();

    fixture = TestBed.createComponent(OperationRequestScheduler);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
