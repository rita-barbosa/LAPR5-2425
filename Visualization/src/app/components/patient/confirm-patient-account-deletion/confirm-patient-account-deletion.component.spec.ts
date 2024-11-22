import { ComponentFixture, TestBed } from '@angular/core/testing';
import { ActivationComponent } from '../verify/verify.component';

describe('VerifyComponent', () => {
  let component: ActivationComponent;
  let fixture: ComponentFixture<ActivationComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [ActivationComponent]
    })
    .compileComponents();

    fixture = TestBed.createComponent(ActivationComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
