import { ComponentFixture, TestBed } from '@angular/core/testing';

import { VerifyProfileEditComponent } from './verify-profile-edit.component';

describe('VerifyProfileEditComponent', () => {
  let component: VerifyProfileEditComponent;
  let fixture: ComponentFixture<VerifyProfileEditComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [VerifyProfileEditComponent]
    })
    .compileComponents();

    fixture = TestBed.createComponent(VerifyProfileEditComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
