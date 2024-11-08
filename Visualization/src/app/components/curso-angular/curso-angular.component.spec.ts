import { ComponentFixture, TestBed } from '@angular/core/testing';

import { CursoAngularComponent } from './curso-angular.component';

describe('CursoAngularComponent', () => {
  let component: CursoAngularComponent;
  let fixture: ComponentFixture<CursoAngularComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [CursoAngularComponent]
    })
    .compileComponents();

    fixture = TestBed.createComponent(CursoAngularComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
