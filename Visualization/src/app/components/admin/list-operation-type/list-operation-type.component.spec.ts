import { ComponentFixture, TestBed } from '@angular/core/testing';

import { ListOperationTypeComponent } from './list-operation-type.component';

describe('ListOperationTypeComponent', () => {
  let component: ListOperationTypeComponent;
  let fixture: ComponentFixture<ListOperationTypeComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [ListOperationTypeComponent]
    })
    .compileComponents();

    fixture = TestBed.createComponent(ListOperationTypeComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
