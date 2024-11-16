import { ComponentFixture, TestBed } from '@angular/core/testing';

import { ListOperationRequestComponent } from './list-operation-request.component';

describe('ListOperationRequestComponent', () => {
  let component: ListOperationRequestComponent;
  let fixture: ComponentFixture<ListOperationRequestComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [ListOperationRequestComponent]
    })
    .compileComponents();

    fixture = TestBed.createComponent(ListOperationRequestComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
