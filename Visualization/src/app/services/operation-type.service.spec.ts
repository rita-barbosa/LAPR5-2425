import { TestBed } from '@angular/core/testing';
import { OperationTypeService } from './operation-type.service';

describe('StaffService', () => {
  let service: OperationTypeService;

  beforeEach(() => {
    TestBed.configureTestingModule({});
    service = TestBed.inject(OperationTypeService);
  });

  it('should be created', () => {
    expect(service).toBeTruthy();
  });
});
