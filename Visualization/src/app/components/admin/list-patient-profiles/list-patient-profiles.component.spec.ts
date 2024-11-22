import { ComponentFixture, TestBed } from '@angular/core/testing';
import { ListStaffProfiles } from '../list-staff-profiles/list-staff-profiles.component';

describe('ListStaffProfiles', () => {
  let component: ListStaffProfiles;
  let fixture: ComponentFixture<ListStaffProfiles>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [ListStaffProfiles]
    })
    .compileComponents();

    fixture = TestBed.createComponent(ListStaffProfiles);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
