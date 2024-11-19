import { Component, OnInit } from '@angular/core';
import { FormsModule } from '@angular/forms';
import { CommonModule } from '@angular/common';
import { TableModule } from 'primeng/table';
import { MessageComponent } from '../../message/message.component';

import { SideBarAdminComponent } from '../sidebar-admin/side-bar-admin.component';
import { StaffService } from '../../../services/staff.service';
import { StaffListingFilterParameters } from '../../../domain/staff-listing-filter-parameters';
import { StaffWithId } from '../../../domain/staff-with-id';
import { StaffQueryParameters } from '../../../domain/staff-query-parameters';

@Component({
  selector: 'app-list-staff-profiles',
  standalone: true,
  imports: [SideBarAdminComponent, CommonModule, TableModule, FormsModule, MessageComponent],
  templateUrl: './list-staff-profiles.component.html',
  styleUrls: ['./list-staff-profiles.component.css']
})
export class ListStaffProfiles implements OnInit {
  staffList: StaffWithId[] = [];
  selectedStaff!: StaffWithId;
  fullStaff!: StaffWithId;
  detailsVisible: boolean = false;

  specializations: string[] = [];
  functions: string[] = ['Doctor', 'Intern', 'Nurse', 'Assistant'];

  // Dynamic query filters
  queryFiltersList: StaffListingFilterParameters[] = [];

  constructor(private service: StaffService) {}

  ngOnInit(): void {
    this.service.getAllSpecializationsAvailable().subscribe({
      next: (data) => {
        this.specializations = data;
      }
    });
    this.addFilter();
    this.fetchStaff();
  }

  fetchStaff(): void {
    const queryParameters: StaffQueryParameters = {
      queryfilters: this.queryFiltersList
    };

    this.service.getStaffByFilters(queryParameters).subscribe({
      next: (data) => {
        console.log('Fetched staff profiles:', data);
        this.staffList = data;
      },
      error: (error) => {
        console.error('Error fetching staff profiles:', error);
      }
    });
  }

  applyFilters(): void {
    this.fetchStaff();
  }

  addFilter(): void {
    this.queryFiltersList.push({
      firstName: '',
      lastName: '',
      email: '',
      specialization: ''
    });
  }

  removeFilter(index: number): void {
    this.queryFiltersList.splice(index, 1);
  }

  toggleDetails(staff: StaffWithId): void {
    this.service.getStaffById(staff.id).subscribe({
      next: (fullStaffInfo: StaffWithId) => {
        this.fullStaff = fullStaffInfo;
        this.detailsVisible = true;
      },
      error: (error: any) => {
        console.error('Error fetching staff profile details:', error);
      }
    });
  }

  closeDetails(): void {
    this.detailsVisible = false;
  }

  editStaffProfile(staff: StaffWithId): void {
    // Implement edit logic
  }

  deleteStaffProfile(staff: StaffWithId): void {
    // Implement delete logic
  }
}
