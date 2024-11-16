import { Component } from '@angular/core';
import { SideBarDoctorComponent } from '../sidebar-doctor/side-bar-doctor.component';
import { CommonModule } from '@angular/common';
import { TableModule } from 'primeng/table';
import { OperationRequest } from '../../../../domain/operation-request';

@Component({
  selector: 'app-list-operation-request',
  standalone: true,
  imports: [SideBarDoctorComponent, CommonModule, TableModule],
  templateUrl: './list-operation-request.component.html',
  styleUrl: './list-operation-request.component.css'
})
export class ListOperationRequestComponent {
  operationRequests: OperationRequest[] = [
    {
      deadlineDate: '2024-12-01',
      priority: 'High',
      dateOfRequest: '2024-11-16',
      status: 'Pending',
      description: 'Appendectomy procedure',
      operationTypeId: '12345'
    },
    {
      deadlineDate: '2024-11-30',
      priority: 'Medium',
      dateOfRequest: '2024-11-15',
      status: 'Approved',
      description: 'Knee surgery',
      operationTypeId: '67890'
    }
  ];

  selectedOperationRequest: OperationRequest = {deadlineDate: '2024-12-01',
    priority: 'High',
    dateOfRequest: '2024-11-16',
    status: 'Pending',
    description: 'Appendectomy procedure',
    operationTypeId: '12345'};
  detailsVisible: boolean = false;

  // Toggle the visibility of additional details for the selected operation request
  toggleDetails(operationRequest: OperationRequest): void {
    if (this.selectedOperationRequest === operationRequest) {
      this.detailsVisible = !this.detailsVisible;
    } else {
      this.selectedOperationRequest = operationRequest;
      this.detailsVisible = true;
    }
  }

  performOperation(operationRequest: OperationRequest): void {
    // Implement logic for performing an operation (e.g., Edit, Delete)
  }


  // constructor(private service: OperationRequestService);

  // ngOnInit(): void {
  //   this.service.getOperationRequestsByFilters().subscribe({
  //     next: data => {
  //       this.operationRequests = data;
  //     }
  //   });
  // }

}
