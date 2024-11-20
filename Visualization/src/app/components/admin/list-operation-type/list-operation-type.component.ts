import { Component, OnInit } from '@angular/core';
import { SideBarAdminComponent } from "../sidebar-admin/side-bar-admin.component";
import { CommonModule } from '@angular/common';
import { TableModule } from 'primeng/table';
import { FormsModule } from '@angular/forms';
import { MessageComponent } from '../../message/message.component';
import { ListOperationType } from '../../../domain/list-operation-type';
import { OperationType } from '../../../domain/OperationType';
import { OperationTypeService } from '../../../services/operation-type.service';
import { OperationTypeEdit } from '../../../domain/OperationTypeEdit';

@Component({
  selector: 'app-list-operation-type',
  standalone: true,
  imports: [SideBarAdminComponent, CommonModule, TableModule, FormsModule, MessageComponent],
  templateUrl: './list-operation-type.component.html',
  styleUrl: './list-operation-type.component.css'
})
export class ListOperationTypeComponent implements OnInit {
  operationTypes: ListOperationType[] = [];
  selectedOperationType!: ListOperationType;
  fullOperationType!: OperationType;
  detailsVisible: boolean = false;
  editDetails: boolean = false;

  filters = {
    name: '',
    specialization: '',
    status: ''
  };

  constructor(private service: OperationTypeService) { }

  ngOnInit(): void {
    this.fetchOperations();
  }

  fetchOperations(): void {
    // Construir a lista de filtros
    const queryFilters = [
      {
        name: this.filters.name || '',
        specialization: this.filters.specialization || '',
        status: this.filters.status || ''
      }
    ];

    // Fazer a requisição para o endpoint do backend
    this.service.getOperationTypesByFilters({ queryFilters }).subscribe({
      next: data => {
        console.log('Fetched Operations:', data); // Log para validar a resposta
        this.operationTypes = data;
      },
      error: err => {
        console.error('Error fetching operation types:', err);
        this.operationTypes = [];
      }
    });
  }

  applyFilters(): void {
    this.fetchOperations();
  }

  toggleDetails(operationType: ListOperationType): void {
    this.service.getOperationTypeById(operationType.name).subscribe({
      next: (fullRequest: OperationType) => {
        this.fullOperationType = fullRequest;
        this.detailsVisible = true; // Show the details section after fetching the data
      },
      error: (error) => {
        console.error('Error fetching operation type details:', error);
      }
    });
  }
  closeDetails(): void {
    this.detailsVisible = false;
  }

  editOperationType(operationTypeEdit: OperationTypeEdit): void {
    operationTypeEdit.id = this.selectedOperationType.name;
  }

  toggleEdition(operationType: ListOperationType): void {
    this.editDetails = true;
  }

  closeEdition(): void {
    this.editDetails = false;
  }

  deleteOperationType(operationType: ListOperationType): void {
    this.service.removeOperationType(operationType.name);
    this.fetchOperations(); // to update the list of operation types
  }

}
