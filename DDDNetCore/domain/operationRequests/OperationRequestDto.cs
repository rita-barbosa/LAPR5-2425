using System;
using DDDNetCore.Domain.Shared;
using DDDNetCore.Domain.Patients;
using DDDNetCore.Domain.StaffProfiles;
using DDDNetCore.Domain.OperationTypes;

namespace DDDNetCore.Domain.OperationRequest
{
    public class OperationRequestDto
    {
        public Guid Id { get; set; }
        public string DeadLineDate { get; set; }

        public string Priority { get; set; }

        public string DateOfRequest { get; set; }

        public string Status { get; set; }

        public StaffId StaffId { get; set; } 

        public MedicalRecordNumber PatientId { get; set; } 

        public OperationTypeId OperationTypeId { get; set; } 


        public OperationRequestDto(Guid Id, string deadLineDate, string priority, string dateOfRequest, string status, StaffId staffId, MedicalRecordNumber patientId, OperationTypeId operationTypeId)
        {
            this.Id = Id;
            this.DeadLineDate = deadLineDate;
            this.Priority = priority;
            this.DateOfRequest = dateOfRequest;
            this.Status = status;
            this.StaffId = staffId;
            this.PatientId = patientId;
            this.OperationTypeId = operationTypeId;  
        }
    }
}