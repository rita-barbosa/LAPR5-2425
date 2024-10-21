using System;
using DDDNetCore.Domain.Shared;
using DDDNetCore.Domain.StaffProfiles;
using DDDNetCore.Domain.Patients;
using DDDNetCore.Domain.OperationTypes;
using DDDNetCore.Domain.Tokens;

namespace DDDNetCore.Domain.OperationRequest
{
    public class OperationRequest : Entity<OperationRequestId>, IAggregateRoot{

        public Date DeadLineDate { get; private set; }

        public Priority Priority { get; private set; }

        public Date DateOfRequest { get; private set; }

        public Status Status { get; private set; }

        public StaffId StaffId { get; private set; } 

        public MedicalRecordNumber PatientId { get; private set; } 

        public OperationTypeId OperationTypeId { get; private set; } 

        public OperationRequest(){
    
        }

        public OperationRequest(string code, string deadLineDate, string priority, string dateOfRequest, string status, StaffId staffId, MedicalRecordNumber patientId, OperationTypeId operationTypeId)
        {
            this.Id = new OperationRequestId(code);
            this.DeadLineDate = new Date(deadLineDate);
            this.Priority = new Priority(priority);
            this.DateOfRequest = new Date(dateOfRequest);
            this.Status = new Status(status);
            this.StaffId = staffId;
            this.PatientId = patientId;
            this.OperationTypeId = operationTypeId;      
        }

        public OperationRequest(Date deadLineDate, Priority priority, Date dateOfRequest, Status status, StaffId staffId, MedicalRecordNumber patientId, OperationTypeId operationTypeId)
        {
            this.Id = new OperationRequestId(Guid.NewGuid());
            Console.WriteLine($"OperationRequestId: {this.Id.Value}");
            this.DeadLineDate = deadLineDate;
            this.Priority = priority;
            this.DateOfRequest = dateOfRequest;
            this.Status = status;
            this.StaffId = staffId;
            this.PatientId = patientId;
            this.OperationTypeId = operationTypeId;
        }

        public void ChangeDeadLineDate(string deadLineDate)
        {
            this.DeadLineDate = new Date(deadLineDate);
        }

        public void ChangePriority(string priority)
        {
            this.Priority = new Priority(priority);
        }

        public void ChangeDateOfRequest(string dateOfRequest)
        {
            this.DateOfRequest = new Date(dateOfRequest);
        }

        public void ChangeStatus(string status)
        {
            this.Status = new Status(status);
        }

        public void ChangePatientId(string patientId)
        {
            this.PatientId = new MedicalRecordNumber(patientId);
        }
        

    }
}