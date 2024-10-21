using DDDNetCore.Domain.Patients;
using DDDNetCore.Domain.StaffProfiles;
using DDDNetCore.Domain.OperationTypes;

namespace DDDNetCore.Domain.OperationRequest
{
    public class CreatingOperationRequestDto
    {
        public string DeadLineDate { get; set; }

        public string Priority { get; set; }

        public string DateOfRequest { get; set; }

        public string Status { get; set; }

        public string StaffId { get; set; } 

        public string PatientId { get; set; } 

        public string OperationTypeId { get; set; } 


        public CreatingOperationRequestDto(string deadLineDate, string priority, string dateOfRequest, string status, string staffId, string patientId, string operationTypeId)
        {
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