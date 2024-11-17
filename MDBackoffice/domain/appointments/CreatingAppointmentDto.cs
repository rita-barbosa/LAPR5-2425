

using System;

namespace MDBackoffice.Domain.Appointments
{
    public class CreatingAppointmentDto
    {
        public string Status { get; set; }

        public string OperationRequestId { get; set; } 

        public string RoomNumber { get; set; } 

        public string StartTime { get; set; } 

        public string EndTime { get; set; } 
        
        public string StartDate { get; set; } 

        public string EndDate { get; set; } 


        public CreatingAppointmentDto(string status, string operationRequestId, string roomNumber, string startTime, string endTime, string startDate, string endDate)
        {
            Status = status;
            OperationRequestId = operationRequestId;
            RoomNumber = roomNumber;
            StartTime = startTime;
            EndTime = endTime;
            StartDate = startDate;
            EndDate = endDate;
        }
        
    }
}