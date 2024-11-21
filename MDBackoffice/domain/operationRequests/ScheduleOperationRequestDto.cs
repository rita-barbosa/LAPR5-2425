using System;
using MDBackoffice.Domain.Shared;
using MDBackoffice.Domain.Patients;
using MDBackoffice.Domain.StaffProfiles;
using MDBackoffice.Domain.OperationTypes;

namespace MDBackoffice.Domain.OperationRequests
{
    public class ScheduleOperationRequestDto
    {
        public string Id { get; set; }

        public string OperationTypeId { get; set; }

        public string PrepTime { get; set; }

        public string SurgTime { get; set; }

        public string CleanTime { get; set; }


        public ScheduleOperationRequestDto(string Id, string operationType, string prepTime, string surgTime, string cleanTime)
        {
            this.Id = Id;
            this.OperationTypeId = operationType;
            this.PrepTime = prepTime;
            this.SurgTime = surgTime;
            this.CleanTime = cleanTime;
        }
    }
}