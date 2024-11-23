using System;
using MDBackoffice.Domain.Shared;
using MDBackoffice.Domain.Patients;
using MDBackoffice.Domain.StaffProfiles;
using MDBackoffice.Domain.OperationTypes;
using System.Collections.Generic;

namespace MDBackoffice.Domain.OperationRequests
{
    public class StaffForRequestEntry
    {
        public List<StaffDto> Staff { get; set; }
        public string Value { get; set; }
    }

    public class OperationRequestScheduleInfoDto
    {
        public List<StaffForRequestEntry> StaffForRequest { get; set; }

        public string SelectedRoomId { get; set; }

        public string Algorithm { get; set; }

        public string Day { get; set; }

        public OperationRequestScheduleInfoDto(
            List<StaffForRequestEntry> staffForRequest,
            string selectedRoomId,
            string algorithm,
            string day)
        {
            this.StaffForRequest = staffForRequest;
            this.SelectedRoomId = selectedRoomId;
            this.Algorithm = algorithm;
            this.Day = day;
        }
    }

}
