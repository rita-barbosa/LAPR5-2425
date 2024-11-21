using System.Collections.Generic;
using MDBackoffice.Domain.Appointments;
using MDBackoffice.Domain.OperationRequests;
using MDBackoffice.Domain.Rooms;
using MDBackoffice.Domain.StaffProfiles;

namespace MDBackoffice.Domain.OperationRequests
{
    public interface IOperationSchedulerAdapter
    {
        List<AppointmentDto> ScheduleOperations(
            Dictionary<ScheduleOperationRequestDto, List<ScheduleStaffDto>> operationsMap,
            RoomDto room,
            string day,
            string algorithm);
    }
}