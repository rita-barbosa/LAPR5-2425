using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.Json;
using MDBackoffice.Domain.Appointments;
using MDBackoffice.Domain.OperationRequests;
using MDBackoffice.Domain.Rooms;
using MDBackoffice.Domain.StaffProfiles;

namespace MDBackoffice.Infrastructure.OperationRequests
{
    public class PlanningSchedulerAdapter : IOperationSchedulerAdapter
    {
        public List<SchedulesDto> ScheduleOperations(
            Dictionary<ScheduleOperationRequestDto, List<ScheduleStaffDto>> operationsMap,
            RoomDto room,
            string day,
            string algorithm)
        {
            var agRoom = new
            {
                roomId = room.RoomNumber,
                date = day,
                occupied = room.MaintenanceSlots.Select(slot => new
                {
                    start = slot.StartTime, // Combine date and time in ISO format
                    end = slot.EndTime,
                    operationId = slot.Name
                }).ToList()
            };

            var staffJson = operationsMap
                .SelectMany(op => op.Value, (operation, staff) => new
                {
                    staffId = staff.Id,
                    function = staff.Function,
                    spec = staff.SpecializationId,
                    assocOp = new List<string>()
                })
                .Distinct() // Remove duplicates if staff are referenced in multiple operations
                .ToList();

            var agStaffJson = operationsMap
                .SelectMany(op => op.Value, (operation, staff) => new
                {
                    staffId = staff.Id,
                    date = day,
                    schedule = staff.Slots.Select(slot => new
                    {
                        start = slot.StartTime, // Combine date and time in ISO format
                        end = slot.EndTime,
                        operationId = slot.Name
                    }).ToList()
                })
                .Distinct() // Remove duplicates if staff are referenced in multiple operations
                .ToList();

            var timetableJson = operationsMap
               .SelectMany(op => op.Value, (operation, staff) => new
               {
                   staffId = staff.Id,
                   date = day,
                   availability = new
                   {
                       start = 400, // Combine date and time in ISO format
                       end = 1400,
                   }
               })
               .Distinct() // Remove duplicates if staff are referenced in multiple operations
               .ToList();

            var surgeryJson = operationsMap
                .Keys // Get all OperationRequestDto keys from the dictionary
                .Select(operation => new
                {
                    surgeryId = operation.OperationTypeId,
                    prepTime = operation.PrepTime,
                    surgeryDuration = operation.SurgTime,
                    recoveryTime = operation.CleanTime
                })
                .ToList();

            var surgeryIdJson = operationsMap
              .Keys // Get all OperationRequestDto keys from the dictionary
              .Select(operation => new
              {
                  operationId = operation.Id,
                  surgeryId = operation.OperationTypeId
              })
              .ToList();


            var assignSurgeryJson = operationsMap
                .SelectMany(op => op.Value, (operation, staff) => new
                {
                    operationId = operation.Key.Id, // Assuming OperationRequestDto has OperationId
                    staffId = staff.Id // Assuming StaffDto has StaffId
                })
                .ToList();
            var allData = new
            {
                agRoom,
                staffJson,
                agStaffJson,
                timetableJson,
                surgeryJson,
                surgeryIdJson,
                assignSurgeryJson
            };

            string json = JsonSerializer.Serialize(allData, new JsonSerializerOptions { WriteIndented = true });
            Console.WriteLine(json);
            return new List<SchedulesDto>();
        }
    }
}