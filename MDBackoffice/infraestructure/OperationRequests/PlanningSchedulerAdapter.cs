using System;
using System.Collections.Generic;
using System.Linq;
using System.Net.Http;
using System.Text;
using System.Text.Json;
using System.Threading.Tasks;
using MDBackoffice.Domain.Appointments;
using MDBackoffice.Domain.OperationRequests;
using MDBackoffice.Domain.Rooms;
using MDBackoffice.Domain.StaffProfiles;

namespace MDBackoffice.Infrastructure.OperationRequests
{
    public class PlanningSchedulerAdapter : IOperationSchedulerAdapter
    {
        public async Task<List<SchedulesDto>> ScheduleOperationsAsync(
            Dictionary<ScheduleOperationRequestDto, List<ScheduleStaffDto>> operationsMap,
            RoomDto room,
            string day,
            string algorithm)
        {

            string json = CreateScheduleJson(operationsMap, room, day);
            Console.WriteLine(json);

            var url = "https://yourserver.com/api/schedule";

            using (var httpClient = new HttpClient())
            {
                var content = new StringContent(json, Encoding.UTF8, "application/json");
                try
                {
                    // Send POST request
                    var response = httpClient.PostAsync(url, content).Result;

                    // Ensure the response indicates success
                    response.EnsureSuccessStatusCode();

                    // Read and process the response
                    var responseBody = response.Content.ReadAsStringAsync().Result;
                    return ConvertJsonToDto(responseBody);
                }
                catch (Exception ex)
                {
                    Console.WriteLine($"Error occurred: {ex.Message}");
                    throw;
                }
            }



        }

        private string CreateScheduleJson(Dictionary<ScheduleOperationRequestDto, List<ScheduleStaffDto>> operationsMap,
            RoomDto room,
            string day)
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

            return JsonSerializer.Serialize(allData, new JsonSerializerOptions { WriteIndented = true });
        }

        private List<SchedulesDto> ConvertJsonToDto(String json)
        {

            return null;
        }
    }
}