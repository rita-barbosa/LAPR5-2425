using System.Collections.Generic;
using MDBackoffice.Domain.OperationTypes.ValueObjects.Phase;
using MDBackoffice.Domain.OperationTypes.ValueObjects.RequiredStaff;

namespace MDBackoffice.Domain.OperationTypes
{
    public class EditOpTypeDto(string id, string newName, int newEstimatedDuration)
    {
        public string Id { get; set; } = id;
        public string Name { get; set; } = newName;
        public int EstimatedDuration { get; set; } = newEstimatedDuration;
        public List<RequiredStaffDto> RequiredStaff {get; set;}
        public List<PhaseDto> Phases {get; set;}
    }
}