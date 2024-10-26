using System.Collections.Generic;
using DDDNetCore.Domain.OperationTypes.ValueObjects.Phase;
using DDDNetCore.Domain.OperationTypes.ValueObjects.RequiredStaff;

namespace DDDNetCore.Domain.OperationTypes
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