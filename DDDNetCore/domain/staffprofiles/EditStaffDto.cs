using System;
using System.Collections.Generic;
using DDDNetCore.Domain.Shared;

namespace DDDNetCore.Domain.StaffProfiles
{
    public class EditStaffDto
    {
        public string StaffId { get; set; }
        public string? Phone { get; set; }
        public string? Email { get; set; }
        public string? Address { get; set; }
        public string? SpecializationId { get; set; }
        public List<SlotsDto>? Slots { get; set; }

        public EditStaffDto(string id, string phone, string email, string address, string specializationDenomination, List<SlotsDto> slots)
        {
            StaffId = id;
            Phone = phone;
            Email = email;
            SpecializationId = specializationDenomination;
            Slots = slots;
        }
    }
}