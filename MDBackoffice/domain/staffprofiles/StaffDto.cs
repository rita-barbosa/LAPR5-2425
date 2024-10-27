using System;
using System.Collections.Generic;
using MDBackoffice.Domain.Shared;

namespace MDBackoffice.Domain.StaffProfiles
{
    public class StaffDto
    {
        public string Id { get; set; }
        public string Name { get; set; }
        public string Phone { get; set; }
        public string Email { get; set; }
        public string Address { get; set; }
        public string SpecializationId { get; set; }
        public List<SlotsDto>? Slots { get; set; }

        public StaffDto(string name, string phone, string email, string address, string staffId, string specializationDenomination)
        {
            Name = name;
            Phone = phone;
            Email = email;
            Address = address;
            SpecializationId = specializationDenomination;
            Id = staffId;
        }
        public StaffDto(string name, string phone, string email, string address, string staffId, string specializationDenomination, List<SlotsDto> slots)
        {
            Name = name;
            Phone = phone;
            Email = email;
            Address = address;
            SpecializationId = specializationDenomination;
            Id = staffId;
            Slots = slots;
        }
    }
}