using System;

namespace DDDNetCore.Domain.StaffProfiles
{
    public class StaffDto
    {
        public string Id { get; set; }
        public string Name { get; set; }
        public string Phone { get; set; }
        public string Email { get; set; }
        public string SpecializationId { get; set; }

        public StaffDto(string name, string phone, string email, string staffId, string specializationDenomination)
        {
            Name = name;
            Phone = phone;
            Email = email;
            SpecializationId = specializationDenomination;
            Id = staffId;
        }
    }
}