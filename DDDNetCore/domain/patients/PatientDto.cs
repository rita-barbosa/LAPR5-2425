using DDDNetCore.Domain.Shared;

namespace DDDNetCore.Domain.Patients
{
    public class PatientDto
    {
        public string Name { get; set; }
        public string Phone { get; set; }
        public string Email { get; set; }
        public string? Address { get; set; }
        public string PatientId { get; set; }

        public PatientDto(string name, string phone, string email, string address, string patientId)
        {
            Name = name;
            Phone = phone;
            Email = email;
            Address = address;
            PatientId = patientId;
            
        }

        public PatientDto(string name, string phone, string email, string patientId)
        {
            Name = name;
            Phone = phone;
            Email = email;
            PatientId = patientId;
        }
    }
}
