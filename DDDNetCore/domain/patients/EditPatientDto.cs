namespace DDDNetCore.Domain.Patients
{
    public class EditPatientDto
    {
        public string? Name { get; set; }
        public string? Phone { get; set; }
        public string? EmergencyContact { get; set; }
        public string? Email { get; set; }
        public string? Address { get; set; }
        public string? DateBirth { get; set;}

        public string PatientId { get; set; }

        public EditPatientDto(string name, string phone, string email, string address, string patientId)
        {
            PatientId = patientId;
            Name = name;
            Phone = phone;
            Email = email;
            Address = address;
        }
        public EditPatientDto(string name, string phone, string emContact, string email, string address, string patientId)
        {
            PatientId = patientId;
            Name = name;
            Phone = phone;
            EmergencyContact = emContact;
            Email = email;
            DateBirth = DateBirth;
            Address = address;
        }
    }
}