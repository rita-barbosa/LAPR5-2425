namespace DDDNetCore.Domain.Patients
{
    public class PatientDto(string name, string phone, string email, string patientId)
    {

        public string Name { get; set; } = name;
        public string Phone { get; set; } = phone;
        public string Email { get; set; } = email;
        public string PatientId { get; set; } = patientId;
    }
}
