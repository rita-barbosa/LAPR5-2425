namespace DDDNetCore.Domain.Patients
{
    public class CreatingPatientDto(string firstName, string lastName, string address, string phone, string email, string emergencyContact, string gender, string date)
    {

        public string FirstName { get; set; } = firstName;
        public string LastName { get; set; } = lastName;
        public string Phone { get; set; } = phone;
        public string Email { get; set; } = email;
        public string Address { get; set; } = address;
        public string EmergencyContact { get; set; } = emergencyContact;
        public string Gender { get; set; } = gender;
        public string DateBirth { get; set; } = date;
    }
}
