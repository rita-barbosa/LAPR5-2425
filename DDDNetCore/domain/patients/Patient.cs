using System;
using DDDNetCore.Domain.Shared;

namespace DDDNetCore.Domain.Patients
{
    public class Patient : Entity<MedicalRecordNumber>, IAggregateRoot
    {

        public Name Name { get; private set; }
        public Gender Gender { get; private set; }
        public Phone PhoneNumber { get; private set; }
        public Phone EmergencyContact { get; private set; }
        public Email Email { get; private set; }
        public MedicalCondition? MedicalCondition { get; private set; }
        public DateTime DateBirth { get; private set; }

        private Patient() { }
        public Patient(string firstName, string lastName, string fullName, Gender gender,
                        string countryCode, string phoneNumber, string emergencyContact, string email,
                         string dateBirth, string seqNumber)
        {
            this.Id = new MedicalRecordNumber(seqNumber, true);
            Name = new Name(firstName, lastName, fullName);
            if (string.Equals(phoneNumber, emergencyContact))
            {
                throw new BusinessRuleValidationException("The patients phone number can't be the same as the emergency contact.");
            }
            PhoneNumber = new Phone(countryCode, phoneNumber);
            EmergencyContact = new Phone(countryCode, emergencyContact);
            Email = new Email(email);
            if (!DateTime.TryParse(dateBirth, out DateTime dateOfBirth) || dateOfBirth > DateTime.Now)
            {
                throw new BusinessRuleValidationException("The Date of Birth is either in an incorrect format or cannot be in the future. Please provide a valid past date.");
            }
            DateBirth = dateOfBirth;
            Gender = gender;
        }

        public Patient(string firstName, string lastName, Gender gender,
                     string phoneNumber, string emergencyContact, string email,
                      string dateBirth, string seqNumber)
        {
            this.Id = new MedicalRecordNumber(seqNumber, true);
            Name = new Name(firstName, lastName);
            if (string.Equals(phoneNumber, emergencyContact))
            {
                throw new BusinessRuleValidationException("The patients phone number can't be the same as the emergency contact.");
            }
            PhoneNumber = new Phone(phoneNumber);
            EmergencyContact = new Phone(emergencyContact);
            Email = new Email(email);
            if (!DateTime.TryParse(dateBirth, out DateTime dateOfBirth) || dateOfBirth > DateTime.Now)
            {
                throw new BusinessRuleValidationException("The Date of Birth is either in an incorrect format or cannot be in the future. Please provide a valid past date.");
            }
            DateBirth = dateOfBirth;
            Gender = gender;
        }
    }
}