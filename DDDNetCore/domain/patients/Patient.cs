using System;
using DDDNetCore.Domain.Shared;
using DDDNetCore.Domain.Users;

namespace DDDNetCore.Domain.Patients
{
    public class Patient : Entity<MedicalRecordNumber>, IAggregateRoot
    {

        public Name Name { get; private set; }
        public Gender Gender { get; private set; }
        public Phone PhoneNumber { get; private set; }
        public Phone EmergencyContact { get; private set; }
        public Email Email { get; private set; }
        public ResidentialAddress Address { get; private set; }
        public MedicalCondition? MedicalCondition { get; private set; }
        public DateTime DateBirth { get; private set; }
        public string? UserReference { get; set; }

        private Patient() { }
        public Patient(string firstName, string lastName, string fullName, string address, Gender gender,
                        string countryCode, string phoneNumber, string emergencyContact, string email,
                         string dateBirth, string seqNumber)
        {
            this.Id = new MedicalRecordNumber(seqNumber, true);
            Address = new ResidentialAddress(address);
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

        public Patient(string firstName, string lastName, string address, Gender gender,
                     string phoneNumber, string emergencyContact, string email,
                      string dateBirth, string seqNumber)
        {
            this.Id = new MedicalRecordNumber(seqNumber, true);
            Address = new ResidentialAddress(address);
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

        public void AddUser(User user)
        {
            if (user == null)
            {
                throw new ArgumentNullException(nameof(user), "User cannot be null.");
            }

            if (this.UserReference != null)
            {
                throw new InvalidOperationException("This patient already has a user assigned.");
            }

            this.UserReference = user.Id;
        }


        public void ChangeEmail(string email)
        {
            if(this.Email.Equals(new Email(email))) throw new BusinessRuleValidationException("The new email is identical to the existing one.");
            Email = new Email(email);
        }

        public void ChangePhone(string phone)
        {
            if (this.PhoneNumber.Equals(new Phone(phone))) throw new BusinessRuleValidationException("The new phone number is identical to the existing one.");
            PhoneNumber = new Phone(phone);
        }

        public void ChangeAddress(string address)
        {
            if (this.Address.Equals(new ResidentialAddress(address))) throw new BusinessRuleValidationException("The new address is identical to the existing one.");
            Address = new ResidentialAddress(address);
        }

        public void ChangeName(string name)
        {
            if (Name.Equals(new Name(name))) throw new BusinessRuleValidationException("The new name is identical to the existing one.");
            Name = new Name(name);
        }

        internal void ChangeEmergencyContact(string emergencyContact)
        {
            if (EmergencyContact.Equals(new Phone(emergencyContact))) throw new BusinessRuleValidationException("The new emergency contact is identical to the existing one.");
            EmergencyContact = new Phone(emergencyContact);
        }
    }
}