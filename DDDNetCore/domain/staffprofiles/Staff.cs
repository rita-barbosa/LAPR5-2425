using System.Collections.Generic;
using DDDNetCore.Domain.Shared;
using DDDNetCore.Domain.Specializations;

namespace DDDNetCore.Domain.Staff
{
    public class Staff : Entity<StaffId>, IAggregateRoot
    {
        public LicenseNumber LicenseNumber { get; private set; }
        public Name Name { get; private set; }
        //public ContactInfo ContactInfo { get; private set; }
        public Phone Phone { get; private set; }
        public Email Email { get; private set; }
        public List<Slot> Slots { get; set; }
        public Function Function { get; private set; }
        public SpecializationDenomination SpecializationId { get; private set; }

        private Staff() { }
        public Staff(string seqNumber, string licenseNumber, string firstName, string lastName, string email, string phoneNumber, Function function, SpecializationDenomination specializationId)
        {
            this.Id = new StaffId(function switch
            {
                var f when f == Function.Doctor => "D",
                var f when f == Function.Nurse => "N",
                _ => "O"
            }, seqNumber);

            LicenseNumber = new LicenseNumber(licenseNumber);
            Name = new Name(firstName, lastName);
            //ContactInfo = new ContactInfo(email, phoneNumber);
            Phone = new Phone(phoneNumber);
            Email = new Email(email);
            Slots = [];
            Function = function;
            SpecializationId = specializationId ?? throw new BusinessRuleValidationException("Staff members must have a specialization.");
        }
        public Staff(string seqNumber, string licenseNumber, string firstName, string lastName, string fullName, string email, string countryCode, string phoneNumber, Function function, SpecializationDenomination specializationId)
        {
            this.Id = new StaffId(function switch
            {
                var f when f == Function.Doctor => "D",
                var f when f == Function.Nurse => "N",
                _ => "O"
            }, seqNumber);

            LicenseNumber = new LicenseNumber(licenseNumber);
            Name = new Name(firstName, lastName, fullName);
            //ContactInfo = new ContactInfo(email, countryCode, phoneNumber);
            Phone = new Phone(countryCode, phoneNumber);
            Email = new Email(email);
            Slots = [];
            Function = function;
            SpecializationId = specializationId ?? throw new BusinessRuleValidationException("Staff members must have a specialization.");
        }

        public void AddSlot(string startTime, string endTime, string startDate, string endDate = null)
        {
            var slot = new Slot(startTime, endTime, startDate, endDate);
            Slots.Add(slot);
        }
    }
}