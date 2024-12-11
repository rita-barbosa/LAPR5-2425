using System.Collections.Generic;
using MDBackoffice.Domain.OperationTypes.ValueObjects.RequiredStaff;
using MDBackoffice.Domain.Shared;

namespace MDBackoffice.Domain.Specializations
{
    public class Specialization : Entity<SpecializationCode>, IAggregateRoot
    {
        public SpecializationDenomination Denomination { get; }
        public SpecializationDescription Description { get; }

        public Specialization()
        {
            //for EF Core
        }

        public Specialization(string code, string denomination, string description)
        {
            this.Id = new SpecializationCode(code);
            this.Denomination = new SpecializationDenomination(denomination);
            this.Description = new SpecializationDescription(description);
        }
    }
}