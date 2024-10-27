using System.Collections.Generic;
using MDBackoffice.Domain.OperationTypes.ValueObjects.RequiredStaff;
using MDBackoffice.Domain.Shared;

namespace MDBackoffice.Domain.Specializations
{
    public class Specialization : Entity<SpecializationDenomination>, IAggregateRoot
    {

        public ICollection<RequiredStaff> RequiredStaff { get; set; }

        public Specialization(){
            //for EF Core
        }

        public Specialization(string denomination)
        {
            this.Id = new SpecializationDenomination(denomination);
        }

        public void ChangeDenomination(string denomination)
        {
            this.Id = new SpecializationDenomination(denomination);
        }

    }
}