using System.Collections.Generic;
using DDDNetCore.Domain.OperationTypes.ValueObjects.RequiredStaff;
using DDDNetCore.Domain.OperationTypesRecords;
using DDDNetCore.Domain.Shared;

namespace DDDNetCore.Domain.Specializations
{
    public class Specialization : Entity<SpecializationDenomination>, IAggregateRoot
    {

        public ICollection<RequiredStaff> RequiredStaff { get; set; }
        public ICollection<RequiredStaffRecord> RequiredStaffRecords { get; set; }

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