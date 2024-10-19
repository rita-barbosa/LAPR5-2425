using DDDNetCore.Domain.Shared;

namespace DDDNetCore.Domain.Specializations
{
    public class Specialization : Entity<SpecializationId>, IAggregateRoot
    {

        public SpecializationDenomination Denomination { get; private set; }

        public Specialization(string denomination)
        {
            this.Id = new SpecializationId(denomination);
            this.Denomination = new SpecializationDenomination(denomination);
        }

        public void ChangeDenomination(string denomination)
        {
            this.Denomination = new SpecializationDenomination(denomination);
        }

    }
}