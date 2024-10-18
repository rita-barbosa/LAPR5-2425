using DDDNetCore.Domain.Shared;

namespace DDDNetCore.Domain.Specializations
{
    public class Specialization : Entity<SpecializationDenomination>, IAggregateRoot
    {

        public bool Active{ get;  private set; }

        public Specialization(string denomination)
        {
            this.Id = new SpecializationDenomination(denomination);
            this.Active = true;
        }

        public void ChangeDenomination(string denomination)
        {
            if (!this.Active)
            // TODO:
            //rever isto
                throw new BusinessRuleValidationException("It is not possible to change the denomination to an inactive operation type.");
            this.Id = new SpecializationDenomination(denomination);
        }
        public void MarkAsInative()
        {
            this.Active = false;
        }
    }
}