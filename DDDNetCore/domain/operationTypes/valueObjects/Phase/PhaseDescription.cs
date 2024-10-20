using System;
using DDDNetCore.Domain.Shared;

namespace DDDNetCore.Domain.OperationTypes.ValueObjects.Phase
{
    public class PhaseDescription : IValueObject
    {

        public string Description { get;  private set; }

        public PhaseDescription(){
            // for EF Core
        }

        public PhaseDescription(string description)
        {
            if (string.IsNullOrEmpty(description))
            {
                throw new ArgumentException("Operation Type Description cannot be null or empty.");
            }
            this.Description = description;
        }

        public override bool Equals(object obj)
        {
            if (obj == null || GetType() != obj.GetType())
            {
                return false;
            }

            var other = (PhaseDescription)obj;
            return Description == other.Description;
        }

        public override int GetHashCode()
        {
            return HashCode.Combine(Description);
        }

    }
}