using System;
using DDDNetCore.Domain.Shared;

namespace DDDNetCore.Domain.Specializations
{
    public class SpecializationDenomination : IValueObject
    {

        public string Denomination { get;  private set; }

        public SpecializationDenomination(string denomination)
        {
            if (string.IsNullOrEmpty(denomination))
            {
                throw new ArgumentException("Specialization Denomination cannot be null or empty.");
            }
            this.Denomination = denomination;
        }

        public override bool Equals(object obj)
        {
            if (obj == null || GetType() != obj.GetType())
            {
                return false;
            }

            var other = (SpecializationDenomination)obj;
            return Denomination == other.Denomination;
        }

        public override int GetHashCode()
        {
            return HashCode.Combine(Denomination);
        }

    }
}