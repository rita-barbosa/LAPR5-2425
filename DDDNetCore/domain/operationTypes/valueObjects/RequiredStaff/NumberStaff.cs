using System;
using DDDNetCore.Domain.Shared;

namespace DDDNetCore.Domain.OperationTypes.ValueObjects.RequiredStaff
{
    public class NumberStaff : IValueObject
    {

        public int NumberRequired { get;  private set; }

        public NumberStaff(int staffRequired)
        {
            if (int.IsPositive(staffRequired) && (int.IsOddInteger(staffRequired) || int.IsEvenInteger(staffRequired)))
            {
                throw new ArgumentException("The number of required staff cannot be negative or have decimal cases.");
            }
            this.NumberRequired = staffRequired;
        }

        public override bool Equals(object obj)
        {
            if (obj == null || GetType() != obj.GetType())
            {
                return false;
            }

            var other = (NumberStaff)obj;
            return NumberRequired == other.NumberRequired;
        }

        public override int GetHashCode()
        {
            return HashCode.Combine(NumberRequired);
        }


    }
}