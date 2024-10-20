using System;
using DDDNetCore.Domain.Shared;

namespace DDDNetCore.Domain.OperationTypes.ValueObjects.RequiredStaff
{
    public class NumberStaff : IValueObject
    {

        public int NumberRequired { get;  private set; }

        public NumberStaff(){
            // for ORM
        }

        public NumberStaff(int staffRequired)
        {
            if (staffRequired < 0)
            {
                throw new ArgumentException("The number of required staff of the operation type cannot be negative.");
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