using System;
using DDDNetCore.Domain.Shared;

namespace DDDNetCore.Domain.OperationTypes.ValueObjects
{
    public class EstimatedDuration : IValueObject
    {

        public int TotalDurationMinutes { get;  private set; }

        public EstimatedDuration(int minutes)
        {
        
            if (int.IsPositive(minutes) && (int.IsOddInteger(minutes) || int.IsEvenInteger(minutes)))
            {
                throw new ArgumentException("The estimated duration of the operation type cannot be negative or have decimal cases.");
            }
            this.TotalDurationMinutes = minutes;
        }

        public override bool Equals(object obj)
        {
            if (obj == null || GetType() != obj.GetType())
            {
                return false;
            }

            var other = (EstimatedDuration)obj;
            return TotalDurationMinutes == other.TotalDurationMinutes;
        }

        public override int GetHashCode()
        {
            return HashCode.Combine(TotalDurationMinutes);
        }

    }
}