using System;
using DDDNetCore.Domain.Shared;

namespace DDDNetCore.Domain.OperationTypes.ValueObjects.Phase
{
    public class PhaseDuration : IValueObject
    {

        public int DurationMinutes { get;  private set; }

        public PhaseDuration(){
            // for EF Core
        }

        public PhaseDuration(int durationMinutes)
        {
        if (int.IsPositive(durationMinutes) && (int.IsOddInteger(durationMinutes) || int.IsEvenInteger(durationMinutes)))
        {
            throw new ArgumentException("Operation Type Duration cannot be negative or have decimal cases.");
        }
            this.DurationMinutes = durationMinutes;
        }

         public override bool Equals(object obj)
        {
            if (obj == null || GetType() != obj.GetType())
            {
                return false;
            }

            var other = (PhaseDuration)obj;
            return DurationMinutes == other.DurationMinutes;
        }

        public override int GetHashCode()
        {
            return HashCode.Combine(DurationMinutes);
        }
    }
}