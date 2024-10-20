using System;
using DDDNetCore.Domain.Shared;

namespace DDDNetCore.Domain.OperationTypes.ValueObjects
{
    public class EstimatedDuration : IValueObject
    {

        public int TotalDurationMinutes { get;  private set; }

        public EstimatedDuration(){
            //for ORM
        }

        public EstimatedDuration(int minutes)
        {
        
            if (minutes < 0)
            {
                throw new ArgumentException("The estimated duration of the operation type cannot be negative.");
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