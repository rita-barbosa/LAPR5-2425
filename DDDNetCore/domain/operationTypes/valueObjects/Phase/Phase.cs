using System;
using DDDNetCore.Domain.Shared;

namespace DDDNetCore.Domain.OperationTypes.ValueObjects.Phase
{
    public class Phase : IValueObject
    {

        public PhaseDescription Description { get;  private set; }

        public PhaseDuration Duration { get;  private set; }


        public Phase(){
            // for EF Core
        }


        public Phase(string description, int durationMinutes)
        {
            this.Description = new PhaseDescription(description);
            this.Duration = new PhaseDuration(durationMinutes);
        }

        public override bool Equals(object obj)
        {
            if (obj == null || GetType() != obj.GetType())
            {
                return false;
            }

            var other = (Phase)obj;
            return Description == other.Description && Duration == other.Duration;
        }

        public override int GetHashCode()
        {
            return HashCode.Combine(Description, Duration);
        }
    }
}