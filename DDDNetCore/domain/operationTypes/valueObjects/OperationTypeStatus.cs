using System;
using DDDNetCore.Domain.Shared;
using Newtonsoft.Json.Linq;

namespace DDDNetCore.Domain.OperationTypes.ValueObjects
{
    public class OperationTypeStatus : IValueObject
    {

        public bool Active{ get;  private set; }

        public OperationTypeStatus(){
            // for EF Core
        }

        public OperationTypeStatus(bool status)
        {
            if (status == false)
            {
                throw new BusinessRuleValidationException("Status cannot be false.");
            }
            this.Active = status;
        }

        public override bool Equals(object obj)
        {
            if (obj == null || GetType() != obj.GetType())
            {
                return false;
            }

            var other = (OperationTypeStatus)obj;
            return Active == other.Active;
        }

        public override int GetHashCode()
        {
            return HashCode.Combine(Active);
        }

        public string AsString()
        {
            return Active ? "true" : "false";
        }

    }
}