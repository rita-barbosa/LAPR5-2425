using System;
using DDDNetCore.Domain.Shared;

namespace DDDNetCore.Domain.OperationRequest
{
    public class OperationRequestStatus : IValueObject
    {

        public string StatusName{ get;  private set; }

        public OperationRequestStatus() 
        {

        }

        public OperationRequestStatus (string status)
        {
            if (string.IsNullOrWhiteSpace(status))
            {
                throw new ArgumentException("Status cannot be null or empty.");
            }
            this.StatusName = status;
        }

        public override bool Equals(object obj)
        {
            if (obj == null || GetType() != obj.GetType())
            {
                return false;
            }

            var other = (Status)obj;
            return StatusName == other.StatusName;
        }

        public override int GetHashCode()
        {
            return HashCode.Combine(StatusName);
        }

        public override string ToString()
        {
            return "Status: " + StatusName;
        }

    }
}