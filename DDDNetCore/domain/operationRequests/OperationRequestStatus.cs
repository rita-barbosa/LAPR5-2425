using System;
using DDDNetCore.Domain.Shared;

namespace DDDNetCore.Domain.OperationRequest
{
    public class OperationRequestStatus : IValueObject
    {

        public OperationRequestStatusEnum Status { get; private set; }

        public OperationRequestStatus() 
        {
        }

        public OperationRequestStatus(OperationRequestStatusEnum status)
        {
            this.Status = status;
        }

        public OperationRequestStatus(string status)
        {
            if (Enum.TryParse<OperationRequestStatusEnum>(status, true, out var result))
            {
                this.Status = result;
            }
            else
            {
                throw new BusinessRuleValidationException($"Invalid status: {status}");
            }
        }

        public override bool Equals(object obj)
        {
            if (obj == null || GetType() != obj.GetType())
            {
                return false;
            }

            var other = (OperationRequestStatus)obj;
            return Status == other.Status;
        }

        public override int GetHashCode()
        {
            return HashCode.Combine(Status);
        }

        public override string ToString()
        {
            return Status.ToString();
        }

    }
}