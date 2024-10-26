using System;
using DDDNetCore.Domain.Shared;

namespace DDDNetCore.Domain.OperationTypes
{
    public class OperationTypeId : EntityId
    {

        public OperationTypeId(String value) : base(value)
        {
            if (string.IsNullOrEmpty(value))
            {
                throw new BusinessRuleValidationException("Operation name cannot be null or empty.");
            }
        }

        override
        protected Object createFromString(String text)
        {
            return text;
        }
        override
        public String AsString()
        {
            return (String)base.Value;
        }
    }
}