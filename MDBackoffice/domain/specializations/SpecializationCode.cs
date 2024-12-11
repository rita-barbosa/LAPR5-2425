using System;
using MDBackoffice.Domain.Shared;

namespace MDBackoffice.Domain.Specializations
{
    public class SpecializationCode : EntityId
    {

        public SpecializationCode(string code) : base(code)
        {
            if (string.IsNullOrEmpty(code) || string.IsNullOrWhiteSpace(code))
            {
                throw new BusinessRuleValidationException("Specializations must have a code");
            }
        }

        protected override object createFromString(string text)
        {
            return text;
        }
        
        public override string AsString()
        {
            // Return the value as a string
            return Value;
        }
    }
}
