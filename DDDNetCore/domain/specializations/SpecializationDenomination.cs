using System;
using DDDNetCore.Domain.Shared;

namespace DDDNetCore.Domain.Specializations
{
    public class SpecializationDenomination : EntityId
    {

        public SpecializationDenomination(string denomination) : base(denomination)
        {
            if (string.IsNullOrEmpty(denomination))
            {
                throw new BusinessRuleValidationException("Specializations must have a denomination");
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
