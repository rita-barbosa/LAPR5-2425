using System;
using DDDNetCore.Domain.Shared;

namespace DDDNetCore.Domain.Specializations
{
    public class SpecializationId : EntityId
    {

        public SpecializationId(String value):base(value)
        {
            if (string.IsNullOrEmpty(value))
            {
                throw new ArgumentException("Specialization Denomination cannot be null or empty.");
            }
        }

        override
        protected  Object createFromString(String text){
            return text;
        }
        override
        public String AsString(){
            return (String) base.Value;
        }
    }
}