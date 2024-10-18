using System;
using DDDNetCore.Domain.Shared;

namespace DDDNetCore.Domain.Specializations{
    public class SpecializationDenomination : EntityId, IValueObject
    {

       public string Name {get; set; }

        public SpecializationDenomination(String value):base(value)
        {
            this.Name = value;
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

