using System;
using DDDNetCore.Domain.Shared;

namespace DDDNetCore.Domain.OperationTypes.ValueObjects.RequiredStaff
{
    public class RequiredStaffId : EntityId
    {

        public RequiredStaffId(String value):base(value)
        {
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