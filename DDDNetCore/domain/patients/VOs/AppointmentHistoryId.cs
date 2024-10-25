using System;
using DDDNetCore.Domain.Shared;

namespace DDDNetCore.Domain.Patients
{
    public class AppointmentHistoryId : EntityId
    {

        public AppointmentHistoryId(String value, bool v) :base(value)
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