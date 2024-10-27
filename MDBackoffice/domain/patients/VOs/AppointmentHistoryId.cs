using System;
using MDBackoffice.Domain.Shared;

namespace MDBackoffice.Domain.Patients
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