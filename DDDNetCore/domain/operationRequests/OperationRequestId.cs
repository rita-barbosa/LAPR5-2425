using System;
using DDDNetCore.Domain.Shared;
using Newtonsoft.Json;

namespace DDDNetCore.Domain.OperationRequest
{
    public class OperationRequestId : EntityId
    {
        [JsonConstructor]
        public OperationRequestId(Guid value) : base(value)
        {
        }
        public OperationRequestId(String value):base(value)
        {
            if (string.IsNullOrEmpty(value))
            {
                throw new ArgumentException("Operation Request Id cannot be null or empty.");
            }
        }

        override
        protected Object createFromString(String text){
            return text;
        }
        override
        public String AsString(){
            return (String) base.Value;
        }
        public Guid AsGuid(){
            return (Guid) base.ObjValue;
        }
    }
}