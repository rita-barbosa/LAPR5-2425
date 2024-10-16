using System;
using DDDNetCore.Domain.Shared;
using Newtonsoft.Json;

namespace DDDNetCore.Domain.Tokens
{
    public class TokenTypeId : EntityId
    {

        [JsonConstructor]
        public TokenTypeId(Guid value) : base(value)
        {
        }

        public TokenTypeId(String value) : base(value)
        {
        }

        override
        protected  Object createFromString(String text){
            return new Guid(text);
        }

        override
        public String AsString(){
            Guid obj = (Guid) base.ObjValue;
            return obj.ToString();
        }
        
       
        public Guid AsGuid(){
            return (Guid) base.ObjValue;
        }
    }
}