using System;
using System.Collections.Generic;
using DDDNetCore.Domain.Shared;
using DDDNetCore.Domain.Specializations;
using DDDNetCore.Domain.Users;
using Microsoft.EntityFrameworkCore.ChangeTracking.Internal;
using Org.BouncyCastle.Tls;

namespace DDDNetCore.Domain.Logs
{
    public class Change : IValueObject
    {
        public ChangeType Type { get; set; }
        public string ChangeDescription { get; set; }

        private Change() { }
        public Change(ChangeType type, string changeDescription)
        {
            Type = type;
            ChangeDescription = changeDescription;
        }

        
    }
}