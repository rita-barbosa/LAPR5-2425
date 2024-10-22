using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations.Schema;
using DDDNetCore.Domain.Shared;

namespace DDDNetCore.Domain.OperationRequest
{
    public class  Priority : IValueObject
    {
        public string Name { get; }


        [NotMapped]
        public static Priority Elective { get; } = new Priority("Elective");
        [NotMapped]
        public static Priority Urgent { get; } = new Priority("Urgent");
        [NotMapped]
        public static Priority Emergency { get; } = new Priority("Emergency");

        private static readonly Dictionary<string, Priority> _prioritys = new Dictionary<string, Priority>(StringComparer.OrdinalIgnoreCase)
        {
            { Elective.Name, Elective },
            { Urgent.Name, Urgent },
            { Emergency.Name, Emergency }
        };

        public Priority(){
            // for ORM  
        }

        private Priority(string name)
        {
            if (string.IsNullOrEmpty(name))
            {
                throw new ArgumentException("Priority cannot be null or empty");
            }
            Name = name;
        }
        
        public static Priority? GetPriorityByName(string name)
        {
            if (string.IsNullOrEmpty(name))
            {
                return null;
            }

            _prioritys.TryGetValue(name, out var matchingPriority);
            return new Priority(matchingPriority.Name);
        }

        public override bool Equals(object obj)
        {
            if (obj == null || GetType() != obj.GetType())
            {
                return false;
            }

            var other = (Priority)obj;
            return Name == other.Name;
        }

        public override int GetHashCode()
        {
            return  Name.GetHashCode();
        }

        public override string ToString()
        {
            return Name;
        }

    }
}