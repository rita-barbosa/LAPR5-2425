using System;
using DDDNetCore.Domain.Shared;

namespace DDDNetCore.Domain.OperationRequest
{
    public class  Priority : IValueObject
    {
        public string Name { get; }
        public Priority(string name)
        {
            if (string.IsNullOrEmpty(name))
            {
                throw new ArgumentException("Priority cannot be null or empty");
            }
            Name = name;
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
            return "Priority: " + Name;
        }

    }
}