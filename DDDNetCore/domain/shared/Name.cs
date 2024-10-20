using System;

namespace DDDNetCore.Domain.Shared
{
    public class Name : IValueObject
    {
        public string FirstName { get; }
        public string LastName { get; }
        public string FullName { get; }
        public Name(string firstName, string lastName)
        {
            if (string.IsNullOrEmpty(firstName) || string.IsNullOrEmpty(lastName))
            {
                throw new BusinessRuleValidationException("Name cannot be null or empty.");
            }
            this.FirstName = firstName;
            this.LastName = lastName;
            this.FullName = $"{firstName} {lastName}";
        }
        public Name(string firstName, string lastName, string fullName)
        {
            if (string.IsNullOrEmpty(firstName) || string.IsNullOrEmpty(lastName) || string.IsNullOrEmpty(fullName))
            {
                throw new BusinessRuleValidationException("Name cannot be null or empty.");
            }
            this.FirstName = firstName;
            this.LastName = lastName;
            this.FullName = fullName;
        }
        public override bool Equals(object obj)
        {
            if (obj == null || GetType() != obj.GetType())
            {
                return false;
            }

            var other = (Name)obj;
            return FirstName == other.FirstName && LastName == other.LastName && FullName == other.FullName;
        }

        public override int GetHashCode()
        {
            return HashCode.Combine(FirstName, LastName, FullName);
        }

        public override string ToString()
        {
            return FullName;
        }
    }
}