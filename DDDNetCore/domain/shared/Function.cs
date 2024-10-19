using System;

namespace DDDNetCore.Domain.Shared
{
    public class Function : IValueObject
    {
        public string Description { get; }


        public static Function Intern { get; } = new Function("intern");
        public static Function Doctor { get; } = new Function("doctor");
        public static Function Nurse { get; } = new Function("nurse");
        public static Function Assistant { get; } = new Function("assistant");

        private Function(string description)
        {
            if (string.IsNullOrEmpty(description))
            {
                throw new ArgumentException("Function description cannot be null or empty.");
            }
            Description = description;
        }

        public override bool Equals(object obj)
        {
            return obj is Function function && Description == function.Description;
        }

        public override int GetHashCode()
        {
            return Description.GetHashCode();
        }
    }
}