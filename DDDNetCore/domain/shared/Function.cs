using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations.Schema;

namespace DDDNetCore.Domain.Shared
{
    public class Function : IValueObject
    {
        public string Description { get; }

        [NotMapped]
        public static Function Intern { get; } = new Function("intern");
        [NotMapped]
        public static Function Doctor { get; } = new Function("doctor");
        [NotMapped]
        public static Function Nurse { get; } = new Function("nurse");
        [NotMapped]
        public static Function Assistant { get; } = new Function("assistant");

        private static readonly Dictionary<string, Function> _functions = new Dictionary<string, Function>(StringComparer.OrdinalIgnoreCase)
        {
            { Intern.Description, Intern },
            { Doctor.Description, Doctor },
            { Nurse.Description, Nurse },
            { Assistant.Description, Assistant }
        };

        public Function()
        {
            // for ORM
        }

        private Function(string description)
        {
            if (string.IsNullOrEmpty(description))
            {
                throw new BusinessRuleValidationException("Function description cannot be null or empty.");
            }
            Description = description;
        }

        public static Function? GetFunctionByDescription(string description)
        {
            if (string.IsNullOrEmpty(description))
            {
                return null;
            }

            _functions.TryGetValue(description, out var matchingFunction);
            return new Function(matchingFunction.Description);
        }
        public string GetCorrespondingChar()
        {
            return Description.ToLower() switch
            {
                "doctor" => "D",
                "nurse" => "N",
                _ => "O" // For all other types
            };
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