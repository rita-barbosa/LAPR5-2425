using System;
using DDDNetCore.Domain.Shared;

namespace DDDNetCore.Domain.Patients
{

    public class MedicalCondition : IValueObject
    {
        public string Description { get; }

        public MedicalCondition(string description)
        {
            if (string.IsNullOrEmpty(description))
            {
                throw new ArgumentException("Gender denomination cannot be null or empty");
            }
            Description = description;
        }
    }
}