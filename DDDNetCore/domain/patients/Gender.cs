using System;
using DDDNetCore.Domain.Shared;

namespace DDDNetCore.Domain.Patients
{

    public class Gender : IValueObject
    {
        public string Denomination { get; }

        public static Gender Female { get; } = new Gender("female");
        public static Gender Male { get; } = new Gender("male");

        private Gender(string denomination)
        {
            if (string.IsNullOrEmpty(denomination))
            {
                throw new ArgumentException("Gender denomination cannot be null or empty");
            }
            Denomination = denomination;
        }

        


        public override bool Equals(object obj)
        {
            return obj is Gender gender && Denomination == gender.Denomination;
        }

        public override int GetHashCode()
        {
            return Denomination.GetHashCode();
        }

    }

}