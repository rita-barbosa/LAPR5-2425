using System;
using DDDNetCore.Domain.Shared;

namespace DDDNetCore.Domain.Patients
{
    public class MedicalRecordNumber : EntityId
    {
        public MedicalRecordNumber(string seqNumber)
              : base($"{DateTime.Now.Year}{DateTime.Now.Month}{seqNumber}")
        {
            if (seqNumber.Length != 6 || !int.TryParse(seqNumber, out _))
            {
                throw new ArgumentException("Sequential number must be a 5-digit number.");
            }
        }
        private MedicalRecordNumber(string year, string month, string seqNumber) : base($"{year}{month}{seqNumber}") { }
        protected override object createFromString(string text)
        {
            if (string.IsNullOrEmpty(text) || text.Length < 10)
            {
                throw new ArgumentException("Invalid Medical Record Number format.");
            }

            string year = text[..4];
            string month = text.Substring(4, 2);
            string seqNumber = text[6..];

            if (!int.TryParse(year, out _) || !int.TryParse(month, out _) || !int.TryParse(seqNumber, out _))
            {
                throw new ArgumentException("Invalid year, month, or sequential number format.");
            }

            if (seqNumber.Length != 6)
            {
                throw new ArgumentException("Sequential number must be a 6-digit number.");
            }

            // Now, create a new instance using the valid seqNumber
            return new MedicalRecordNumber(year, month, seqNumber);
        }


        public override string AsString()
        {
            return Value;
        }
    }
}