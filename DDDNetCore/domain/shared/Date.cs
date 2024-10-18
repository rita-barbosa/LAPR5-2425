using System;

namespace DDDNetCore.Domain.Shared
{
    public class Date : IValueObject
    {
        public DateTime Start { get; }
        public DateTime End { get; }

        private Date() { }
        public Date(string start, string end = null)
        {
            if (string.IsNullOrWhiteSpace(start))
                throw new ArgumentException("Start date cannot be null or empty.");

            if (!DateTime.TryParse(start, out DateTime startDate))
                throw new ArgumentException("Invalid start date format.", nameof(start));

            End = string.IsNullOrWhiteSpace(end)
                ? startDate
                : (DateTime.TryParse(end, out DateTime endDate) && endDate >= startDate)
                    ? endDate
                    : throw new ArgumentException("End date must be greater than or equal to start date.");

            Start = startDate;
        }

        public override string ToString()
        {
            return Start == End ? $"{Start:yyyy-MM-dd}" : $"{Start:yyyy-MM-dd}/{End:yyyy-MM-dd}";
        }


        public override bool Equals(object obj)
        {
            return obj is Date other && Start == other.Start && End == other.End;
        }

        public override int GetHashCode() { return HashCode.Combine(Start, End); }
    }
}
