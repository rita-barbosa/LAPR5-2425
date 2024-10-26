
using System;

namespace DDDNetCore.Domain.Shared
{
    public class Slot : IValueObject
    {
        public Date Date { get; }
        public TimeInterval TimeInterval { get; }

        private Slot() { }
        public Slot(string startTime, string endTime, string startDate, string endDate = null)
        {
            Date = new Date(startDate, endDate);
            bool moreDays = false;
            if(endDate != null || startDate.Equals(endDate)){
                moreDays = true;
            }
            TimeInterval = new TimeInterval(startTime, endTime, moreDays);
        }

        public override string ToString() => $"{Date}:{TimeInterval}";

        public override bool Equals(object obj)
        {
            if (obj is Slot other)
            {
                return Date.Equals(other.Date) &&
                       TimeInterval.Equals(other.TimeInterval);
            }
            return false;
        }

        public override int GetHashCode()
        {
            return HashCode.Combine(Date, TimeInterval);
        }
    }
}