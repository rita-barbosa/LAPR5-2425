
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
            TimeInterval = new TimeInterval(startTime, endTime);
        }

        public override string ToString() => $"{Date}:{TimeInterval}";
    }
}