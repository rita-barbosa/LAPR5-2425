using System;

namespace DDDNetCore.Domain.Shared
{

    public class TimeInterval : IValueObject
    {
        public TimeSpan Start { get; }
        public TimeSpan End { get; }

        private TimeInterval() { }
        public TimeInterval(string startString, string endString)
        {
            if (!TimeSpan.TryParse(startString, out TimeSpan start) || !TimeSpan.TryParse(endString, out TimeSpan end))
                throw new BusinessRuleValidationException("Invalid time format. Use hh:mm.", nameof(startString));

            if (end < start)
                throw new BusinessRuleValidationException("End time must be greater than or equal to start time.");

            Start = start;
            End = end;
        }
        public override string ToString()
        {
            return $"{Start:HH}h{Start:mm}-{End:HH}h{End:mm}";
        }
    }
}