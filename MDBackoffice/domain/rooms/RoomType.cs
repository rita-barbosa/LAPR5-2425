using System;
using MDBackoffice.Domain.Shared;

namespace MDBackoffice.Domain.Rooms
{
    public class RoomType : IValueObject
    {
        public string RoomTypeName { get; private set; }

        public RoomType(){

        }

        public RoomType(string name)
        {
            if(string.IsNullOrEmpty(name)){
                throw new BusinessRuleValidationException("The Room Type cannot be null or empty.");
            }
            this.RoomTypeName = name;
        }

        public override bool Equals(object obj)
        {
            if (obj == null || GetType() != obj.GetType())
            {
                return false;
            }

            var other = (RoomType)obj;
            return RoomTypeName == other.RoomTypeName;
        }

        public override int GetHashCode()
        {
            return HashCode.Combine(RoomTypeName);
        }
    }
}