using DDDNetCore.Domain.Shared;

namespace DDDNetCore.Domain.OperationTypes.ValueObjects.RequiredStaff
{
    public class RequiredStaffDto : Entity<RequiredStaffId>
    {

        public int StaffQuantity { get; set; }
        public string Function { get; set; }
        public string Specialization { get; set; }
       
    }
}