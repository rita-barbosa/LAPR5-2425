using DDDNetCore.Domain.Shared;

namespace DDDNetCore.Domain.Staff
{
    public interface IStaffRepository: IRepository<Staff, StaffId>
    {
    }
}