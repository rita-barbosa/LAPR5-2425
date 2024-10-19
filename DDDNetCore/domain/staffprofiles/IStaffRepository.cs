using DDDNetCore.Domain.Shared;

namespace DDDNetCore.Domain.StaffProfiles
{
    public interface IStaffRepository: IRepository<Staff, StaffId>
    {
    }
}