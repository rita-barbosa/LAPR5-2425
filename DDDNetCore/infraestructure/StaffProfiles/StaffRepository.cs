using DDDNetCore.Domain.Staff;
using DDDNetCore.Infrastructure.Shared;

namespace DDDNetCore.Infrastructure.StaffProfiles
{
    public class StaffRepository : BaseRepository<Staff, StaffId>, IStaffRepository
    {
        public StaffRepository(DDDNetCoreDbContext context) : base(context.StaffProfiles)
        {

        }
    }
}