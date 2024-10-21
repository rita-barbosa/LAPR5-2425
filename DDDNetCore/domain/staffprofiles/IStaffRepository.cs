using System.Threading.Tasks;
using System.Threading.Tasks;
using DDDNetCore.Domain.Shared;

namespace DDDNetCore.Domain.StaffProfiles
{
    public interface IStaffRepository : IRepository<Staff, StaffId>
    {
        Task<bool> ExistsStaffWithEmailOrPhone(string email, string CountryCode, string phone);
        Task<StaffId> FindLastStaffIdAsync();
        Task<Staff> GetStaffWithEmail(string email);
    }
}