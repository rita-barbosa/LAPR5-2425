using System;
using System.Linq;
using System.Threading.Tasks;
using DDDNetCore.Domain.StaffProfiles;
using DDDNetCore.Infrastructure.Shared;
using Microsoft.EntityFrameworkCore;

namespace DDDNetCore.Infrastructure.StaffProfiles
{
    public class StaffRepository : BaseRepository<Staff, StaffId>, IStaffRepository
    {
        private readonly DDDNetCoreDbContext _context;
        public StaffRepository(DDDNetCoreDbContext context) : base(context.StaffProfiles)
        {
            _context = context;
        }
        public async Task<bool> ExistsStaffWithEmailOrPhone(string email, string countryCode, string phone)
        {
            return await _context.StaffProfiles
              .AnyAsync(staff =>
                  (staff.Email != null && staff.Email.EmailAddress == email) ||
                  (staff.Phone != null &&
                   staff.Phone.CountryCode == countryCode &&
                   staff.Phone.PhoneNumber == phone));

        }

        public async Task<StaffId> FindLastStaffIdAsync()
        {
            var staffList = await _context.StaffProfiles.ToListAsync(); // Load all staff profiles into memory

            var lastStaff = staffList
                .OrderByDescending(staff => staff.Id.AsString().Substring(5)) // Now we can safely use AsString and Substring
                .FirstOrDefault() ?? throw new NullReferenceException();

            return lastStaff.Id;
        }
    }
}