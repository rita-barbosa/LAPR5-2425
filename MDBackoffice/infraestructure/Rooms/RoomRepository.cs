using System.Collections.Generic;
using System.Threading.Tasks;
using MDBackoffice.Domain.Rooms;
using MDBackoffice.Infrastructure.Shared;
using Microsoft.EntityFrameworkCore;

namespace MDBackoffice.Infrastructure.Rooms
{
    public class RoomRepository : BaseRepository<Room, RoomNumber>, IRoomRepository
    {
        private readonly MDBackofficeDbContext _context;
        public RoomRepository(MDBackofficeDbContext context) : base(context.Rooms)
        {
            _context = context;
        }
        
        public async Task<List<Room>> GetAllRoomsAsync()
        {
            return await _context.Rooms
                .ToListAsync();
        }
    }
}