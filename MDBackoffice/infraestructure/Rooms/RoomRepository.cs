using MDBackoffice.Domain.Rooms;
using MDBackoffice.Infrastructure.Shared;

namespace MDBackoffice.Infrastructure.Rooms
{
    public class RoomRepository : BaseRepository<Room, RoomNumber>, IRoomRepository
    {
        private readonly MDBackofficeDbContext _context;
        public RoomRepository(MDBackofficeDbContext context) : base(context.Rooms)
        {
            _context = context;
        }
    }
}