using System.Collections.Generic;
using System.Threading.Tasks;
using MDBackoffice.Domain.Shared;

namespace MDBackoffice.Domain.Rooms
{
    public interface IRoomRepository : IRepository<Room, RoomNumber>
    {
        Task<List<Room>>  GetAllRoomsAsync();
    }
}