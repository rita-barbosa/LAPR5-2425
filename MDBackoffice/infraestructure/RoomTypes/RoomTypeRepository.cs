using System.Collections.Generic;
using System.Threading.Tasks;
using MDBackoffice.Domain.RoomTypes;
using MDBackoffice.Infrastructure.Shared;
using Microsoft.EntityFrameworkCore;

namespace MDBackoffice.Infrastructure.RoomTypes
{
    public class RoomTypeRepository : BaseRepository<RoomType, RoomTypeCode>, IRoomTypeRepository
    {
        private readonly MDBackofficeDbContext _context;
        public RoomTypeRepository(MDBackofficeDbContext context) : base(context.RoomTypes)
        {
            _context = context;
        }

        public Task<List<RoomType>> GetAllRoomTypesAsync()
        {
            throw new System.NotImplementedException();
        }
    }
}