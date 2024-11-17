using System.Threading.Tasks;
using MDBackoffice.Domain.Shared;

namespace MDBackoffice.Domain.Rooms
{
    public class RoomService
    {
        private readonly IUnitOfWork _unitOfWork;
        private readonly IRoomRepository _repo;

        public RoomService(IUnitOfWork unitOfWork, IRoomRepository repo)
        {
            _unitOfWork = unitOfWork;
            _repo = repo;
        }

        public virtual async Task<RoomDto> AddAsync(RoomDto dto)
        {
            var room = new Room(
                dto.RoomNumber,
                dto.Type,
                dto.Capacity,
                dto.CurrentStatus
            );

            if (dto.AvailableEquipment != null)
            {
                foreach (var equipment in dto.AvailableEquipment)
                {
                    room.AddEquipment(equipment);
                }
            }

            if (dto.MaintenanceSlots != null)
            {
                foreach (var slotDto in dto.MaintenanceSlots)
                {
                    room.AddSlot(slotDto.StartTime, slotDto.EndTime, slotDto.StartDate, slotDto.EndDate);
                }
            }

            await _repo.AddAsync(room);
            await _unitOfWork.CommitAsync();

            return new RoomDto(
                room.Id.AsString(),
                room.Type.RoomTypeName,
                room.Capacity.CapcityNumber,
                room.AvailableEquipment.ConvertAll(e => e.EquipmentName.ToString()),
                room.CurrentStatus.Description,
                room.MaintenanceSlots.ConvertAll(slot => new SlotsDto(
                    slot.TimeInterval.Start.ToString(),
                    slot.TimeInterval.End.ToString(),
                    slot.Date.Start.ToString(),
                    slot.Date.End.ToString()
                ))
            );
        }

    }
}