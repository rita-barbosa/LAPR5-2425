using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using MDBackoffice.Domain.Shared;

namespace MDBackoffice.Domain.RoomTypes
{
    public class RoomTypeService
    {
        private readonly IUnitOfWork _unitOfWork;
        private readonly IRoomTypeRepository _repo;

        public RoomTypeService(IUnitOfWork unitOfWork, IRoomTypeRepository repo)
        {
            _unitOfWork = unitOfWork;
            _repo = repo;
        }

        public async Task<RoomTypeDto> AddAsync(RoomTypeDto dto)
        {
            var roomType = new RoomType(dto.Code, dto.Designation, dto.Description);
            await _repo.AddAsync(roomType);
            await _unitOfWork.CommitAsync();

            return new RoomTypeDto(roomType.Id.Value, roomType.Designation.Designation, roomType.Description.Description);
        }



    }
}