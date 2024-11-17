using System.Threading.Tasks;
using MDBackoffice.Domain.OperationRequests;
using MDBackoffice.Domain.Rooms;
using MDBackoffice.Domain.Shared;

namespace MDBackoffice.Domain.Appointments
{
    public class AppointmentService
    {
        private readonly IUnitOfWork _unitOfWork;
        private readonly IAppointmentRepository _repo;
        private readonly IOperationRequestRepository _repoOpReq;
        private readonly IRoomRepository _repoRoom;

        public AppointmentService(IUnitOfWork unitOfWork, IAppointmentRepository repo, IOperationRequestRepository repoOpReq, IRoomRepository repoRoom)
        {
            _unitOfWork = unitOfWork;
            _repo = repo;
            _repoOpReq = repoOpReq;
            _repoRoom = repoRoom;
        }

        public virtual async Task<AppointmentDto> AddAsync(CreatingAppointmentDto dto)
        {
            OperationRequest operationRequest = await this._repoOpReq.GetByIdAsync(new OperationRequestId(dto.OperationRequestId)) ??
                throw new BusinessRuleValidationException("Operation Request is invalid.");

            Room room = await this._repoRoom.GetByIdAsync(new RoomNumber(dto.RoomNumber)) ??
                throw new BusinessRuleValidationException("Room is invalid");


            var appointment = new Appointment(
                dto.Status,
                operationRequest.Id,
                room.Id.AsString(),
                dto.StartTime,
                dto.EndTime,
                dto.StartDate,
                dto.EndDate
            );

            await _repo.AddAsync(appointment);
            await _unitOfWork.CommitAsync();

            return new AppointmentDto(appointment.Id.AsGuid(), appointment.Status.Description.ToString(), appointment.OperationRequestId.Value, appointment.RoomNumber.AsString(),
                appointment.Slot.TimeInterval.Start.ToString(), appointment.Slot.TimeInterval.End.ToString(), appointment.Slot.Date.Start.ToString(), appointment.Slot.Date.End.ToString()
            );
        }

    }
}