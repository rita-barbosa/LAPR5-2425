using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using MDBackoffice.Domain.AppointmentStaffs;
using MDBackoffice.Domain.OperationRequests;
using MDBackoffice.Domain.OperationTypes;
using MDBackoffice.Domain.OperationTypes.ValueObjects.RequiredStaff;
using MDBackoffice.Domain.Rooms;
using MDBackoffice.Domain.Shared;
using MDBackoffice.Domain.Specializations;
using MDBackoffice.Domain.StaffProfiles;
using Microsoft.AspNetCore.Mvc;

namespace MDBackoffice.Domain.Appointments
{
    public class AppointmentService
    {
        private readonly IUnitOfWork _unitOfWork;
        private readonly IAppointmentRepository _repo;
        private readonly IOperationRequestRepository _repoOpReq;
        private readonly IRoomRepository _repoRoom;
        private readonly IOperationTypeRepository _repoOpType;
        private readonly IStaffRepository _repoSta;
        private readonly IRequiredStaffRepository _repoReqSta;
        private readonly IAppointmentStaffRepository _appointmentStaffRepo;

        public AppointmentService(IUnitOfWork unitOfWork, IAppointmentRepository repo, IOperationRequestRepository repoOpReq, IRoomRepository repoRoom, IOperationTypeRepository repoOpType,
                                IStaffRepository repoSta, IRequiredStaffRepository repoReqSta, IAppointmentStaffRepository appointmentStaffRepo)
        {
            _unitOfWork = unitOfWork;
            _repo = repo;
            _repoOpReq = repoOpReq;
            _repoRoom = repoRoom;
            _repoOpType = repoOpType;
            _repoSta = repoSta;
            _repoReqSta = repoReqSta;
            _appointmentStaffRepo = appointmentStaffRepo;
        }

        public virtual async Task<AppointmentDto> AddAsync(CreatingAppointmentDto dto)
        {
            OperationRequest operationRequest = await this._repoOpReq.GetByIdAsync(new OperationRequestId(dto.OperationRequestId)) ??
                throw new BusinessRuleValidationException("Operation Request is invalid.");

            OperationType operationType = await this._repoOpType.GetByIdAsync(new OperationTypeId(operationRequest.OperationTypeId.AsString())) ??
                throw new BusinessRuleValidationException("Operation Type is invalid.");

            List<RequiredStaff> requiredStaffList = await this._repoOpType.GetRequiredStaffByOperationTypeIdAsync(operationType.Id) ??
                throw new BusinessRuleValidationException("There are no Required Staff.");

            List<Staff> staffs = new List<Staff>();
            var isStaffAvailable = true;
            
            foreach(RequiredStaff requiredStaff in requiredStaffList){
                Function function = requiredStaff.Function;
                SpecializationCode specialization = requiredStaff.SpecializationId;
                NumberStaff numberStaff = requiredStaff.StaffQuantity;

                int total = 0;
                
                foreach(string sta in dto.StaffList){
                   Staff staff = await this._repoSta.GetByIdAsync(new StaffId(sta)) ??
                        throw new BusinessRuleValidationException("Staff is invalid.");

                        isStaffAvailable &= await _appointmentStaffRepo.IsStaffAvailableAsync(staff.Id, dto.StartTime, dto.EndTime);

                    if(staff.Function.Equals(function) && staff.SpecializationId.Equals(specialization) && total < numberStaff.NumberRequired)
                    {
                        total++;
                        staffs.Add(staff);
                    } else {
                        throw new BusinessRuleValidationException("The Staff is not required for this surgery.");
                    }
                }
            }

            if (!isStaffAvailable)
            {
                throw new BusinessRuleValidationException("One or more staff members are not available for the selected time.");
            }

            Room room = await this._repoRoom.GetByIdAsync(new RoomNumber(dto.RoomNumber)) ??
                throw new BusinessRuleValidationException("Room is invalid");

            var isRoomAvailable = await _repoRoom.IsRoomAvailableAsync(room.Id, dto.StartTime, dto.EndTime);

            if (!isRoomAvailable)
            {
                throw new BusinessRuleValidationException("The surgery room is not available for the selected time.");
            }


            var appointment = new Appointment(
                operationRequest.Id,
                room.Id.AsString(),
                dto.StartTime,
                dto.EndTime,
                dto.StartDate,
                dto.EndDate
            );

            await _repo.AddAsync(appointment);

            List<string> staffIds = new List<string>();
            foreach (Staff staff in staffs)
            {
                var appointmentStaff = new AppointmentStaff(appointment, staff);
                await _appointmentStaffRepo.AddAsync(appointmentStaff);
                staffIds.Add(staff.Id.AsString());
            }

            operationRequest.ChangeStatus("Planned");

            await _unitOfWork.CommitAsync();
  
            return new AppointmentDto(appointment.Id.AsGuid(), appointment.Status.Description.ToString(), appointment.OperationRequestId.Value, appointment.RoomNumber.AsString(),
                appointment.Slot.TimeInterval.Start.ToString(), appointment.Slot.TimeInterval.End.ToString(), appointment.Slot.Date.Start.ToString(), appointment.Slot.Date.End.ToString(), staffIds
            );
        }

        public async Task<List<AppointmentDto>> GetAllAsync()
        {
           var list = await _repo.GetAllAsync();

            List<AppointmentDto> listDto = list.ConvertAll(appoint =>
               new AppointmentDto
               (appoint.Id.AsGuid(), appoint.Status.Description, appoint.OperationRequestId.ToString(),
               appoint.RoomNumber.Value, appoint.Slot.TimeInterval.Start.ToString(), appoint.Slot.TimeInterval.End.ToString(),
               appoint.Slot.Date.Start.ToString(), appoint.Slot.Date.End.ToString(),
               appoint.AppointmentStaffs.Select(staff => new string(staff.Staff.Id.Value)).ToList()
               ));

            return listDto; 
        }

        public async Task<AppointmentDto> UpdateAsync(UpdateAppointmentDto dto)
        {
            var appointment = await this._repo.GetByIdAsync(new AppointmentId(dto.AppointmentId));

            if (appointment == null)
                return null;

            bool roomChange = false, startTimeChange = false, endTimeChange = false, startDateChange = false, endDateChange = false, staffChange = false;

            if (dto.NewRoomNumber != null && dto.NewRoomNumber != appointment.RoomNumber.Value)
                roomChange = true;

            startTimeChange = dto.NewStartTime != null && dto.NewStartTime != appointment.Slot.TimeInterval.Start.ToString("HH:mm");
            endTimeChange = dto.NewEndTime != null && dto.NewEndTime != appointment.Slot.TimeInterval.End.ToString("HH:mm");
            startDateChange = dto.NewStartDate != null && dto.NewStartDate != appointment.Slot.Date.Start.ToString("yyyy-MM-dd");
            endDateChange = dto.NewEndDate != null && dto.NewEndDate != appointment.Slot.Date.End.ToString("yyyy-MM-dd");

            if (dto.NewStaffList != null && dto.NewStaffList.Any())
                staffChange = true;

            if (roomChange)
                appointment.ChangeRoom(dto.NewRoomNumber);

            if (startTimeChange || endTimeChange || startDateChange || endDateChange)
            {
                string toChangeStartTime = startTimeChange ? dto.NewStartTime : appointment.Slot.TimeInterval.Start.ToString("HH:mm");
                string toChangeEndTime = endTimeChange ? dto.NewEndTime : appointment.Slot.TimeInterval.End.ToString("HH:mm");
                string toChangeStartDate = startDateChange ? dto.NewStartDate : appointment.Slot.Date.Start.ToString("yyyy-MM-dd");
                string toChangeEndDate = endDateChange ? dto.NewEndDate : appointment.Slot.Date.End.ToString("yyyy-MM-dd");

                appointment.ChangeSlot(toChangeStartTime, toChangeEndTime, toChangeStartDate, toChangeEndDate);
            }

            if (staffChange)
            {
                var turnedToStaffList = new List<Staff>();

                for(int i = 0; i < dto.NewStaffList.Count(); i++)
                {
                    turnedToStaffList.Add(await _repoSta.GetByIdAsync(new StaffId(dto.NewStaffList.ElementAt(i))));
                }

                appointment.ChangeStaff(turnedToStaffList); 
            }

            await this._unitOfWork.CommitAsync();

            return new AppointmentDto(appointment.Id.AsGuid(),
                                        appointment.Status.Description, 
                                        appointment.OperationRequestId.Value, 
                                        appointment.RoomNumber.Value, 
                                        appointment.Slot.TimeInterval.Start.ToString("HH:mm"), 
                                        appointment.Slot.TimeInterval.End.ToString("HH:mm"), 
                                        appointment.Slot.Date.Start.ToString("yyyy-MM-dd"), 
                                        appointment.Slot.Date.End.ToString("yyyy-MM-dd"), 
                                        appointment.AppointmentStaffs.Select(appointStaff => new string(appointStaff.Staff.Id.Value)).ToList());
        }

        public async Task<ActionResult<AppointmentDto>> GetByIdAsync(string id)
        {
           var appointment = await _repo.GetByIdAsync(new AppointmentId(id));
           return new AppointmentDto(appointment.Id.AsGuid(),
                                        appointment.Status.Description, 
                                        appointment.OperationRequestId.Value, 
                                        appointment.RoomNumber.Value, 
                                        appointment.Slot.TimeInterval.Start.ToString("HH:mm"), 
                                        appointment.Slot.TimeInterval.End.ToString("HH:mm"), 
                                        appointment.Slot.Date.Start.ToString("yyyy-MM-dd"), 
                                        appointment.Slot.Date.End.ToString("yyyy-MM-dd"), 
                                        appointment.AppointmentStaffs.Select(appointStaff => new string(appointStaff.Staff.Id.Value)).ToList());
        }
    }
}