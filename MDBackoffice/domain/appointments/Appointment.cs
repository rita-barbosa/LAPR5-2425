using System;
using MDBackoffice.Domain.OperationRequests;
using MDBackoffice.Domain.Rooms;
using MDBackoffice.Domain.Shared;

namespace MDBackoffice.Domain.Appointments
{
    public class Appointment : Entity<AppointmentId>, IAggregateRoot
    {
        public AppointmentStatus Status { get; private set; }

        public OperationRequestId OperationRequestId { get; private set; }

        public RoomNumber RoomNumber { get; private set; }

        public Slot Slot { get; private set; }

        public Appointment()
        {

        }

        public Appointment(OperationRequestId opRequestId, RoomNumber roomNumber, Slot slot)
        {
            this.Id = new AppointmentId(Guid.NewGuid());
            this.Status = AppointmentStatus.Scheduled;
            this.OperationRequestId = opRequestId;
            this.RoomNumber = roomNumber;
            this.Slot = slot;
        }

        public Appointment(string status,  OperationRequestId opRequestId, string roomNumber, string startTime, string endTime, string startDate, string endDate = null)
        {
            this.Id = new AppointmentId(Guid.NewGuid());
            this.Status = new AppointmentStatus(status);
            this.OperationRequestId = opRequestId;
            this.RoomNumber = new RoomNumber(roomNumber);
            this.Slot = new Slot(startTime, endTime, startDate, endDate);

        }

        public void ChangeStatus(string status)
        {
            this.Status = AppointmentStatus.GetStatusByDescription(status);
        }
    }
}