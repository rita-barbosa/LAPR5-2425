using MDBackoffice.Domain.Shared;

namespace MDBackoffice.Domain.Appointments
{
    public interface IAppointmentRepository : IRepository<Appointment, AppointmentId>
    {

    }
}