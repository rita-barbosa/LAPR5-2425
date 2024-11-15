using MDBackoffice.Domain.Appointments;
using MDBackoffice.Infrastructure.Shared;


namespace MDBackoffice.Infrastructure.Appointments
{
    public class AppointmentRepository : BaseRepository<Appointment, AppointmentId>, IAppointmentRepository
    {
        private readonly MDBackofficeDbContext _context;
        public AppointmentRepository(MDBackofficeDbContext context):base(context.Appointments){
            _context = context;
        }
    }
}
