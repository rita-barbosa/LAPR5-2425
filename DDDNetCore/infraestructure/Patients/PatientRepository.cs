using DDDNetCore.Domain.Patients;
using DDDNetCore.Infrastructure.Shared;

namespace DDDNetCore.Infrastructure.StaffProfiles
{
    public class PatientRepository : BaseRepository<Patient, MedicalRecordNumber>, IPatientRepository
    {
        public PatientRepository(DDDNetCoreDbContext context) : base(context.Patients)
        {

        }
    }
}