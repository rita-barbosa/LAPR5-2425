using DDDNetCore.Domain.Shared;

namespace DDDNetCore.Domain.Patients
{
    public interface IPatientRepository : IRepository<Patient, MedicalRecordNumber>
    {
    }
}