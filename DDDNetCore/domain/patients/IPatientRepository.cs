using System.Collections.Generic;
using System.Threading.Tasks;
using DDDNetCore.Domain.Shared;

namespace DDDNetCore.Domain.Patients
{
    public interface IPatientRepository : IRepository<Patient, MedicalRecordNumber>
    {
        Task<bool> ExistsPatientWithEmailOrPhone(string email, string v1, string v2);
        Task<List<Patient>> FilterPatientProfiles(PatientQueryParametersDto dto);
        Task<MedicalRecordNumber> FindLastPatientIdAsync();
        Task<Patient> GetPatientWithEmail(string email);
    }
}