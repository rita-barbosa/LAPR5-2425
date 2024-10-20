using System;
using System.Linq;
using System.Threading.Tasks;
using DDDNetCore.Domain.Patients;
using DDDNetCore.Infrastructure.Shared;
using Microsoft.EntityFrameworkCore;

namespace DDDNetCore.Infrastructure.Patients
{
    public class PatientRepository : BaseRepository<Patient, MedicalRecordNumber>, IPatientRepository
    {
        private readonly DDDNetCoreDbContext _context;
        public PatientRepository(DDDNetCoreDbContext context) : base(context.Patients)
        {
            _context = context;
        }

        public async Task<bool> ExistsPatientWithEmailOrPhone(string email, string countryCode, string phone)
        {
            return await _context.Patients
                .AnyAsync(patient =>
                    (patient.Email != null && patient.Email.EmailAddress == email) ||
                    (patient.PhoneNumber != null &&
                     patient.PhoneNumber.CountryCode == countryCode &&
                     patient.PhoneNumber.PhoneNumber == phone));
        }
        public async Task<MedicalRecordNumber> FindLastPatientIdAsync()
        {
            var patientList = await _context.Patients.ToListAsync(); // Load all staff profiles into memory

            var lastPatient = patientList
                .OrderByDescending(patient => patient.Id.AsString()[6..]) // Now we can safely use AsString and Substring
                .FirstOrDefault() ?? throw new NullReferenceException();

            return lastPatient.Id;
        }
    }
}