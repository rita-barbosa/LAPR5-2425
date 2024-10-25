using System;
using System.Collections;
using System.Collections.Generic;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using DDDNetCore.Domain.Patients;
using DDDNetCore.Domain.Shared;
using DDDNetCore.Infrastructure.Shared;
using Microsoft.EntityFrameworkCore;
using Microsoft.IdentityModel.Tokens;

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

        public async Task<bool> ExistsPatientWithId(string MRN)
        {
            List<Patient> reqPatients = _context.Patients
                .Where(s=> s.Id.Equals(new MedicalRecordNumber(MRN))).ToList();

            if(!reqPatients.IsNullOrEmpty()) return true;
            return false;
        }



        public async Task<List<Patient>> FilterPatientProfiles(PatientQueryParametersDto dto)
        {
           IQueryable<Patient> combinedQuery = null;

            foreach (PatientListingFilterParametersDto filter in dto.queryFilters)
            {

                var query = _context.Patients.AsQueryable();

                // if both names are in the filter then create the full name, else search by the provided name
                if (!string.IsNullOrEmpty(filter.FirstName) && !string.IsNullOrEmpty(filter.LastName))
                {
                    query = query.Where(s => s.Name.FullName.Contains($"{filter.FirstName} {filter.LastName}"));
                }
                else
                {

                    if (!string.IsNullOrEmpty(filter.FirstName))
                    {
                        query = query.Where(s => s.Name.FirstName.Contains(filter.FirstName));
                    }

                    if (!string.IsNullOrEmpty(filter.LastName))
                    {
                        query = query.Where(s => s.Name.LastName.Contains(filter.LastName));
                    }

                }

                if (!string.IsNullOrEmpty(filter.Email))
                {
                    query = query.Where(s => s.Email.EmailAddress.Contains(filter.Email));
                }


                if (!string.IsNullOrEmpty(filter.Gender))
                {
                    query = query.Where(s => s.Gender.Denomination.Equals(filter.Gender.ToLower()));
                }

                if (!string.IsNullOrEmpty(filter.DateBirth))
                {
                    query = query.Where(s => s.DateBirth.Date.ToString() == $"{filter.DateBirth} {"00:00:00"}");
                }

                if (!string.IsNullOrEmpty(filter.MedicalRecordNumber))
                {
                    query = query.AsEnumerable().Where(s => s.Id.AsString() == filter.MedicalRecordNumber).AsQueryable();
                }

                 if (!query.IsNullOrEmpty()){
                    combinedQuery = combinedQuery == null ? query : combinedQuery.Union(query);
                 }

            }

            if(combinedQuery.IsNullOrEmpty())
                return [];

            return [.. combinedQuery];
        }


        public async Task<MedicalRecordNumber> FindLastPatientIdAsync()
        {
            var patientList = await _context.Patients.ToListAsync(); // Load all staff profiles into memory

            var lastPatient = patientList
                .OrderByDescending(patient => patient.Id.AsString()[6..]) // Now we can safely use AsString and Substring
                .FirstOrDefault() ?? throw new NullReferenceException();

            return lastPatient.Id;
        }

        public async Task<Patient> FindPatientWithUserEmail(string email)
        {
            return await _context.Patients
                .Join(
                    _context.Users,
                    patient => patient.UserReference,
                    user => user.Id,
                    (patient, user) => new { patient, user }
                )
                .Where(joinResult => joinResult.user.Email == email)
                .Select(joinResult => joinResult.patient)
                .FirstOrDefaultAsync() ?? throw new NullReferenceException();
        }

        public async Task<Patient> GetPatientWithEmail(string email)
        {
            return await _context.Patients
                .Where(patient =>
                    patient.Email != null &&
                    patient.Email.EmailAddress == email)
                    .FirstOrDefaultAsync();
        }

         public async Task<Patient> FindPatientWithEmailOrPhone(string email, string countryCode, string phone)
        {
            return await _context.Patients
                .Where(patient =>
                    (patient.Email != null && patient.Email.EmailAddress == email) ||
                    (patient.PhoneNumber != null &&
                    patient.PhoneNumber.CountryCode == countryCode &&
                    patient.PhoneNumber.PhoneNumber == phone))
                .FirstOrDefaultAsync(); // Get the first match or return null if none found
        }

        
    }
}