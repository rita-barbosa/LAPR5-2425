using System;
using System.Threading.Tasks;
using System.Xml;
using DDDNetCore.Domain.Shared;
using DDDNetCore.Domain.Users;

namespace DDDNetCore.Domain.Patients
{
    public class PatientService
    {
        private readonly IUnitOfWork _unitOfWork;
        private readonly IPatientRepository _repo;

        public PatientService(IUnitOfWork unitOfWork, IPatientRepository repo)
        {
            this._unitOfWork = unitOfWork;
            this._repo = repo;
        }

        public async Task<PatientDto> CreatePatientProfile(CreatingPatientDto dto)
        {
            if (await _repo.ExistsPatientWithEmailOrPhone(dto.Email, dto.Phone.Split(' ')[0], dto.Phone.Split(' ')[1]))
            {
                throw new BusinessRuleValidationException("There already exists a patient member with that email or phone.");
            }
            var seqNumber = await getSequentialNumber();

            Gender? gender = Gender.GetGenderByDescription(dto.Gender) ??
                           throw new BusinessRuleValidationException("Invalid gender.");

            var patient = new Patient(dto.FirstName, dto.LastName, gender, dto.Phone, dto.EmergencyContact, dto.Email, dto.DateBirth, seqNumber);

            await this._repo.AddAsync(patient);

            await this._unitOfWork.CommitAsync();

            return new PatientDto(patient.Name.ToString(), patient.PhoneNumber.ToString(), patient.Email.ToString(), patient.Id.AsString());
        }

        public async void AddUser(User user, string email, string phone)
        {
            bool result = await _repo.ExistsPatientWithEmailOrPhone(email, phone.Split(' ')[0], phone.Split(' ')[1]);

            if(!result){
                throw new InvalidOperationException("There isn't a staff member associated with this email/phone.");
            }

            Patient patient = await _repo.GetPatientWithEmail(email);

            patient.AddUser(user);

            await this._unitOfWork.CommitAsync();
        }

        private async Task<string> getSequentialNumber()
        {
            try
            {
                MedicalRecordNumber patientId = await _repo.FindLastPatientIdAsync();
                string lastSequentialNumber = patientId.AsString().Substring(6);
                int newSequentialNumber = int.Parse(lastSequentialNumber) + 1;
                return newSequentialNumber.ToString("D6");
            }
            catch (NullReferenceException)
            {
                return "000001";
            }
        }
    }
}