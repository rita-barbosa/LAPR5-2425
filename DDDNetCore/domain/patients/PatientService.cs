using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using DDDNetCore.Domain.Emails;
using DDDNetCore.Domain.Shared;
using DDDNetCore.Domain.Users;
using Microsoft.AspNetCore.Mvc;
using DDDNetCore.Domain.Logs;
using Microsoft.Extensions.Configuration;

namespace DDDNetCore.Domain.Patients
{
    public class PatientService
    {
        private readonly IUnitOfWork _unitOfWork;
        private readonly IPatientRepository _repo;
        private readonly UserService _userService;
        private readonly EmailService _emailService;
        private readonly LogService _logService;
        private readonly IConfiguration _configuration;
        public PatientService(IUnitOfWork unitOfWork, LogService logService, IConfiguration configuration, IPatientRepository repo, UserService userService, EmailService emailService)
        {
            this._unitOfWork = unitOfWork;
            this._repo = repo;
            this._userService = userService;
            this._configuration = configuration;
            this._emailService = emailService;
            this._logService = logService;
        }

        public async Task<PatientDto> GetByIdAsync( MedicalRecordNumber id)
        {
            var patient = await _repo.GetByIdAsync(id);

            if (patient == null)
                return null;

            return new PatientDto(patient.Name.ToString(), patient.PhoneNumber.ToString(), patient.Email.ToString(), patient.Id.AsString());
        }
        public async Task<PatientDto> CreatePatientProfile([FromBody] CreatingPatientDto dto)
        {
            if (await _repo.ExistsPatientWithEmailOrPhone(dto.Email, dto.Phone.Split(' ')[0], dto.Phone.Split(' ')[1]))
            {
                throw new BusinessRuleValidationException("There already exists a patient member with that email or phone.");
            }
            var seqNumber = await getSequentialNumber();

            Gender? gender = Gender.GetGenderByDescription(dto.Gender) ??
                           throw new BusinessRuleValidationException("Invalid gender.");

            var patient = new Patient(dto.FirstName, dto.LastName, dto.Address, gender, dto.Phone, dto.EmergencyContact, dto.Email, dto.DateBirth, seqNumber);

            await _repo.AddAsync(patient);

            await _unitOfWork.CommitAsync();

            return new PatientDto(patient.Name.ToString(), patient.PhoneNumber.ToString(), patient.Email.ToString(), patient.Address.ToString(), patient.DateBirth.ToString("dd-MM-yyyy"), patient.Id.AsString());
        }

        public async void AddUser(User user, string email, string phone)
        {
            bool result = await _repo.ExistsPatientWithEmailOrPhone(email, phone.Split(' ')[0], phone.Split(' ')[1]);

            if (!result)
            {
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

        public async Task DeletePatientProfile(string id)
        {
            if (!await _repo.ExistsPatientWithId(id))
            {
                throw new BusinessRuleValidationException("There is no patient profile with the given Id.");
            }

            Patient patient = await _repo.GetByIdAsync(new MedicalRecordNumber(id));
            string userRef = patient.UserReference;

            if (patient.Anonymize())
            {
                var result = await _userService.DeleteByIdAsync(patient.UserReference);
                if (result.Succeeded)
                {
                    await _logService.CreateDeletionLog(patient.Id.Value, patient.GetType().ToString(), "Anonymization of patient's profile.");
                    await _logService.CreateDeletionLog(userRef, "DDDNetCore.Domain.Users", "Deletion of patient's account.");
                    await this._unitOfWork.CommitAsync();
                }
            }
            
        }
    

        public async Task<List<PatientDto>> FilterPatientProfiles(PatientQueryParametersDto dto)
        {
            List<Patient> filteredPatients = await _repo.FilterPatientProfiles(dto);
            List<PatientDto> patientDtoListFiltered = [];

            foreach (Patient patient in filteredPatients)
            {
                patientDtoListFiltered.Add(new PatientDto(patient.Name.ToString(), patient.PhoneNumber.ToString(), patient.Email.ToString(), patient.Id.AsString()));
            }

            return patientDtoListFiltered;
        }

        public async Task<List<PatientDto>> GetAllAsysnc()
        {
            var list = await _repo.GetAllAsync();

            List<PatientDto> listDto = list.ConvertAll<PatientDto>(patient => new PatientDto(patient.Name.ToString(),
                patient.PhoneNumber.ToString(), patient.Email.ToString(), patient.Id.AsString()));

            return listDto;
        }

        public async Task<PatientDto> UpdateAsync(EditPatientDto dto)
        {
            var patient = await _repo.GetByIdAsync(new MedicalRecordNumber(dto.PatientId));

            if (patient == null)
                return null;

            bool phoneChange = false, emailChange = false, adressChange = false;
            string oldEmail = null;

            if (dto.Phone != null)
            {
                patient.ChangePhone(dto.Phone);
                phoneChange = true;
            }

            if (dto.Email != null)
            {
                oldEmail = patient.Email.ToString();
                patient.ChangeEmail(dto.Email);
                emailChange = true;
            }

            if (dto.Address != null)
            {
                patient.ChangeAddress(dto.Address);
                adressChange = true;
            }

            if (dto.Name != null)
                patient.ChangeName(dto.Name);

            if(dto.DateBirth != null)
                patient.ChangeDateBirth(dto.DateBirth);

            await this._unitOfWork.CommitAsync();

            VerificationsToSendEmail(phoneChange, emailChange, adressChange, false, oldEmail, patient);

            return new PatientDto(patient.Name.ToString(), patient.PhoneNumber.ToString(),
                patient.Email.ToString(), patient.Address.ToString(), patient.DateBirth.ToString("dd-MM-yyyy"), patient.Id.AsString());
        }

        private async void VerificationsToSendEmail(bool phoneChange, bool emailChange, bool adressChange, bool emContactChange, string oldEmail, Patient patient)
        {
            if (phoneChange || emailChange || adressChange)
            {
                string changedInformation = "<p>The new information is the following:</p><ul>";

                if (phoneChange)
                    changedInformation += "<li>Phone Number: " + patient.PhoneNumber.ToString() + "</li>";

                if (emailChange)
                    changedInformation += "<li>Email: " + patient.Email.ToString() + "</li>";

                if (adressChange)
                    changedInformation += "<li>Address: " + patient.Address.ToString() + "</li>";

                changedInformation += "</ul>";

                EmailMessageDto emailDto = new(
                    _configuration["App:Email"] ?? throw new NullReferenceException("The hospital email is not configured."),
                    oldEmail,
                    "Contact Information Edition",
                    "<p>Hello,</p><p>This email was sent to inform you that your contact information in your patient profile in the HealthCare Clinic System has been changed.</p>" +
                    changedInformation +
                    "<p>Thank you for choosing us,<br>HealthCare Clinic</p></body></html>"
                );

                await _emailService.SendProfileEditEmail(emailDto);
            }
        }

        public async Task<PatientDto> EditProfile(string email, EditPatientProfileDto dto)
        {
            var patient = await _repo.FindPatientWithUserEmail(email);

            if (dto.Phone != null) patient.ChangePhone(dto.Phone);
            if (dto.Address != null) patient.ChangeAddress(dto.Address);
            if (dto.Name != null) patient.ChangeName(dto.Name);
            if (dto.EmergencyContact != null) patient.ChangeEmergencyContact(dto.EmergencyContact);
            if (dto.Email != null)
            {
                await _userService.EditUserProfile(email, dto.Email);
                patient.ChangeEmail(dto.Email);
            }
            await _unitOfWork.CommitAsync();

            return new PatientDto(patient.Name.ToString(), patient.PhoneNumber.ToString(), patient.Email.ToString(), patient.Address.ToString(), patient.DateBirth.ToString("dd-MM-yyyy"), patient.Id.AsString());
        }

        public async void ConfirmPatientAccountDeletionEmail(string? confirmationLink, string email)
        {
            string emailBody = "<html><body><p>Hello,</p><p>This email was sent to confirm your account's deletion in the HealthCare Clinic System.</p><p><a href='" + confirmationLink + "'>Click in the link to confirm the deletion of the account.</a></p><p>Thank you for choosing us,<br>HealthCare Clinic</p></body></html>"  ;                  
            
            EmailMessageDto emailMessageDto = new EmailMessageDto(_configuration["App:Email"] ?? throw new NullReferenceException("The hospital email is not configured."), email, "Account Deletion Confirmation", emailBody);
            
            await _emailService.SendAccountDeletionEmail(emailMessageDto);
        }

        public async void AnonymizeProfile(string email)
        {
            var patient = await _repo.GetPatientWithEmail(email);

            patient.RemoveUser();
            patient.Anonymize();

            await _logService.CreateDeletionLog(patient.Id.AsString(), patient.GetType().Name, "Anonymization of patient profile.");

            await _unitOfWork.CommitAsync();
        }



        public async Task<string> GetProfileEmail(string email, string phone)
        {
            Patient patient = await _repo.FindPatientWithEmailOrPhone(email, phone.Split(' ')[0], phone.Split(' ')[1]);

            return patient.Email.EmailAddress;
        }
    }
}