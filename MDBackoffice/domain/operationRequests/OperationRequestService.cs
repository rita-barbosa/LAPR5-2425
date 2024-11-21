using System.Threading.Tasks;
using System.Collections.Generic;
using MDBackoffice.Domain.Shared;
using MDBackoffice.Domain.StaffProfiles;
using MDBackoffice.Domain.Patients;
using MDBackoffice.Domain.OperationTypes;
using System;
using System.Linq;
using Microsoft.AspNetCore.Http.HttpResults;
using MDBackoffice.Domain.Users;
using Microsoft.AspNetCore.Identity;
using MDBackoffice.Domain.Logs;
using Microsoft.AspNetCore.Mvc;

namespace MDBackoffice.Domain.OperationRequests
{
    public class OperationRequestService
    {
        private readonly IUnitOfWork _unitOfWork;
        private readonly IOperationRequestRepository _repo;
        private readonly IStaffRepository _repoSta;
        private readonly LogService _logService;
        private readonly PatientService _patientService;
        private readonly IPatientRepository _repoPat;
        private readonly IOperationTypeRepository _repoOpTy;
        private readonly UserService _userService;

        public OperationRequestService(IUnitOfWork unitOfWork, IOperationRequestRepository repo, IStaffRepository repoSta, LogService logService, PatientService patientService, IPatientRepository repoPat, IOperationTypeRepository repoOpTy, UserService userService)
        {
            this._unitOfWork = unitOfWork;
            this._repo = repo;
            this._repoSta = repoSta;
            this._repoPat = repoPat;
            this._repoOpTy = repoOpTy;
            this._logService = logService;
            this._patientService = patientService;
            this._userService = userService;
        }

        public async Task<List<OperationRequestDto>> GetAllAsysnc()
        {
            var list = await _repo.GetAllAsync();

            List<OperationRequestDto> listDto = list.ConvertAll(opRequest =>
                new OperationRequestDto(opRequest.Id.AsGuid(), opRequest.DeadLineDate.ToString(), opRequest.Priority.ToString(),
                 opRequest.DateOfRequest.ToString(), opRequest.Status.ToString(), opRequest.StaffId.AsString(), opRequest.Description.DescriptionText, opRequest.PatientId.AsString(), opRequest.OperationTypeId.AsString()));

            return listDto;
        }

        public async Task<OperationRequestDto> GetByIdAsync(OperationRequestId id)
        {
            var opRequest = await this._repo.GetByIdAsync(id);

            if (opRequest == null)
                return null;

            return new OperationRequestDto(opRequest.Id.AsGuid(), opRequest.DeadLineDate.ToString(), opRequest.Priority.ToString(),
                 opRequest.DateOfRequest.ToString(), opRequest.Status.ToString(), opRequest.StaffId.AsString(), opRequest.Description.DescriptionText, opRequest.PatientId.AsString(), opRequest.OperationTypeId.AsString());
        }

        public async Task<OperationRequestDto> AddAsync(CreatingOperationRequestDto dto)
        {
            Staff staff = await this._repoSta.GetByIdAsync(new StaffId(dto.StaffId)) ??
                throw new BusinessRuleValidationException("Staff is invalid.");

            Patient patient = await this._repoPat.GetByIdAsync(new MedicalRecordNumber(dto.PatientId)) ??
                throw new BusinessRuleValidationException("Patient is invalid.");

            OperationType opType = await this._repoOpTy.GetByIdWithStaffAsync(new OperationTypeId(dto.OperationTypeId)) ??
                throw new BusinessRuleValidationException("Operation Type is invalid.");

            CheckStaffFunctionAndSpecialization(opType, staff);

            var opRequest = new OperationRequest(new Date(dto.DeadLineDate), Priority.GetPriorityByName(dto.Priority), new Date(dto.DateOfRequest), staff.Id, dto.Description, patient.Id, opType.Id);

            await this._repo.AddAsync(opRequest);

            await this._unitOfWork.CommitAsync();

            return new OperationRequestDto(opRequest.Id.AsGuid(), opRequest.DeadLineDate.ToString(), opRequest.Priority.ToString(),
                 opRequest.DateOfRequest.ToString(), opRequest.Status.ToString(), opRequest.StaffId.AsString(), opRequest.Description.DescriptionText, opRequest.PatientId.AsString(), opRequest.OperationTypeId.AsString());
        }

        private void CheckStaffFunctionAndSpecialization(OperationType opType, Staff staff)
        {
            bool specializationMatches = false;
            foreach (var requiredStaff in opType.RequiredStaff)
            {
                if (requiredStaff.Function.Equals(staff.Function) && requiredStaff.SpecializationId.AsString() == staff.SpecializationId.AsString())
                {
                    specializationMatches = true;
                    break;
                }
            }

            if (!specializationMatches)
            {
                throw new BusinessRuleValidationException("The Staff Function or Specialization does not match any of the required staff for the operation.");
            }
        }


        public virtual async Task<OperationRequestDto> UpdateAsync(UpdateOperationRequestDto dto)
        {
            var opRequest = await _repo.GetByIdAsync(new OperationRequestId(dto.Id));

            if (opRequest == null)
                return null;

            if (!opRequest.DeadLineDate.Equals(dto.DeadLineDate)){
                opRequest.ChangeDeadLineDate(dto.DeadLineDate);
                await _logService.CreateEditLog(opRequest.Id.ToString(), opRequest.DeadLineDate.GetType().Name, "The operation request deadline date was altered.");
            }

            if (!opRequest.Priority.Equals(dto.Priority)){
                opRequest.ChangePriority(dto.Priority);
                await _logService.CreateEditLog(opRequest.Id.ToString(), opRequest.Priority.GetType().Name, "The operation request priority was altered.");
            }

            if (!opRequest.Description.Equals(dto.Description)){
                opRequest.ChangeDescription(dto.Description);
                await _logService.CreateEditLog(opRequest.Id.ToString(), opRequest.Description.GetType().Name, "The operation request description was altered.");
            }

            await _unitOfWork.CommitAsync();

            return new OperationRequestDto(opRequest.Id.AsGuid(), opRequest.DeadLineDate.ToString(), opRequest.Priority.ToString(),
                 opRequest.DateOfRequest.ToString(), opRequest.Status.ToString(), opRequest.StaffId.AsString(), opRequest.Description.DescriptionText, opRequest.PatientId.AsString(), opRequest.OperationTypeId.AsString());
        }

        public async Task<OperationRequestDto> DeleteAsync(OperationRequestId id)
        {
            var opRequest = await this._repo.GetByIdAsync(id);

            if (opRequest == null)
                return null;

            this._repo.Remove(opRequest);
            await this._unitOfWork.CommitAsync();

            return new OperationRequestDto(opRequest.Id.AsGuid(), opRequest.DeadLineDate.ToString(), opRequest.Priority.ToString(),
                 opRequest.DateOfRequest.ToString(), opRequest.Status.ToString(), opRequest.StaffId.AsString(), opRequest.Description.DescriptionText, opRequest.PatientId.AsString(), opRequest.OperationTypeId.AsString());
        }

        public async Task<List<ListOperationRequestDto>> GetOperationRequestByFiltersAsync(string email, string? name, string? priority, string? operationType, string? status, string? dateOfRequest, string? deadLineDate)
        {

            var staff = await _repoSta.GetStaffWithEmail(email);

            var opRequests = await _repo.FindAllConditioned(staff.Id, name, priority, operationType, status, dateOfRequest, deadLineDate);

            return await ConvertOperationRequestToDto(opRequests.ToList());
        }

        private async Task<List<ListOperationRequestDto>> ConvertOperationRequestToDto(List<OperationRequest> opRequests)
        {
            var operationRequestDtos = new List<ListOperationRequestDto>();
            foreach (var opRequest in opRequests)
            {
                var patient = await _repoPat.GetByIdAsync(opRequest.PatientId) ?? throw new BusinessRuleValidationException("Couldn't obtain the patient associated with the operation request.");

                var dto = new ListOperationRequestDto(
                    opRequest.Id.Value.ToString(),
                    patient.Name.ToString(), 
                    opRequest.OperationTypeId.AsString(),
                    opRequest.Status.ToString()
                );

                operationRequestDtos.Add(dto);
            }
            return operationRequestDtos;
        }

        public async Task<bool> DeleteOperationRequest(string id, string userEmail)
        {
            User user = await _userService.FindByEmailAsync(userEmail);
            if (user == null)
            {
                throw new BusinessRuleValidationException("No user found with this email.");
            }

            Staff staff = await _repoSta.FindStaffWithUserId(user.Id.ToString());
            if (staff == null)
            {
                throw new BusinessRuleValidationException("No staff found with this user.");
            }

            OperationRequest operationRequest = await _repo.GetByIdAsync(new OperationRequestId(id));
            if (operationRequest == null)
            {
                return false;
            }

            if (operationRequest.Status.Description.Equals("Requested") && 
                operationRequest.Id.Value == id &&
                operationRequest.StaffId == staff.Id)
            {
                _repo.Remove(operationRequest);
                await _unitOfWork.CommitAsync();
                return true;
            }
            else
            {
                return false;
            }
        }

        public virtual async Task<bool> CheckDoctorIsRequestingDoctor(string? userEmail, string id)
        {

            var request = await GetByIdAsync(new OperationRequestId(id));

            var doctor = await _repoSta.GetStaffWithEmail(userEmail);

            if (request.StaffId.Equals(doctor.Id.AsString())){
                return true;
            }

            return false;            
        }

        public async Task<bool> DeleteOperationRequestFromPatient(string patientId, string operationRequestId, string email)
        {
            User user = await _userService.FindByEmailAsync(email);
            if (user == null)
            {
                throw new BusinessRuleValidationException("No user found with this email.");
            }

            Staff staff = await _repoSta.FindStaffWithUserId(user.Id.ToString());
            if (staff == null)
            {
                throw new BusinessRuleValidationException("No staff found with this user.");
            }

            Patient patient = await _repoPat.GetByIdAsync(new MedicalRecordNumber(patientId));
            if (patient == null)
            {
                throw new BusinessRuleValidationException("No patient found with this id.");
            }

            patient.RemoveRequestFromHistory(new OperationRequestId(operationRequestId));
            await _unitOfWork.CommitAsync();
            return true;
        }


        public async Task<bool> AddOperationRequestToPatient(string patientId, string operationRequestId, string email)
        {
            User user = await _userService.FindByEmailAsync(email);
            if (user == null)
            {
                throw new BusinessRuleValidationException("No user found with this email.");
            }

            Staff staff = await _repoSta.FindStaffWithUserId(user.Id.ToString());
            if (staff == null)
            {
                throw new BusinessRuleValidationException("No staff found with this user.");
            }

            Patient patient = await _repoPat.GetByIdAsync(new MedicalRecordNumber(patientId));
            if (patient == null)
            {
                throw new BusinessRuleValidationException("No patient found with this id.");
            }

            var appointmentHistory = new AppointmentHistory(operationRequestId, 1, 2, patient.Id);
            await _unitOfWork.CommitAsync();

            patient.AddRequestToHistory(appointmentHistory);
            await _unitOfWork.CommitAsync();
            return true;
        }

        public virtual async Task<ActionResult<IEnumerable<OperationRequestDto>>> GetAllFromDoctorAsysnc(string userEmail)
        {
            User user = await _userService.FindByEmailAsync(userEmail);
            if (user == null)
            {
                throw new BusinessRuleValidationException("No user found with this email.");
            }

            Staff staff = await _repoSta.FindStaffWithUserId(user.Id.ToString());
            if (staff == null)
            {
                throw new BusinessRuleValidationException("No staff found with this user.");
            }

            var list = await this._repo.GetAllFromDoctorAsync(staff.Id.ToString());

            List<OperationRequestDto> listDto = list.ConvertAll<OperationRequestDto>(opRequest =>
                new OperationRequestDto(opRequest.Id.AsGuid(), opRequest.DeadLineDate.ToString(), opRequest.Priority.ToString(),
                 opRequest.DateOfRequest.ToString(), opRequest.Status.ToString(), opRequest.StaffId.AsString(), opRequest.Description.DescriptionText, opRequest.PatientId.AsString(), opRequest.OperationTypeId.AsString()));

            return listDto;
        }
    }
}