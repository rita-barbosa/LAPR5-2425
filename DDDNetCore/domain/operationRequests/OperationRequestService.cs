using System.Threading.Tasks;
using System.Collections.Generic;
using DDDNetCore.Domain.Shared;
using DDDNetCore.Domain.StaffProfiles;
using DDDNetCore.Domain.Patients;
using DDDNetCore.Domain.OperationTypes;

namespace DDDNetCore.Domain.OperationRequest
{
    public class OperationRequestService
    {
        private readonly IUnitOfWork _unitOfWork;
        private readonly IOperationRequestRepository _repo;
        private readonly IStaffRepository _repoSta;
        private readonly IPatientRepository _repoPat;
        private readonly IOperationTypeRepository _repoOpTy;

        public OperationRequestService(IUnitOfWork unitOfWork, IOperationRequestRepository repo, IStaffRepository repoSta, IPatientRepository repoPat, IOperationTypeRepository repoOpTy)
        {
            this._unitOfWork = unitOfWork;
            this._repo = repo;
            this._repoSta = repoSta;
            this._repoPat = repoPat;
            this._repoOpTy = repoOpTy;
        }

        public async Task<List<OperationRequestDto>> GetAllAsysnc()
        {
            var list = await this._repo.GetAllAsync();

            List<OperationRequestDto> listDto = list.ConvertAll<OperationRequestDto>(opRequest =>
                new OperationRequestDto(opRequest.Id.AsGuid(), opRequest.DeadLineDate.ToString(), opRequest.Priority.ToString(),
                 opRequest.DateOfRequest.ToString(), opRequest.Status.ToString(),  opRequest.StaffId.AsString(), opRequest.Description.DescriptionText, opRequest.PatientId.AsString(),opRequest.OperationTypeId.AsString()));

            return listDto;
        }

        public async Task<OperationRequestDto> GetByIdAsync(OperationRequestId id)
        {
            var opRequest = await this._repo.GetByIdAsync(id);
            
            if (opRequest == null)
                return null;

            return new OperationRequestDto(opRequest.Id.AsGuid(), opRequest.DeadLineDate.ToString(), opRequest.Priority.ToString(),
                 opRequest.DateOfRequest.ToString(), opRequest.Status.ToString(), opRequest.StaffId.AsString(), opRequest.Description.DescriptionText, opRequest.PatientId.AsString(),opRequest.OperationTypeId.AsString());
        }

        public async Task<OperationRequestDto> AddAsync(CreatingOperationRequestDto dto)
        {
            Staff staff = await this._repoSta.GetByIdAsync(new StaffId(dto.StaffId)) ??
                throw new BusinessRuleValidationException("Staff is invalid.");


            Patient patient = await this._repoPat.GetByIdAsync(new MedicalRecordNumber(dto.PatientId))??
                throw new BusinessRuleValidationException("Patient is invalid.");


            OperationType opType = await this._repoOpTy.GetByIdWithStaffAsync(new OperationTypeId(dto.OperationTypeId))??
                throw new BusinessRuleValidationException("Operation Type is invalid.");

            CheckStaffFunctionAndSpecialization(opType, staff);

            var opRequest = new OperationRequest(new Date(dto.DeadLineDate), Priority.GetPriorityByName(dto.Priority), new Date(dto.DateOfRequest),
                new OperationRequestStatus(dto.Status), staff.Id, dto.Description, patient.Id, opType.Id);

            await this._repo.AddAsync(opRequest);

            await this._unitOfWork.CommitAsync();

            return new OperationRequestDto(opRequest.Id.AsGuid(), opRequest.DeadLineDate.ToString(), opRequest.Priority.ToString(),
                 opRequest.DateOfRequest.ToString(), opRequest.Status.ToString(), opRequest.StaffId.AsString(), opRequest.Description.DescriptionText, opRequest.PatientId.AsString(),opRequest.OperationTypeId.AsString());
        }

        private void CheckStaffFunctionAndSpecialization(OperationType opType, Staff staff){
            bool specializationMatches = false;
            foreach (var requiredStaff in opType.RequiredStaff)
            {
                if(requiredStaff.Function.Equals(staff.Function) && requiredStaff.SpecializationId.AsString() == staff.SpecializationId.AsString()){
                    specializationMatches = true;
                    break;
                }
            }

            if (!specializationMatches)
            {
                throw new BusinessRuleValidationException("The Staff Function or Specialization does not match any of the required staff for the operation.");
            }
        }


        public async Task<OperationRequestDto> UpdateAsync(UpdateOperationRequestDto dto)
        {
            var opRequest = await this._repo.GetByIdAsync(new OperationRequestId(dto.Id));

            if (opRequest == null)
                return null;

            opRequest.ChangeDeadLineDate(dto.DeadLineDate);
            opRequest.ChangePriority(dto.Priority);
            opRequest.ChangeDescription(dto.Description);

            await this._unitOfWork.CommitAsync();

            return new OperationRequestDto(opRequest.Id.AsGuid(), opRequest.DeadLineDate.ToString(), opRequest.Priority.ToString(),
                 opRequest.DateOfRequest.ToString(), opRequest.Status.ToString(), opRequest.StaffId.AsString(), opRequest.Description.DescriptionText, opRequest.PatientId.AsString(),opRequest.OperationTypeId.AsString());
        }

        public async Task<OperationRequestDto> DeleteAsync(OperationRequestId id)
        {
            var opRequest = await this._repo.GetByIdAsync(id);

            if(opRequest == null)
                return null;

            this._repo.Remove(opRequest);
            await this._unitOfWork.CommitAsync();

            return new OperationRequestDto(opRequest.Id.AsGuid(), opRequest.DeadLineDate.ToString(), opRequest.Priority.ToString(),
                 opRequest.DateOfRequest.ToString(), opRequest.Status.ToString(), opRequest.StaffId.AsString(), opRequest.Description.DescriptionText, opRequest.PatientId.AsString(),opRequest.OperationTypeId.AsString());
        }


    }
}