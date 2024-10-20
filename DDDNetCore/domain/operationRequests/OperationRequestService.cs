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
                 opRequest.DateOfRequest.ToString(), opRequest.Status.ToString(), opRequest.StaffId,opRequest.PatientId,opRequest.OperationTypeId));

            return listDto;
        }

        public async Task<OperationRequestDto> GetByIdAsync(OperationRequestId id)
        {
            var opRequest = await this._repo.GetByIdAsync(id);
            
            if (opRequest == null)
                return null;

            return new OperationRequestDto(opRequest.Id.AsGuid(), opRequest.DeadLineDate.ToString(), opRequest.Priority.ToString(),
                 opRequest.DateOfRequest.ToString(), opRequest.Status.ToString(), opRequest.StaffId,opRequest.PatientId,opRequest.OperationTypeId);
        }

        public async Task<OperationRequestDto> AddAsync(OperationRequestDto dto)
        {
            var staff = await this._repoSta.GetByIdAsync(dto.StaffId);
            var patient = await this._repoPat.GetByIdAsync(dto.PatientId);
            var opType = await this._repoOpTy.GetByIdAsync(dto.OperationTypeId);

            if (staff == null || patient == null || opType == null)
                throw new BusinessRuleValidationException("Staff, Patient or Operation Type is invalid.");

            var opRequest = new OperationRequest(new Date(dto.DeadLineDate), new Priority(dto.Priority), new Date(dto.DateOfRequest), 
                new Status(dto.Status), dto.StaffId, dto.PatientId, dto.OperationTypeId);

            await this._repo.AddAsync(opRequest);
            await this._unitOfWork.CommitAsync();

            return new OperationRequestDto(opRequest.Id.AsGuid(), opRequest.DeadLineDate.ToString(), opRequest.Priority.ToString(),
                 opRequest.DateOfRequest.ToString(), opRequest.Status.ToString(), opRequest.StaffId,opRequest.PatientId,opRequest.OperationTypeId);
        }

        public async Task<OperationRequestDto> UpdateAsync(OperationRequestDto dto)
        {
            var opRequest = await this._repo.GetByIdAsync(new OperationRequestId(dto.Id));

            if (opRequest == null)
                return null;

            opRequest.ChangeDeadLineDate(dto.DeadLineDate);
            opRequest.ChangePriority(dto.Priority);
            opRequest.ChangeStatus(dto.Status);

            await this._unitOfWork.CommitAsync();

            return new OperationRequestDto(opRequest.Id.AsGuid(), opRequest.DeadLineDate.ToString(), opRequest.Priority.ToString(),
                 opRequest.DateOfRequest.ToString(), opRequest.Status.ToString(), opRequest.StaffId,opRequest.PatientId,opRequest.OperationTypeId);
        }

        public async Task<OperationRequestDto> DeleteAsync(OperationRequestId id)
        {
            var opRequest = await this._repo.GetByIdAsync(id);

            if(opRequest == null)
                return null;

            this._repo.Remove(opRequest);
            await this._unitOfWork.CommitAsync();

            return new OperationRequestDto(opRequest.Id.AsGuid(), opRequest.DeadLineDate.ToString(), opRequest.Priority.ToString(),
                 opRequest.DateOfRequest.ToString(), opRequest.Status.ToString(), opRequest.StaffId,opRequest.PatientId,opRequest.OperationTypeId);
        }


    }
}