using System.Threading.Tasks;
using System.Collections.Generic;
using DDDNetCore.Domain.Shared;
using DDDNetCore.Domain.OperationTypes.ValueObjects.RequiredStaff;
using DDDNetCore.Domain.OperationTypes.ValueObjects.Phase;

namespace DDDNetCore.Domain.OperationTypes
{
    public class OperationTypeService
    {
        private readonly IUnitOfWork _unitOfWork;
        private readonly IOperationTypeRepository _repo;

        public OperationTypeService(IUnitOfWork unitOfWork, IOperationTypeRepository repo)
        {
            this._unitOfWork = unitOfWork;
            this._repo = repo;
        }

        public async Task<List<OperationTypeDto>> GetAllAsync()
        {
            var list = await this._repo.GetAllAsync();
            
            List<OperationTypeDto> listDto = list.ConvertAll<OperationTypeDto>(operationType => ToDto(operationType));

            return listDto;
        }

        public async Task<OperationTypeDto> GetByIdAsync(OperationTypeId id)
        {
            var operationType = await this._repo.GetByIdAsync(id);
            
            if (operationType == null)
                return null;

            return ToDto(operationType);
        }

        public async Task<OperationTypeDto> AddAsync(OperationTypeDto dto)
        {
            var operationType = new OperationType(
                dto.Name,
                dto.EstimatedDuration,
                dto.Status,
                dto.RequiredStaff,
                dto.Phases
            );

            await this._repo.AddAsync(operationType);
            await this._unitOfWork.CommitAsync();

            return ToDto(operationType);
        }

        public async Task<OperationTypeDto> UpdateAsync(OperationTypeDto dto)
        {
            var operationType = await this._repo.GetByIdAsync(new OperationTypeId(dto.Name));

            if (operationType == null)
                return null;

            operationType.ChangeName(dto.Name);
            operationType.ChangeEstimatedDuration(dto.EstimatedDuration);
            operationType.ChangeStatus(dto.Status);
            operationType.ChangeRequiredStaff(dto.RequiredStaff);
            operationType.ChangePhases(dto.Phases);

            await this._unitOfWork.CommitAsync();

            return ToDto(operationType);
        }

        public async Task<OperationTypeDto> InactivateAsync(OperationTypeId id)
        {
            var operationType = await this._repo.GetByIdAsync(id);

            if (operationType == null)
                return null;

            operationType.ChangeStatus(false); // false == Inactivate status

            await this._unitOfWork.CommitAsync();

            return ToDto(operationType);
        }

        public async Task<OperationTypeDto> DeleteAsync(OperationTypeId id)
        {
            var operationType = await this._repo.GetByIdAsync(id);

            if (operationType == null)
                return null;

            if (operationType.Status.Active)
                throw new BusinessRuleValidationException("It is not possible to delete an active OperationType.");

            this._repo.Remove(operationType);
            await this._unitOfWork.CommitAsync();

            return ToDto(operationType);
        }

        private OperationTypeDto ToDto(OperationType operationType)
        {
            return new OperationTypeDto
            {
                Name = operationType.Name.OperationName,
                EstimatedDuration = operationType.EstimatedDuration.TotalDurationMinutes,
                Status = operationType.Status.Active,
                RequiredStaff = operationType.RequiredStaff.ConvertAll(staff => new RequiredStaffDto
                {
                    StaffQuantity = staff.StaffQuantity.NumberRequired,
                    Function = staff.Function.Description,
                    SpecializationId = staff.SpecializationId.Denomination
                }),
                Phases = operationType.Phases.ConvertAll(phase => new PhaseDto
                {
                    Description = phase.Description.Description,
                    Duration = phase.Duration.DurationMinutes
                })
            };
        }
    }
}
