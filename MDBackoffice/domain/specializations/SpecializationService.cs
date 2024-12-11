using System.Threading.Tasks;
using System.Collections.Generic;
using MDBackoffice.Domain.Shared;
using System;

namespace MDBackoffice.Domain.Specializations
{
    public class SpecializationService
    {
        private readonly IUnitOfWork _unitOfWork;
        private readonly ISpecializationRepository _repo;

        public SpecializationService(IUnitOfWork unitOfWork, ISpecializationRepository repo)
        {
            this._unitOfWork = unitOfWork;
            this._repo = repo;
        }

        public async Task<List<SpecializationDto>> GetAllAsync()
        {
            var list = await this._repo.GetAllAsync();

            List<SpecializationDto> listDto = list.ConvertAll<SpecializationDto>(spe => new SpecializationDto { Denomination = spe.Id.Value });

            return listDto;
        }

        public async Task<SpecializationDto> GetByIdAsync(SpecializationCode id)
        {
            var spe = await this._repo.GetByIdAsync(id);

            if (spe == null)
                return null;

            return new SpecializationDto { Denomination = spe.Id.Value };
        }

        public async Task<SpecializationDto> AddAsync(SpecializationDto dto)
        {
            var specialization = new Specialization(dto.Code, dto.Denomination, dto.Description);

            await this._repo.AddAsync(specialization);

            await this._unitOfWork.CommitAsync();

            return new SpecializationDto { Code = specialization.Id.Value, Denomination = specialization.Denomination.ToString(), Description = specialization.Description.ToString() };
        }

        public async Task<SpecializationDto> DeleteAsync(SpecializationCode id)
        {
            var specialization = await this._repo.GetByIdAsync(id);

            if (specialization == null)
                return null;

            this._repo.Remove(specialization);
            await this._unitOfWork.CommitAsync();

            return new SpecializationDto { Code = specialization.Id.Value };
        }

        public async Task<SpecializationDto> EditSpecialization(EditSpecializationDto dto)
        {
            var spec = await _repo.GetByIdAsync(new SpecializationCode(dto.Code)) ?? throw new BusinessRuleValidationException("No specialization was found for the given code.");

            if (dto.Denomination != null)
            {
                spec.ChangeDenomination(dto.Denomination);
            }
            if (dto.Description != null)
            {
                spec.ChangeDescription(dto.Description);
            }
            await _unitOfWork.CommitAsync();
            
            return new SpecializationDto { Code = spec.Id.Value, Denomination = spec.Denomination.ToString(), Description = spec.Description.ToString() };
        }
    }
}