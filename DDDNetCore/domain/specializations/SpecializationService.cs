using System.Threading.Tasks;
using System.Collections.Generic;
using DDDNetCore.Domain.Shared;

namespace DDDNetCore.Domain.Specializations
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
            
            List<SpecializationDto> listDto = list.ConvertAll<SpecializationDto>(spe => new SpecializationDto{Denomination = spe.Id.AsString()});

            return listDto;
        }

        public async Task<SpecializationDto> GetByIdAsync(SpecializationDenomination id)
        {
            var spe = await this._repo.GetByIdAsync(id);
            
            if(spe == null)
                return null;

            return new SpecializationDto{Denomination = spe.Id.AsString()};
        }

        public async Task<SpecializationDto> AddAsync(SpecializationDto dto)
        {
            var specialization = new Specialization(dto.Denomination);

            await this._repo.AddAsync(specialization);

            await this._unitOfWork.CommitAsync();

            return new SpecializationDto { Denomination = specialization.Id.AsString() };
        }

        public async Task<SpecializationDto> UpdateAsync(SpecializationDto dto)
        {
            var specialization = await this._repo.GetByIdAsync(new SpecializationDenomination(dto.Denomination)); 

            if (specialization == null)
                return null;   

            // change all field
            specialization.ChangeDenomination(dto.Denomination);
            
            await this._unitOfWork.CommitAsync();

            return new SpecializationDto { Denomination = specialization.Id.AsString() };
        }

        public async Task<SpecializationDto> InactivateAsync(SpecializationDenomination id)
        {
            var specialization = await this._repo.GetByIdAsync(id); 

            if (specialization == null)
                return null;   

            // change all fields
            specialization.MarkAsInative();
            
            await this._unitOfWork.CommitAsync();

            return new SpecializationDto { Denomination = specialization.Id.AsString() };
        }

         public async Task<SpecializationDto> DeleteAsync(SpecializationDenomination id)
        {
            var specialization = await this._repo.GetByIdAsync(id); 

            if (specialization == null)
                return null;   

            if (specialization.Active)
                throw new BusinessRuleValidationException("It is not possible to delete an active specialization.");
            
            this._repo.Remove(specialization);
            await this._unitOfWork.CommitAsync();

            return new SpecializationDto { Denomination = specialization.Id.AsString() };
        }
    }
}