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
            
            List<SpecializationDto> listDto = list.ConvertAll<SpecializationDto>(spe => new SpecializationDto{Denomination = spe.Denomination.Denomination});

            return listDto;
        }

        public async Task<SpecializationDto> GetByIdAsync(SpecializationId id)
        {
            var spe = await this._repo.GetByIdAsync(id);
            
            if(spe == null)
                return null;

            return new SpecializationDto{Denomination = spe.Denomination.Denomination};
        }

        public async Task<SpecializationDto> AddAsync(SpecializationDto dto)
        {
            var specialization = new Specialization(dto.Denomination);

            await this._repo.AddAsync(specialization);

            await this._unitOfWork.CommitAsync();

            return new SpecializationDto { Denomination = specialization.Denomination.Denomination };
        }

        public async Task<SpecializationDto> UpdateAsync(SpecializationDto dto)
        {
            var specialization = await this._repo.GetByIdAsync(new SpecializationId(dto.Denomination)); 

            if (specialization == null)
                return null;   

            specialization.ChangeDenomination(dto.Denomination);
            
            await this._unitOfWork.CommitAsync();

            return new SpecializationDto { Denomination = specialization.Denomination.Denomination };
        }

         public async Task<SpecializationDto> DeleteAsync(SpecializationId id)
        {
            var specialization = await this._repo.GetByIdAsync(id); 

            if (specialization == null)
                return null;   

            this._repo.Remove(specialization);
            await this._unitOfWork.CommitAsync();

            return new SpecializationDto { Denomination = specialization.Denomination.Denomination };
        }
    }
}