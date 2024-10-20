using DDDNetCore.Domain.Specializations;
using DDDNetCore.Infrastructure.Shared;

namespace DDDNetCore.Infrastructure.Specializations
{
    public class SpecializationRepository : BaseRepository<Specialization, SpecializationDenomination>, ISpecializationRepository
    {
      
        public SpecializationRepository(DDDNetCoreDbContext context):base(context.Specializations)
        {
            
        }

    }
}