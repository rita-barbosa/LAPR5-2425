using MDBackoffice.Domain.Specializations;
using MDBackoffice.Infrastructure.Shared;

namespace MDBackoffice.Infrastructure.Specializations
{
    public class SpecializationRepository : BaseRepository<Specialization, SpecializationDenomination>, ISpecializationRepository
    {
      
        public SpecializationRepository(MDBackofficeDbContext context):base(context.Specializations)
        {
            
        }

    }
}