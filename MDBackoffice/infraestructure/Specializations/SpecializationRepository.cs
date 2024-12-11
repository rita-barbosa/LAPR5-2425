using MDBackoffice.Domain.Specializations;
using MDBackoffice.Infrastructure.Shared;

namespace MDBackoffice.Infrastructure.Specializations
{
    public class SpecializationRepository : BaseRepository<Specialization, SpecializationCode>, ISpecializationRepository
    {
      
        public SpecializationRepository(MDBackofficeDbContext context):base(context.Specializations)
        {
            
        }

    }
}