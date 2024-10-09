using DDDNetCore.Domain.Families;
using DDDNetCore.Infrastructure.Shared;

namespace DDDNetCore.Infrastructure.Families
{
    public class FamilyRepository : BaseRepository<Family, FamilyId>, IFamilyRepository
    {
      
        public FamilyRepository(DDDNetCoreDbContext context):base(context.Families)
        {
            
        }

    }
}