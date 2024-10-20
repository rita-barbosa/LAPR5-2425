using DDDNetCore.Domain.OperationTypes;
using DDDNetCore.Infrastructure.Shared;

namespace DDDNetCore.Infrastructure.OperationTypes
{
    public class OperationTypeRepository : BaseRepository<OperationType, OperationTypeId>, IOperationTypeRepository
    {
      
        public OperationTypeRepository(DDDNetCoreDbContext context):base(context.OperationTypes)
        {
            
        }
        
    }
}
