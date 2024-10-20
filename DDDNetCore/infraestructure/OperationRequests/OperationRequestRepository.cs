using DDDNetCore.Domain.OperationRequest;
using DDDNetCore.Infrastructure.Shared;

namespace DDDNetCore.Infrastructure.OperationRequests
{
    public class OperationRequestRepository : BaseRepository<OperationRequest, OperationRequestId>, IOperationRequestRepository
    {
      
        public OperationRequestRepository(DDDNetCoreDbContext context):base(context.OperationRequests)
        {
            
        }

    }
}