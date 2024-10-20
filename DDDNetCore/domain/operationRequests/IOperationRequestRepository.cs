using DDDNetCore.Domain.Shared;

namespace DDDNetCore.Domain.OperationRequest
{
    public interface IOperationRequestRepository: IRepository<OperationRequest,OperationRequestId>
    {
    }
}