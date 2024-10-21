using System.Threading.Tasks;
using DDDNetCore.Domain.Shared;


namespace DDDNetCore.Domain.OperationTypes
{
    public interface IOperationTypeRepository:IRepository<OperationType,OperationTypeId>
    {

         public Task<OperationType> GetByIdWithStaffAsync(OperationTypeId id); 

    }
}