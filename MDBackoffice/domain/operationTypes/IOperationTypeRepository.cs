using System.Collections.Generic;
using System.Threading.Tasks;
using MDBackoffice.Domain.Shared;


namespace MDBackoffice.Domain.OperationTypes
{
    public interface IOperationTypeRepository:IRepository<OperationType,OperationTypeId>
    {

         public Task<OperationType> GetByIdWithStaffAsync(OperationTypeId id); 
         Task<List<OperationType>> FilterOperationTypes(OperationTypeQueryParametersDto dto);

        

    }
}