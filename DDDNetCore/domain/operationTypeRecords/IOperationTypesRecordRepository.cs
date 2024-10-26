using System.Threading.Tasks;
using DDDNetCore.Domain.OperationTypes;
using DDDNetCore.Domain.Shared;

namespace DDDNetCore.Domain.OperationTypesRecords
{
    public interface IOperationTypeRecordRepository : IRepository<OperationTypeRecord, OperationTypeRecordId>
    {
        Task<OperationTypeRecord> GetLastFromOpType(OperationTypeId id);
    }
}