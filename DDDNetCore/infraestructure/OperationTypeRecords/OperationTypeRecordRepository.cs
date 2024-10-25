using DDDNetCore.Domain.OperationTypesRecords;
using DDDNetCore.Infrastructure.Shared;


namespace DDDNetCore.Infrastructure.OperationTypeRecords
{
    public class OperationTypeRecordRepository : BaseRepository<OperationTypeRecord, OperationTypeRecordId>, IOperationTypeRecordRepository
    {
      
        public OperationTypeRecordRepository(DDDNetCoreDbContext context):base(context.OperationTypeRecords)
        {

        }

    }
}
