using System.Linq;
using System.Threading.Tasks;
using DDDNetCore.Domain.OperationTypes;
using DDDNetCore.Domain.OperationTypesRecords;
using DDDNetCore.Infrastructure.Shared;
using Microsoft.EntityFrameworkCore;


namespace DDDNetCore.Infrastructure.OperationTypeRecords
{
    public class OperationTypeRecordRepository : BaseRepository<OperationTypeRecord, OperationTypeRecordId>, IOperationTypeRecordRepository
    {
        private readonly DDDNetCoreDbContext _context;
      
        public OperationTypeRecordRepository(DDDNetCoreDbContext context):base(context.OperationTypeRecords)
        {
            _context = context;
        }

        public async Task<OperationTypeRecord> GetLastFromOpType(OperationTypeId id)
        {
            var operationTypeRecord = await _context.OperationTypeRecords
                .Where(p => p.OperationTypeId.OpID == id.ToString())
                .OrderByDescending(p => p.Version.Version) 
                .FirstOrDefaultAsync();

            return operationTypeRecord;
        }

    }
}
