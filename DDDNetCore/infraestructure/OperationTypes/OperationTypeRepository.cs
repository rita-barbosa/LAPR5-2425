using System.Linq;
using System.Threading.Tasks;
using DDDNetCore.Domain.OperationTypes;
using DDDNetCore.Infrastructure.Shared;
using Microsoft.EntityFrameworkCore;

namespace DDDNetCore.Infrastructure.OperationTypes
{
    public class OperationTypeRepository : BaseRepository<OperationType, OperationTypeId>, IOperationTypeRepository
    {
      
      private readonly DDDNetCoreDbContext _context;
        public OperationTypeRepository(DDDNetCoreDbContext context):base(context.OperationTypes)
        {
            _context = context;   
        }

        public async Task<OperationType> GetByIdWithStaffAsync(OperationTypeId id)
        {
            var operationTypeById =
                _context.OperationTypes
                .Include(p => p.RequiredStaff)
                .Where(p => p.Id.Equals(id));

            return operationTypeById.FirstOrDefault();
        }
    }
}
