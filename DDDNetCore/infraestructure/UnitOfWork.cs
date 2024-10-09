using System.Threading.Tasks;
using DDDNetCore.Domain.Shared;

namespace DDDNetCore.Infrastructure
{
    public class UnitOfWork : IUnitOfWork
    {
        private readonly DDDNetCoreDbContext _context;

        public UnitOfWork(DDDNetCoreDbContext context)
        {
            this._context = context;
        }

        public async Task<int> CommitAsync()
        {
            return await this._context.SaveChangesAsync();
        }
    }
}