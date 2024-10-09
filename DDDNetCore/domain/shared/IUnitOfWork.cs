using System.Threading.Tasks;

namespace DDDNetCore.Domain.Shared
{
    public interface IUnitOfWork
    {
        Task<int> CommitAsync();
    }
}