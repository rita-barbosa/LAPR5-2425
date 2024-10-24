using System.Threading.Tasks;
using DDDNetCore.Domain.Logs;
using DDDNetCore.Domain.Shared;

namespace DDDNetCore.Domain.Logs
{
    public interface ILogRepository : IRepository<Log, LogId>
    {
        Task<LogId> FindLastLogIdAsync();
    }
}