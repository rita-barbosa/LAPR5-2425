using System.Threading.Tasks;
using MimeKit;

namespace DDDNetCore.Infrastructure.Emails
{
    public interface IEmailAdapter
    {
        
        Task SendEmail(MimeMessage message);

    }
}