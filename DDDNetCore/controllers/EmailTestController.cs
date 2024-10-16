using DDDNetCore.Domain.Emails;
using Microsoft.AspNetCore.Mvc;
using System.Threading.Tasks;

namespace DDDNetCore.Controllers
{
    [Route("api/[controller]")]
    [ApiController]
    public class EmailTestController : ControllerBase
    {
        private readonly EmailService _service;

        public EmailTestController(EmailService service)
        {
            _service = service;
        }

        [HttpPost("send-email")]
        public async Task<IActionResult> SendTestEmail()
        {
        await _service.SendEmail(new EmailDto
            (
                "noreply.healthcare.dg38@gmail.com",
                "matildexv.04@gmail.com",
                "Test Email",
                "<p>Hello,</p><p>This email was sent to inform you that your user account in the HealthCare Clinic System has been deleted. Some non-identifiable data will be retained in our system for 30 days, as per GDPR policies.</p><p><a href='https://blog.postman.com/rest-api-examples/'>Click in the link to confirm the account deletion</a></p><p>Thank you for choosing us,<br>HealthCare Clinic</p></body></html>"
            ));

            return Ok("Email sent successfully");
        }
    }
}
