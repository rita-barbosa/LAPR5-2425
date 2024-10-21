using System.Threading.Tasks;
using MimeKit;
using MimeKit.Text;
using MailKit.Security;
using System.Net.Http;
using System.Net.Http.Headers;
using Newtonsoft.Json.Linq;
using System;
using System.Collections.Generic;
using DDDNetCore.Domain.Tokens;
using DDDNetCore.Domain.Shared;
using DDDNetCore.Infrastructure.Emails;

namespace DDDNetCore.Domain.Emails
{
    public class EmailService
    {

        private readonly TokenService _service;

        private readonly SendEmailGoogleAdapter _adapter;

        public EmailService(TokenService service, SendEmailGoogleAdapter adapter)
        {
            this._service = service;
            this._adapter = adapter;
        }


        public async Task SendEmail(EmailMessageDto emailDto) {

            TokenDto token = await _service.CreateAccountDeletionToken(emailDto.RecipientEmail);

            //to confirm email formatting
            var senderEmail = new Email(emailDto.SenderEmail);
            var recipientEmail = new Email(emailDto.RecipientEmail);

            var email = new MimeMessage();
            email.From.Add(new MailboxAddress("HealthCare Clinic", senderEmail.EmailAddress));
            email.To.Add(MailboxAddress.Parse(recipientEmail.EmailAddress));
            email.Subject = emailDto.EmailSubject;

            // can be TextFormat Plain but changing it to Html allow us to better structure the email content 
            var appendix = "\nToken: " + token.Code.ToString() + "\n";
            email.Body = new TextPart(TextFormat.Html) { Text = emailDto.EmailBody + appendix };

            await _adapter.SendEmail(email);
        }


    }

}