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

namespace DDDNetCore.Domain.Emails
{
    public class EmailService
    {

        private readonly TokenService _service;
        private static readonly string CLIENT_ID = "643981681426-pdruu3r0crj9nstjg5lgfnrntrog4ahd.apps.googleusercontent.com";
        private static readonly string CLIENT_SECRET = "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";
        private static readonly string REFRESH_TOKEN = "1//0402TzyVzbgWNCgYIARAAGAQSNwF-L9IrAGOptUEkMoSsLM614oCTO4pj3UKZevBtExzkRRFlonZMGV-Ox62E4TarWEulBsv34Lo";
        private static readonly string TOKEN_END_POINT = "https://oauth2.googleapis.com/token";

        public EmailService(TokenService service)
        {
            this._service = service;
        }


        public async Task SendEmail(EmailDto emailDto) {

            var token = await _service.CreateAccountDeletionToken(emailDto.RecipientEmail);

            string accessToken = await GetAccessToken();

            var email = new MimeMessage();
            email.From.Add(new MailboxAddress("HealthCare Clinic", emailDto.SenderEmail));
            email.To.Add(MailboxAddress.Parse(emailDto.RecipientEmail));
            email.Subject = emailDto.EmailSubject;

            // can be TextFormat Plain but changing it to Html allow us to better structure the email content 
            var appendix = "\nToken: " + token.Code.ToString() + "\n";
            email.Body = new TextPart(TextFormat.Html) { Text = (emailDto.EmailBody + appendix) };

            using var smtp = new MailKit.Net.Smtp.SmtpClient();
            smtp.Connect("smtp.gmail.com", 587, SecureSocketOptions.StartTls);
            var oauth2 = new SaslMechanismOAuth2("noreply.healthcare.dg38@gmail.com", accessToken);
            smtp.Authenticate(oauth2);
            smtp.Send(email);
            smtp.Disconnect(true);

            Console.WriteLine("Email sent successfully!");
        }

        private async Task<string> GetAccessToken()
        {
            var client = new HttpClient();
            var request = new HttpRequestMessage(HttpMethod.Post, TOKEN_END_POINT);

            var body = new Dictionary<string, string>
            {
                { "client_id", CLIENT_ID },
                { "client_secret", CLIENT_SECRET },
                { "refresh_token", REFRESH_TOKEN },
                { "grant_type", "refresh_token" }
            };

            request.Content = new FormUrlEncodedContent(body);
            request.Headers.Accept.Add(new MediaTypeWithQualityHeaderValue("application/json"));

            var response = await client.SendAsync(request);

            if (!response.IsSuccessStatusCode)
            {
                throw new System.Exception($"Failed to refresh token: {response.StatusCode}");
            }

            var content = await response.Content.ReadAsStringAsync();
            var tokenData = JObject.Parse(content);

            return tokenData["access_token"].ToString();
        }

    }

}