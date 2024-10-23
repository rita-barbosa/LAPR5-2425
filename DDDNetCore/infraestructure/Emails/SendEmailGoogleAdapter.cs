using System.Collections.Generic;
using System.Net.Http;
using System.Net.Http.Headers;
using System.Threading.Tasks;
using MailKit.Security;
using MimeKit;
using Newtonsoft.Json.Linq;

namespace DDDNetCore.Infrastructure.Emails
{
    public class SendEmailGoogleAdapter : IEmailAdapter
    {

        private static readonly string CLIENT_ID = "643981681426-pdruu3r0crj9nstjg5lgfnrntrog4ahd.apps.googleusercontent.com";
        private static readonly string CLIENT_SECRET = "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";
        private static readonly string REFRESH_TOKEN = "1//04HvLVYDBtN-QCgYIARAAGAQSNwF-L9IrMe0njlGh_d5X5ZbIgFIrcqYgSjVx7mVNaN4-DlqvqhP-AXcJ0zc_9B6Nnme0WDnPOu0";
        private static readonly string TOKEN_END_POINT = "https://oauth2.googleapis.com/token";
        private static readonly string TEST_USER_EMAIL = "noreply.healthcare.dg38@gmail.com";

        public SendEmailGoogleAdapter(){ }

        public async Task SendEmail(MimeMessage message)
        {
            string accessToken = await GetExternalEmailServiceAccessToken();

            using var smtp = new MailKit.Net.Smtp.SmtpClient();
            smtp.Connect("smtp.gmail.com", 587, SecureSocketOptions.StartTls);
            var oauth2 = new SaslMechanismOAuth2(TEST_USER_EMAIL, accessToken);
            smtp.Authenticate(oauth2);
            smtp.Send(message);
            smtp.Disconnect(true);
        }



         private async Task<string> GetExternalEmailServiceAccessToken()
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
                var errorContent = await response.Content.ReadAsStringAsync();
                throw new System.Exception($"Failed to refresh token: {response.StatusCode}, Details: {errorContent}");
            }

            var content = await response.Content.ReadAsStringAsync();
            var tokenData = JObject.Parse(content);

            return tokenData["access_token"].ToString();
        }


    }


}