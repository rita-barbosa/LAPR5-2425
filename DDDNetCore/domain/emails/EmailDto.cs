using System;

namespace DDDNetCore.Domain.Emails
{
public class EmailDto(string sender, string recipient, string title, string body)
    {
        public string SenderEmail { get; set; } = sender;
        public string RecipientEmail { get; set; } = recipient;
        public string EmailSubject { get; set; } = title;
        public string EmailBody { get; set; } = body;
    }

}