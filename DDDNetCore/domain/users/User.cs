using Microsoft.AspNetCore.Identity;

namespace DDDNetCore.Domain.Users
{

    public class User : IdentityUser
    {
        public string? Initials { get; set; }

        public bool Status { get; set; }
    }
}