using Microsoft.AspNetCore.Identity;

namespace DDDNetCore.Domain.Users
{

    public class User : IdentityUser
    {
        public bool Status { get; set; }

        public void changeStatus(bool status)
        {
            this.Status = status;
        }
    }
}