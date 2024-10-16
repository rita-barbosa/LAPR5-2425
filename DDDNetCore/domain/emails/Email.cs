using System;
using DDDNetCore.Domain.Shared;

namespace DDDNetCore.Domain.Emails
{
    public class Email : Entity<EmailId>, IAggregateRoot
    {
     
        public string Description { get;  private set; }

        public bool Active{ get;  private set; }

        private Email()
        {
            this.Active = true;
        }

        public Email(string description)
        {
            this.Id = new EmailId(Guid.NewGuid());
            this.Description = description;
            this.Active = true;
        }

        public void ChangeDescription(string description)
        {
            if (!this.Active)
                throw new BusinessRuleValidationException("It is not possible to change the description to an inactive Email.");
            this.Description = description;
        }
        public void MarkAsInative()
        {
            this.Active = false;
        }
    }
}