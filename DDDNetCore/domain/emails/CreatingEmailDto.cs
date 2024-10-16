namespace DDDNetCore.Domain.Emails
{
    public class CreatingEmailDto
    {
        public string Description { get; set; }


        public CreatingEmailDto(string description)
        {
            this.Description = description;
        }
    }
}