using DDDNetCore.Domain.Shared;

namespace DDDNetCore.Domain.Shared
{
    public class IdPassDto
    {
        public string Id { get; set; }
    
        public IdPassDto(string id)
        {
            Id = id;
        }

    }
}
