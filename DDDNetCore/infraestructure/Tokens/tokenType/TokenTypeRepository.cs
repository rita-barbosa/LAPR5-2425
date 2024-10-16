using DDDNetCore.Domain.Tokens;
using DDDNetCore.Infrastructure.Shared;

namespace DDDNetCore.Infrastructure.Tokens
{
    public class TokenTypeRepository : BaseRepository<TokenType, TokenTypeId>, ITokenTypeRepository
    {
    
        public TokenTypeRepository(DDDNetCoreDbContext context):base(context.TokenTypes)
        {
           
        }


    }
}