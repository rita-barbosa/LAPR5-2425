using DDDNetCore.Domain.Tokens;
using DDDNetCore.Infrastructure.Shared;

namespace DDDNetCore.Infrastructure.Tokens
{
    public class TokenRepository : BaseRepository<Token, TokenId>, ITokenRepository
    {
    
        public TokenRepository(DDDNetCoreDbContext context):base(context.Tokens)
        {
           
        }


    }
}