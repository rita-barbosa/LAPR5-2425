
using DDDNetCore.Domain.Shared;

namespace DDDNetCore.Domain.Tokens
{
    public interface ITokenRepository: IRepository<Token, TokenId>
    {
    }
}