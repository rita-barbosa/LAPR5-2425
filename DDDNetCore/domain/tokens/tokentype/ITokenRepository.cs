
using DDDNetCore.Domain.Shared;

namespace DDDNetCore.Domain.Tokens
{
    public interface ITokenTypeRepository: IRepository<TokenType, TokenTypeId>
    {
    }
}