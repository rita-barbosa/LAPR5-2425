using System.Threading.Tasks;
using System.Collections.Generic;
using DDDNetCore.Domain.Shared;
using System;

namespace DDDNetCore.Domain.Tokens
{
    public class TokenTypeService
    {
        private readonly IUnitOfWork _unitOfWork;
        private readonly ITokenTypeRepository _repo;

        public TokenTypeService(IUnitOfWork unitOfWork, ITokenTypeRepository repo)
        {
            this._unitOfWork = unitOfWork;
            this._repo = repo;
        }


        public async Task<List<TokenTypeDto>> GetAllAsync()
        {
            var list = await this._repo.GetAllAsync();
            
            List<TokenTypeDto> listDto = list.ConvertAll<TokenTypeDto>(type => new TokenTypeDto{Code = type.Code.AsGuid(), Name = type.Name, ExpirationDuration = type.ExpirationDuration});

            return listDto;
        }

        public async Task<TokenTypeDto> GetByIdAsync(TokenTypeId id)
        {
            var type = await this._repo.GetByIdAsync(id);
            
            if(type == null)
                return null;

            return new TokenTypeDto{Code = type.Code.AsGuid(), Name = type.Name, ExpirationDuration = type.ExpirationDuration};
        }

        public async Task<TokenTypeDto> AddAsync(CreatingTokenTypeDto dto)
        {

            var TokenType = new TokenType(dto.Name,  dto.ExpirationDuration);

            Console.WriteLine("TOKEN TYPE name " + TokenType.Name);
            Console.WriteLine("TOKEN exp duration " + TokenType.ExpirationDuration.ToString());
            Console.WriteLine("TOKEN code " + TokenType.Code.AsString());
            await this._repo.AddAsync(TokenType);

            await this._unitOfWork.CommitAsync();

            return new TokenTypeDto {Code = TokenType.Code.AsGuid(), Name = TokenType.Name, ExpirationDuration = TokenType.ExpirationDuration};
        }

        public async Task<TokenTypeDto> UpdateAsync(TokenTypeDto dto)
        {
            var TokenType = await this._repo.GetByIdAsync(new TokenTypeId(dto.Code)); 

            if (TokenType == null)
                return null;   

            TokenType.ChangeName(dto.Name);
            TokenType.ChangeExpirationDuration(dto.ExpirationDuration);
            
            await this._unitOfWork.CommitAsync();

            return new TokenTypeDto { Code = TokenType.Code.AsGuid(), Name = TokenType.Name, ExpirationDuration = TokenType.ExpirationDuration };
        }

         public async Task<TokenTypeDto> DeleteAsync(TokenTypeId id)
        {
            var TokenType = await this._repo.GetByIdAsync(id); 

            if (TokenType == null)
                return null;   

            this._repo.Remove(TokenType);
            await this._unitOfWork.CommitAsync();

            return new TokenTypeDto { Code = TokenType.Code.AsGuid(), Name = TokenType.Name, ExpirationDuration = TokenType.ExpirationDuration };
        }
    }
}