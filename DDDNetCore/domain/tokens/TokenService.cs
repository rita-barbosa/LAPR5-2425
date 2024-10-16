using System.Threading.Tasks;
using System.Collections.Generic;
using DDDNetCore.Domain.Shared;
using System;

namespace DDDNetCore.Domain.Tokens
{
    public class TokenService
    {
        private readonly IUnitOfWork _unitOfWork;
        private readonly ITokenRepository _repo;

        private readonly TokenTypeService _service;

        public TokenService(IUnitOfWork unitOfWork, ITokenRepository repo, TokenTypeService service)
        {
            this._unitOfWork = unitOfWork;
            this._repo = repo;
            this._service = service;
        }

        public async Task<TokenDto> CreateAccountDeletionToken(string username)
        {

            TokenTypeDto tokenTypeDto = await _service.AddAsync(new CreatingTokenTypeDto{ Name = "Account Deletion", ExpirationDuration = 24});

            var token = new Token(username, DateTime.Now.AddHours(tokenTypeDto.ExpirationDuration), new TokenType(tokenTypeDto.Name, tokenTypeDto.ExpirationDuration));
            var dto = new TokenDto{UserId = token.UserId.ToString(), ExpirationTime = token.ExpirationTime.ToString(), Code = token.Code.AsGuid(), TokenType = tokenTypeDto, Active = token.Active};

            await AddAsync(new CreatingTokenDto{UserId = token.UserId.ToString(), ExpirationTime = token.ExpirationTime.ToString(), Code = token.Code.AsGuid(), TokenType = tokenTypeDto, Active = token.Active});
            return dto;
        }


        public async Task<List<TokenDto>> GetAllAsync()
        {
            var list = await this._repo.GetAllAsync();
            
            List<TokenDto> listDto = list.ConvertAll<TokenDto>(token => new TokenDto{UserId = token.UserId.ToString(), ExpirationTime = token.ExpirationTime.ToString(), Code = token.Code.AsGuid(), TokenType = new TokenTypeDto{ Name = token.TokenType.Name, ExpirationDuration = token.TokenType.ExpirationDuration }, Active = token.Active});

            return listDto;
        }

        public async Task<TokenDto> GetByIdAsync(TokenId id)
        {
            var token = await this._repo.GetByIdAsync(id);
            
            if(token == null)
                return null;

            return new TokenDto{UserId = token.UserId.ToString(), ExpirationTime = token.ExpirationTime.ToString(), Code = token.Code.AsGuid(), TokenType = new TokenTypeDto{ Name = token.TokenType.Name, ExpirationDuration = token.TokenType.ExpirationDuration }, Active = token.Active};
        }

        public async Task<TokenDto> AddAsync(CreatingTokenDto dto)
        {

            var Token = new Token(dto.UserId, DateTime.Parse(dto.ExpirationTime), new TokenType(dto.TokenType.Name, dto.TokenType.ExpirationDuration));

            Console.WriteLine("TOKEN user " + Token.UserId.ToString());
            Console.WriteLine("TOKEN exp time " + Token.ExpirationTime.ToString());
            Console.WriteLine("TOKEN code " + Token.Code.AsString());
            Console.WriteLine("TOKEN active " + Token.Active.ToString());
            Console.WriteLine("TOKEN type " + Token.TokenType.ToString());
            await this._repo.AddAsync(Token);

            await this._unitOfWork.CommitAsync();

            return new TokenDto {UserId = Token.UserId.ToString(), ExpirationTime = Token.ExpirationTime.ToString(), Code = Token.Code.AsGuid(), TokenType = dto.TokenType, Active = Token.Active};
        }

        public async Task<TokenDto> UpdateAsync(TokenDto dto)
        {
            var Token = await this._repo.GetByIdAsync(new TokenId(dto.Code)); 

            if (Token == null)
                return null;   

            Token.ChangeUser(dto.UserId);
            Token.ChangeExpirationTime(DateTime.Parse(dto.ExpirationTime));
            Token.ChangeTokenType(new TokenType(dto.TokenType.Name, dto.TokenType.ExpirationDuration));
            
            await this._unitOfWork.CommitAsync();

            return new TokenDto { UserId = Token.UserId.ToString(), ExpirationTime = Token.ExpirationTime.ToString(), Code = Token.Code.AsGuid(), TokenType = dto.TokenType, Active = Token.Active };
        }

        public async Task<TokenDto> InactivateAsync(TokenId id)
        {
            var token = await this._repo.GetByIdAsync(id); 

            if (token == null)
                return null;   

            token.MarkAsInative();
            
            await this._unitOfWork.CommitAsync();

            return new TokenDto { UserId = token.UserId.ToString(), ExpirationTime = token.ExpirationTime.ToString(), Code = token.Code.AsGuid(), TokenType = new TokenTypeDto{ Name = token.TokenType.Name, ExpirationDuration = token.TokenType.ExpirationDuration }, Active = token.Active };
        }

         public async Task<TokenDto> DeleteAsync(TokenId id)
        {
            var Token = await this._repo.GetByIdAsync(id); 

            if (Token == null)
                return null;   

            if (Token.Active)
                throw new BusinessRuleValidationException("It is not possible to delete an active Token.");
            
            this._repo.Remove(Token);
            await this._unitOfWork.CommitAsync();

            return new TokenDto { UserId = Token.UserId.ToString(), ExpirationTime = Token.ExpirationTime.ToString(), Code = Token.Code.AsGuid(), TokenType = new TokenTypeDto{ Name = Token.TokenType.Name, ExpirationDuration = Token.TokenType.ExpirationDuration }, Active = Token.Active };
        }
    }
}