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

        public TokenService(IUnitOfWork unitOfWork, ITokenRepository repo)
        {
            this._unitOfWork = unitOfWork;
            this._repo = repo;
        }

        public async Task<TokenDto> CreateAccountDeletionToken(string username)
        {
            var token = new Token(username, DateTime.Now.AddHours(TokenType.AccountDeletion.ExpirationDurationHours.Hours), TokenType.AccountDeletion);

            return await AddAsync(new TokenDto{UserId = token.UserId, ExpirationTime = token.ExpirationTime.ToString(), Code = token.Id.AsString(), TokenType = ToDto(TokenType.AccountDeletion), Active = token.Active});   
        }


        public async Task<List<TokenDto>> GetAllAsync()
        {
            var list = await this._repo.GetAllAsync();
            
            List<TokenDto> listDto = list.ConvertAll<TokenDto>(token => new TokenDto{UserId = token.UserId, ExpirationTime = token.ExpirationTime.ToString(), Code = token.Id.AsString(), TokenType = ToDto(token.TokenType), Active = token.Active});

            return listDto;
        }

        public async Task<TokenDto> GetByIdAsync(TokenId id)
        {
            var token = await this._repo.GetByIdAsync(id);
            
            if(token == null)
                return null;

            return new TokenDto{UserId = token.UserId.ToString(), ExpirationTime = token.ExpirationTime.ToString(), Code = token.Id.AsString(), TokenType = ToDto(token.TokenType), Active = token.Active};
        }

        public async Task<TokenDto> AddAsync(TokenDto dto)
        {

            var Token = new Token(dto.UserId, DateTime.Parse(dto.ExpirationTime), new TokenType(dto.TokenType.Name, dto.TokenType.ExpirationDuration));

            // Console.WriteLine("TOKEN user " + Token.UserId.ToString());
            // Console.WriteLine("TOKEN exp time " + Token.ExpirationTime.ToString());
            // Console.WriteLine("TOKEN code " + Token.Id.AsString());
            // Console.WriteLine("TOKEN active " + Token.Active.ToString());
            // Console.WriteLine("TOKEN type " + Token.TokenType.ToString());
            await this._repo.AddAsync(Token);

            await this._unitOfWork.CommitAsync();

            return new TokenDto {UserId = Token.UserId.ToString(), ExpirationTime = Token.ExpirationTime.ToString(), Code = Token.Id.AsString(), TokenType = dto.TokenType, Active = Token.Active};
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

            return new TokenDto { UserId = Token.UserId.ToString(), ExpirationTime = Token.ExpirationTime.ToString(), Code = Token.Id.AsString(), TokenType = dto.TokenType, Active = Token.Active };
        }

        public async Task<TokenDto> InactivateAsync(TokenId id)
        {
            var token = await this._repo.GetByIdAsync(id); 

            if (token == null)
                return null;   

            token.MarkAsInative();
            
            await this._unitOfWork.CommitAsync();

            return new TokenDto { UserId = token.UserId.ToString(), ExpirationTime = token.ExpirationTime.ToString(), Code = token.Id.AsString(), TokenType = ToDto(token.TokenType), Active = token.Active };
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

            return new TokenDto { UserId = Token.UserId.ToString(), ExpirationTime = Token.ExpirationTime.ToString(), Code = Token.Id.AsString(), TokenType = ToDto(Token.TokenType), Active = Token.Active };
        }


        public TokenTypeDto ToDto(TokenType tokenType){
            return new TokenTypeDto( tokenType.ExpirationDurationHours.Hours, tokenType.TypeDenomination.Denomination );
        }
    }
}