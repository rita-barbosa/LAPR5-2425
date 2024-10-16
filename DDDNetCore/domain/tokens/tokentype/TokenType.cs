using System;
using DDDNetCore.Domain.Shared;

namespace DDDNetCore.Domain.Tokens
{
        public class TokenType : Entity<TokenTypeId>, IAggregateRoot {

        public TokenTypeId Code {get; set;}

        public string Name { get; set; }
        public int ExpirationDuration { get; set; }

            private TokenType(){}

            public TokenType(string name, int expirationHours) {
                this.Code = new TokenTypeId(Guid.NewGuid());
                this.Name = name;
                this.ExpirationDuration = expirationHours;
            }


            public void ChangeName(string name){
                this.Name = name;
            }

            public void ChangeExpirationDuration(int time){
                this.ExpirationDuration = time;
            }

        }

}