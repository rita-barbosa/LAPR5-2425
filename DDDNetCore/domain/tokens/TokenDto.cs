using System;

namespace DDDNetCore.Domain.Tokens{
    public class TokenDto{

            public string UserId { get; set; }

            public string ExpirationTime { get; set; }

            public Guid Code { get; set; }

            public TokenTypeDto TokenType { get; set; }

            public bool Active { get; set; }

    }
}
