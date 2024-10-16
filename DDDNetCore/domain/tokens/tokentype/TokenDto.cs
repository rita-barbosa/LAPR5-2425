using System;

namespace DDDNetCore.Domain.Tokens{
    public class TokenTypeDto{

            public Guid Code { get; set; }

            public int ExpirationDuration { get; set; }

            public string Name { get; set; }

    }
}
