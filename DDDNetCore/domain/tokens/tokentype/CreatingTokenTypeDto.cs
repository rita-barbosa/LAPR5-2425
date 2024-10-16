using System;

namespace DDDNetCore.Domain.Tokens
{
    public class CreatingTokenTypeDto
    {

        public string Name { get; set; }

        public int ExpirationDuration { get; set; }

    }
}