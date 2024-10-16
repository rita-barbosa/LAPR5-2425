using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using DDDNetCore.Domain.Tokens;

namespace DDDNetCore.Infrastructure.Tokens
{
    internal class TokenTypeEntityTypeConfiguration : IEntityTypeConfiguration<TokenType>
    {

        public void Configure(EntityTypeBuilder<TokenType> builder)
        {
            // cf. https://www.entityframeworktutorial.net/efcore/fluent-api-in-entity-framework-core.aspx
            
            //builder.ToTable("Categories", SchemaNames.DDDNetCore);
            builder.HasKey(e => e.Code);
            //builder.Property<bool>("_active").HasColumnName("Active");
        }

    }
}