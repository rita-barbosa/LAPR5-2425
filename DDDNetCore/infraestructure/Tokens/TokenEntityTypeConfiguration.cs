using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using DDDNetCore.Domain.Tokens;

namespace DDDNetCore.Infrastructure.Tokens
{
    internal class TokenEntityTypeConfiguration : IEntityTypeConfiguration<Token>
    {
        public void Configure(EntityTypeBuilder<Token> builder)
        {
            // cf. https://www.entityframeworktutorial.net/efcore/fluent-api-in-entity-framework-core.aspx
            
            //builder.ToTable("Categories", SchemaNames.DDDNetCore);
            builder.HasKey(b => b.Code);
            //builder.Property<bool>("_active").HasColumnName("Active");
        }

    }
}