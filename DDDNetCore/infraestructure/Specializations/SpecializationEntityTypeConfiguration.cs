using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using DDDNetCore.Domain.Specializations;

namespace DDDNetCore.Infrastructure.Specializations
{
    internal class SpecializationEntityTypeConfiguration : IEntityTypeConfiguration<Specialization>
    {
        public void Configure(EntityTypeBuilder<Specialization> builder)
        {
            builder.HasKey(b => b.Id);

            builder.OwnsOne(d => d.Denomination, dn =>
            {
                dn.Property(denomination => denomination.Denomination)
                .IsRequired()
                .HasColumnName("Denomination");
            });

            builder.ToTable("Specialization");
        }
    }
}