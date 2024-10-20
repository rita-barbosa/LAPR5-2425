using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using DDDNetCore.Domain.Specializations;

namespace DDDNetCore.Infrastructure.Specializations
{
    internal class SpecializationEntityTypeConfiguration : IEntityTypeConfiguration<Specialization>
    {
        public void Configure(EntityTypeBuilder<Specialization> builder)
        {
            builder.HasKey(s => s.Id);

            builder.HasMany(s => s.RequiredStaff)
            .WithOne()
            .HasForeignKey(s => s.SpecializationId)
            .IsRequired();

            builder.ToTable("Specialization");
        }
    }
}