using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using MDBackoffice.Domain.Specializations;

namespace MDBackoffice.Infrastructure.Specializations
{
    internal class SpecializationEntityTypeConfiguration : IEntityTypeConfiguration<Specialization>
    {
        public void Configure(EntityTypeBuilder<Specialization> builder)
        {
            builder.HasKey(s => s.Id);

            builder.HasMany(s => s.RequiredStaff)
            .WithOne()
            .HasForeignKey(s => s.SpecializationId)
            .IsRequired()
            .OnDelete(DeleteBehavior.Restrict);;

            builder.ToTable("Specialization");
        }
    }
}