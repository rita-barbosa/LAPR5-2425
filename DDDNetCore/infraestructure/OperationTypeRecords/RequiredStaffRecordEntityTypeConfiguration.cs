using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using DDDNetCore.Domain.OperationTypesRecords;


namespace DDDNetCore.Infrastructure.OperationTypeRecords
{
    internal class RequiredStaffRecordEntityTypeConfiguration : IEntityTypeConfiguration<RequiredStaffRecord>
    {
        public void Configure(EntityTypeBuilder<RequiredStaffRecord> builder)
        {
            // primary key
            builder.HasKey(b => b.RequiredStaffRecordId);
        
            builder.OwnsOne(b => b.Function, f =>
            {
                f.Property(func => func.Description)
                    .IsRequired()
                    .HasColumnName("Function");
            });

            builder.OwnsOne(b => b.StaffQuantity, f =>
            {
                f.Property(func => func.NumberRequired)
                    .IsRequired()
                    .HasColumnName("NumberRequired");
            });

            builder.ToTable("RequiredStaffRecords");
        
        }
    }
}
