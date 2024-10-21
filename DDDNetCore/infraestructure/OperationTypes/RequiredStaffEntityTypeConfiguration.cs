using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using DDDNetCore.Domain.OperationTypes.ValueObjects.RequiredStaff;


namespace DDDNetCore.Infrastructure.OperationTypes
{
    internal class RequiredStaffEntityTypeConfiguration : IEntityTypeConfiguration<RequiredStaff>
    {
        public void Configure(EntityTypeBuilder<RequiredStaff> builder)
        {
            // primary key
            builder.HasKey(b => b.Id);
        
            builder.OwnsOne(b => b.Function, f =>
            {
                f.Property(func => func.Description)
                    .IsRequired()
                    .HasColumnName("Function");
            });

            builder.OwnsOne(s => s.StaffQuantity);

            builder.ToTable("RequiredStaff");
        
        }
    }
}
