using DDDNetCore.Domain.Specializations;
using DDDNetCore.Domain.StaffProfiles;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using Org.BouncyCastle.Asn1.Eac;

namespace DDDNetCore.Infrastructure.StaffProfiles
{
    internal class StaffEntityTypeConfiguration : IEntityTypeConfiguration<Staff>
    {
        public void Configure(EntityTypeBuilder<Staff> builder)
        {
            // Configure the primary key
            builder.HasKey(b => b.Id);

            // Configure LicenseNumber as a value object
            builder.OwnsOne(b => b.LicenseNumber, ln =>
            {
                ln.Property(l => l.Number)
                    .IsRequired()
                    .HasColumnName("LicenseNumber");
            });

            // Configure Name as a value object
            builder.OwnsOne(b => b.Name, n =>
            {
                n.Property(name => name.FirstName)
                    .IsRequired()
                    .HasColumnName("FirstName");
                n.Property(name => name.LastName)
                    .IsRequired()
                    .HasColumnName("LastName");
                n.Property(name => name.FullName)
                    .HasColumnName("FullName");
            });

            builder.OwnsOne(c => c.Phone, pn =>
            {
                pn.Property(p => p.PhoneNumber)
                    .IsRequired() // Ensure PhoneNumber is required
                    .HasColumnName("PhoneNumber");

                pn.Property(p => p.CountryCode)
                    .IsRequired() // Ensure CountryCode is required
                    .HasColumnName("CountryCode");
            });

            builder.OwnsOne(c => c.Email, ea =>
            {
                ea.Property(e => e.EmailAddress)
                    .IsRequired() // Ensure EmailAddress is required
                    .HasColumnName("EmailAddress");
            });

            // builder.OwnsOne(b => b.ContactInfo, ci =>
            // {
            //     ci.OwnsOne(c => c.PhoneNumber, pn =>
            //     {
            //         pn.Property(p => p.PhoneNumber)
            //             .IsRequired() // Ensure PhoneNumber is required
            //             .HasColumnName("PhoneNumber");

            //         pn.Property(p => p.CountryCode)
            //             .IsRequired() // Ensure CountryCode is required
            //             .HasColumnName("CountryCode");
            //     });

            //     ci.OwnsOne(c => c.EmailAdress, ea =>
            //     {
            //         ea.Property(e => e.EmailAddress)
            //             .IsRequired() // Ensure EmailAddress is required
            //             .HasColumnName("EmailAddress");
            //     });
            // });


            // Configure Function as a value object
            builder.OwnsOne(b => b.Function, f =>
            {
                f.Property(func => func.Description) // Assuming Function is a value object with a property `Value`
                    .IsRequired()
                    .HasColumnName("Function");
            });

            builder.HasOne<Specialization>()
                .WithMany()
                .HasForeignKey(b => b.SpecializationId)
                .IsRequired();

            builder.OwnsMany(b => b.Slots, slotBuilder =>
            {
                slotBuilder.WithOwner();

                slotBuilder.OwnsOne(s => s.TimeInterval, intervalBuilder =>
                {
                    intervalBuilder.Property(i => i.Start)
                        .IsRequired()
                        .HasColumnName("StartTime");

                    intervalBuilder.Property(i => i.End)
                        .IsRequired()
                        .HasColumnName("EndTime");
                });

                slotBuilder.OwnsOne(s => s.Date, intervalBuilder =>
                {
                    intervalBuilder.Property(i => i.Start)
                        .IsRequired()
                        .HasColumnName("StartDate");

                    intervalBuilder.Property(i => i.End)
                        .IsRequired()
                        .HasColumnName("EndDate");
                });
                slotBuilder.ToTable("StaffSlots");
            });
            // Configure the table name for Staff
            builder.ToTable("Staff");
        }
    }
}
