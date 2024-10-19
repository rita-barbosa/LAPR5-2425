using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using DDDNetCore.Domain.Patients;

namespace DDDNetCore.Infrastructure.Products
{
    internal class PatientEntityTypeConfiguration : IEntityTypeConfiguration<Patient>
    {
        public void Configure(EntityTypeBuilder<Patient> builder)
        {
            //builder.ToTable("Products", SchemaNames.DDDNetCore);
            builder.HasKey(b => b.Id);
            builder.OwnsOne(b => b.Name, n =>
           {
               n.Property(name => name.FirstName)
                   .IsRequired()
                   .HasColumnName("FirstName");
               n.Property(name => name.LastName)
                   .IsRequired()
                   .HasColumnName("LastName");
               n.Property(name => name.FullName)
                   .IsRequired()
                   .HasColumnName("FullName");
           });

            builder.OwnsOne(c => c.PhoneNumber, pn =>
             {
                 pn.Property(p => p.PhoneNumber)
                     .IsRequired()
                     .HasColumnName("PhoneNumber");

                 pn.Property(p => p.CountryCode)
                     .IsRequired()
                     .HasColumnName("CountryCode");
             });
            builder.OwnsOne(c => c.EmergencyContact, pn =>
            {
                pn.Property(p => p.PhoneNumber)
                    .IsRequired()
                    .HasColumnName("PhoneNumber");

                pn.Property(p => p.CountryCode)
                    .IsRequired()
                    .HasColumnName("CountryCode");
            });
            builder.OwnsOne(c => c.Email, ea =>
           {
               ea.Property(e => e.EmailAddress)
                   .IsRequired()
                   .HasColumnName("EmailAddress");
           });

            builder.OwnsOne(b => b.Gender, f =>
            {
                f.Property(func => func.Denomination)
                    .IsRequired()
                    .HasColumnName("Gender");
            });

            builder.OwnsOne(b => b.MedicalCondition, f =>
            {
                f.Property(func => func.Description)
                    .HasColumnName("MedicalCondition");
            });

            builder.Property(e => e.DateBirth)
                .IsRequired()
                .HasColumnType("DateBirth");



        }
    }
}