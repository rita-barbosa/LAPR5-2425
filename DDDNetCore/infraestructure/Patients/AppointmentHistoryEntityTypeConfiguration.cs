using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using DDDNetCore.Domain.Patients;

public class AppointmentHistoryConfiguration : IEntityTypeConfiguration<AppointmentHistory>
{
    public void Configure(EntityTypeBuilder<AppointmentHistory> builder)
    {
        // Define the primary key
        builder.HasKey(ah => ah.Id);

        // Define properties for AppointmentHistory
        builder.Property(ah => ah.Status)
            .IsRequired(); // Assuming Status is an enum or another required type

        builder.Property(ah => ah.Type)
            .IsRequired(); // Assuming Type is an enum or another required type

        builder.OwnsOne(b => b.CreatedAt, dr => 
            {
                dr.Property(createdAt => createdAt.Start)
                .IsRequired()
                .HasColumnName("CreatedAt");
            });

        // Configure PatientId
        builder.Property(ah => ah.PatientId) // Just use the whole object here
            .IsRequired(); // PatientId is required and should not be null

        // Configure the relationship with Patient
        builder.HasOne<Patient>()
            .WithMany(p => p.AppointmentList)
            .HasForeignKey(ah => ah.PatientId); // Just use PatientId instead of PatientId.Value
    }
}
