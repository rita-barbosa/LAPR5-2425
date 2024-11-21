using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using MDBackoffice.Domain.Appointments;
using MDBackoffice.Domain.OperationRequests;
using MDBackoffice.Domain.Rooms;

namespace MDBackoffice.Infrastructure.Appointments
{
    internal class AppointmentTypeConfiguration : IEntityTypeConfiguration<Appointment>
    {
        public void Configure(EntityTypeBuilder<Appointment> builder)
        {
            //primary key
            builder.HasKey(b => b.Id);

            // Status as a value object
            builder.OwnsOne(a => a.Status, s =>
            {
                s.Property(status => status.Description)
                 .IsRequired()
                 .HasColumnName("Status");
            });

            // OperationRequestId as a reference
            builder.HasOne<OperationRequest>()
                   .WithMany()
                   .HasForeignKey(a => a.OperationRequestId)
                   .IsRequired();

            // RoomNumber as a value object
            builder.HasOne<Room>()
                   .WithMany()
                   .HasForeignKey(a => a.RoomNumber)
                   .IsRequired();

            // Slot as a value object
            builder.OwnsOne(b => b.Slot, slotBuilder =>
            {
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
                slotBuilder.Property(s => s.Description)
                     .HasColumnName("Description");
            });

            // Configure the table name for Appointment
            builder.ToTable("Appointment");
        }
    }

}
