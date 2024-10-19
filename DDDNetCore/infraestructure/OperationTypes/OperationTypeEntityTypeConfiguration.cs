using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using DDDNetCore.Domain.OperationTypes;
using DDDNetCore.Domain.Specializations;

namespace DDDNetCore.Infrastructure.OperationTypes
{
    internal class OperationTypeEntityTypeConfiguration : IEntityTypeConfiguration<OperationType>
    {
        public void Configure(EntityTypeBuilder<OperationType> builder)
        {
            // primary key
            builder.HasKey(b => b.Id);
        
            // OperationTypeName as value object
            builder.OwnsOne(b => b.Name, n =>
            {
                n.Property(name => name.OperationName)
                    .IsRequired()
                    .HasColumnName("OperationTypeName");
            });

            // EstimatedDuration as value object
            builder.OwnsOne(b => b.EstimatedDuration, ed =>
            {
                ed.Property(duration => duration.TotalDurationMinutes)
                    .IsRequired()
                    .HasColumnName("EstimatedDuration");
            });

            // OperationTypeStatus as value object
            builder.OwnsOne(b => b.Status, st =>
            {
                st.Property(status => status.Active)
                    .IsRequired()
                    .HasColumnName("OperationTypeStatus");
            });
        
            // OperationTypeStatus as value object
            builder.OwnsOne(b => b.Status, st =>
            {
                st.Property(status => status.Active)
                    .IsRequired()
                    .HasColumnName("OperationTypeStatus");
            });


            // RequiredStaff is a collection of local entities
             builder.OwnsMany(b => b.RequiredStaff, requiredStaffBuilder =>
            {
                requiredStaffBuilder.WithOwner();

                requiredStaffBuilder.OwnsOne(s => s.StaffQuantity, qt =>
                {
                    qt.Property(q => q.NumberRequired)
                        .IsRequired()
                        .HasColumnName("StaffQuantity");
                });

                requiredStaffBuilder.OwnsOne(s => s.Function, ft =>
                {
                    ft.Property(f => f.Description)
                        .IsRequired()
                        .HasColumnName("Function");
                });

                requiredStaffBuilder.HasOne<Specialization>()
                .WithMany()
                .HasForeignKey(b => b.SpecializationId)
                .IsRequired();
                
                requiredStaffBuilder.ToTable("RequiredStaff");
            });


            // Phases is a collection of value objects
            builder.OwnsMany(o => o.Phases, phaseBuilder =>
            {

                phaseBuilder.WithOwner();

                 phaseBuilder.OwnsOne(p => p.Description, phase =>
                {
                    phase.Property(p => p.Description)
                        .IsRequired()
                        .HasColumnName("PhaseDescription");
                });

                phaseBuilder.OwnsOne(p => p.Duration, phase =>
                {
                    phase.Property(p => p.DurationMinutes)
                        .IsRequired()
                        .HasColumnName("PhaseDuration");
                });

            });


            // Configure the table name for Staff
            builder.ToTable("OperationType");
        
        }
    }
}
