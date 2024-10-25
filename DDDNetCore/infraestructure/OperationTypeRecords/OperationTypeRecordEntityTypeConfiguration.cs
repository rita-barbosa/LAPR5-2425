using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using DDDNetCore.Domain.OperationTypesRecords;
using DDDNetCore.Domain.OperationTypes;


namespace DDDNetCore.Infrastructure.OperationTypeRecords
{
    internal class OperationTypeRecordEntityTypeConfiguration : IEntityTypeConfiguration<OperationTypeRecord>
    {
        public void Configure(EntityTypeBuilder<OperationTypeRecord> builder)
        {
            // primary key
            builder.HasKey(b => b.Id);

            // Version as value object
            builder.OwnsOne(b => b.Version, n =>
            {
                n.Property(version => version.Version)
                    .IsRequired()
                    .HasColumnName("Version");
            });

            // EffectiveDate as value object
             builder.OwnsOne(b => b.EffectiveDate, ef =>
            {
                ef.Property(effectiveDate => effectiveDate.Start)
                .IsRequired()
                .HasColumnName("EffectiveDate");
            });


            //OperationTypeId as value object
            // builder.HasOne<OperationType>()
            //    .WithMany()
            //    .HasForeignKey(b => b.OperationTypeId)
            //    .IsRequired();
        
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

            //RequiredStaffRecords as value object
            builder.HasMany(s => s.RequiredStaffRecords)
            .WithOne()
            .HasForeignKey(s => s.OperationTypeRecordId)
            .IsRequired()
            .OnDelete(DeleteBehavior.Restrict);

            // Phases is a collection of value objects
            builder.OwnsMany(o => o.Phases, phaseBuilder =>
            {

                phaseBuilder.WithOwner().HasForeignKey(p => p.OperationTypeRecordId);

                phaseBuilder.HasKey(p => p.PhaseId);


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
            builder.ToTable("OperationTypeRecords");
        
        }
    }
}
