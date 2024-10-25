using Microsoft.EntityFrameworkCore;
using DDDNetCore.Domain.Tokens;
using DDDNetCore.Domain.Specializations;
using DDDNetCore.Domain.OperationTypes;
using DDDNetCore.Domain.OperationRequest;
using DDDNetCore.Infrastructure.Tokens;
using DDDNetCore.Infrastructure.Products;
using DDDNetCore.Infrastructure.Specializations;
using DDDNetCore.Infrastructure.StaffProfiles;
using DDDNetCore.Domain.StaffProfiles;
using DDDNetCore.Domain.Patients;
using DDDNetCore.Domain.Users;
using Microsoft.AspNetCore.Identity.EntityFrameworkCore;
using DDDNetCore.Infrastructure.OperationTypes;
using DDDNetCore.Infrastructure.OperationRequests;
using DDDNetCore.Domain.OperationTypes.ValueObjects.RequiredStaff;
using DDDNetCore.Domain.Logs;
using DDDNetCore.Infrastructure.EntityConfigurations;
using Microsoft.EntityFrameworkCore.Internal;


namespace DDDNetCore.Infrastructure
{
    public class DDDNetCoreDbContext : IdentityDbContext<User, Role, string>
    {
        public DbSet<Token> Tokens { get; set; }
        public DbSet<Specialization> Specializations { get; set; }
        public DbSet<OperationType> OperationTypes { get; set; }
        public DbSet<Staff> StaffProfiles { get; set; }
        public DbSet<Patient> Patients { get; set; }
        public DbSet<RequiredStaff> RequiredStaff { get; set; }
        public DbSet<OperationRequest> OperationRequests { get; set; }
        public DbSet<Log> Logs { get; set; }
        public DbSet<AppointmentHistory> appointmentHistories { get; set; }
        public DDDNetCoreDbContext(DbContextOptions options) : base(options)
        {

        }

        protected override void OnModelCreating(ModelBuilder modelBuilder)
        {
            base.OnModelCreating(modelBuilder);
            modelBuilder.ApplyConfiguration(new TokenEntityTypeConfiguration());
            modelBuilder.ApplyConfiguration(new SpecializationEntityTypeConfiguration());
            modelBuilder.ApplyConfiguration(new OperationTypeEntityTypeConfiguration());
            modelBuilder.ApplyConfiguration(new StaffEntityTypeConfiguration());
            modelBuilder.ApplyConfiguration(new PatientEntityTypeConfiguration());
            modelBuilder.ApplyConfiguration(new RequiredStaffEntityTypeConfiguration());
            modelBuilder.ApplyConfiguration(new OperationRequestEntityTypeConfiguration());
            modelBuilder.ApplyConfiguration(new LogEntityTypeConfiguration());
            modelBuilder.ApplyConfiguration(new AppointmentHistoryConfiguration());
        }
    }
}