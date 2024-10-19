using Microsoft.EntityFrameworkCore;
using DDDNetCore.Domain.Categories;
using DDDNetCore.Domain.Products;
using DDDNetCore.Domain.Families;
using DDDNetCore.Domain.Tokens;
using DDDNetCore.Domain.Specializations;
using DDDNetCore.Infrastructure.Tokens;
using DDDNetCore.Infrastructure.Categories;
using DDDNetCore.Infrastructure.Products;
using DDDNetCore.Infrastructure.Specializations;
using DDDNetCore.Infrastructure.StaffProfiles;
using DDDNetCore.Domain.StaffProfiles;
using DDDNetCore.Domain.Patients;
using DDDNetCore.Domain.Users;
using Microsoft.AspNetCore.Identity.EntityFrameworkCore;


namespace DDDNetCore.Infrastructure
{
    public class DDDNetCoreDbContext : IdentityDbContext<User, Role, string>
    {
        public DbSet<Category> Categories { get; set; }

        public DbSet<Product> Products { get; set; }

        public DbSet<Family> Families { get; set; }

        public DbSet<Token> Tokens { get; set; }
        public DbSet<TokenType> TokenTypes { get; set; }
        public DbSet<Specialization> Specializations { get; set; }
        public DbSet<Staff> StaffProfiles { get; set; }
        public DbSet<Patient> Patients { get; set; }
        public DDDNetCoreDbContext(DbContextOptions options) : base(options)
        {

        }

        protected override void OnModelCreating(ModelBuilder modelBuilder)
        {
            base.OnModelCreating(modelBuilder);
            modelBuilder.ApplyConfiguration(new CategoryEntityTypeConfiguration());
            modelBuilder.ApplyConfiguration(new ProductEntityTypeConfiguration());
            modelBuilder.ApplyConfiguration(new FamilyEntityTypeConfiguration());
            modelBuilder.ApplyConfiguration(new TokenEntityTypeConfiguration());
            modelBuilder.ApplyConfiguration(new TokenTypeEntityTypeConfiguration());
            modelBuilder.ApplyConfiguration(new SpecializationEntityTypeConfiguration());
            modelBuilder.ApplyConfiguration(new StaffEntityTypeConfiguration());
            modelBuilder.ApplyConfiguration(new PatientEntityTypeConfiguration());
        }
    }
}