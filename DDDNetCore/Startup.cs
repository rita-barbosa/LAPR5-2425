using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.EntityFrameworkCore;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using Microsoft.EntityFrameworkCore.Storage.ValueConversion;
using DDDNetCore.Infrastructure;
using DDDNetCore.Infrastructure.Categories;
using DDDNetCore.Infrastructure.Products;
using DDDNetCore.Infrastructure.Families;
using DDDNetCore.Infrastructure.Tokens;
using DDDNetCore.Infrastructure.Shared;
using DDDNetCore.Infrastructure.StaffProfiles;
using DDDNetCore.Domain.Shared;
using DDDNetCore.Domain.Categories;
using DDDNetCore.Domain.Products;
using DDDNetCore.Domain.Families;
using DDDNetCore.Domain.Tokens;
using DDDNetCore.Domain.Emails;
using DDDNetCore.Domain.StaffProfiles;
using DDDNetCore.Domain.Patients;
using Microsoft.AspNetCore.Identity;
using DDDNetCore.Domain.Users;
using Microsoft.IdentityModel.Tokens;
using System.Text;
using Microsoft.AspNetCore.Authentication.JwtBearer;
using DDDNetCore.Domain.OperationTypes;
using DDDNetCore.Infrastructure.OperationTypes;
using DDDNetCore.Domain.Specializations;
using DDDNetCore.Infrastructure.Specializations;
using DDDNetCore.Domain.OperationRequest;
using DDDNetCore.Infrastructure.OperationRequests;
using DDDNetCore.Infrastructure.Patients;
using DDDNetCore.Infrastructure.Emails;
using Microsoft.AspNetCore.Identity.UI.Services;

namespace DDDNetCore
{
    public class Startup
    {
        public Startup(IConfiguration configuration)
        {
            Configuration = configuration;
        }

        public IConfiguration Configuration { get; }

        // This method gets called by the runtime. Use this method to add services to the container.
        public void ConfigureServices(IServiceCollection services)
        {
            services.AddAuthentication(JwtBearerDefaults.AuthenticationScheme)
                .AddJwtBearer(options =>
                {
                    options.TokenValidationParameters = new TokenValidationParameters
                    {
                        ValidateIssuer = true,
                        ValidateAudience = true,
                        ValidateLifetime = true,
                        ValidateIssuerSigningKey = true,
                        ValidIssuer = Configuration["Jwt:Issuer"],
                        ValidAudience = Configuration["Jwt:Audience"],
                        IssuerSigningKey = new SymmetricSecurityKey(Encoding.UTF8.GetBytes(Configuration["Jwt:Key"]))
                    };
                });
            services.AddAuthorization(options =>
                     {
                         options.AddPolicy("Admin", policy => policy.RequireRole("Admin")); // Require Admin role
                         options.AddPolicy("Doctor", policy => policy.RequireRole("Doctor")); // Require Doctor role
                         options.AddPolicy("Technician", policy => policy.RequireRole("Technician")); // Require Technician role
                         options.AddPolicy("Nurse", policy => policy.RequireRole("Nurse")); // Require Nurse role
                         options.AddPolicy("Patient", policy => policy.RequireRole("Patient")); // Require Patient role
                         options.AddPolicy("AuthenticatedUsers", policy => policy.RequireRole("Admin", "Nurse", "Doctor", "Technician", "Patient"));
                         options.AddPolicy("Staff", policy => policy.RequireRole("Admin", "Nurse", "Doctor", "Technician"));
                     });
            services.AddIdentityCore<User>(options =>
            {
                // Password settings
                options.Password.RequireDigit = true;
                options.Password.RequiredLength = 10; // Minimum length
                options.Password.RequireLowercase = true; // At least 1 lowercase letter
                options.Password.RequireUppercase = true; // At least 1 uppercase letter
                options.Password.RequireNonAlphanumeric = true; // At least 1 special character
            })
                .AddRoles<Role>()
                .AddEntityFrameworkStores<DDDNetCoreDbContext>()
                .AddDefaultTokenProviders();

            services.AddDbContext<DDDNetCoreDbContext>(opt =>
                opt.UseSqlite(Configuration.GetConnectionString("DefaultConnection"))
                .ReplaceService<IValueConverterSelector, StronglyEntityIdValueConverterSelector>());

            ConfigureMyServices(services);

            services.AddControllers().AddNewtonsoftJson();
            services.AddSwaggerGen();
        }
        public void SeedData(IApplicationBuilder app)
        {
            using IServiceScope scope = app.ApplicationServices.CreateScope();
            RoleManager<Role> roleManager = scope.ServiceProvider.GetRequiredService<RoleManager<Role>>();
            SpecializationService specializationService = scope.ServiceProvider.GetRequiredService<SpecializationService>();
            UserManager<User> userManager = scope.ServiceProvider.GetRequiredService<UserManager<User>>();

            string[] roleNames = { "Admin", "Technician", "Doctor", "Nurse", "Patient" };
            foreach (string roleName in roleNames)
            {
                if (!roleManager.RoleExistsAsync(roleName).GetAwaiter().GetResult())
                {
                    roleManager.CreateAsync(new Role(roleName)).GetAwaiter().GetResult();
                }
            }

        }

        public void Configure(IApplicationBuilder app, IWebHostEnvironment env)
        {
            if (env.IsDevelopment())
            {
                app.UseDeveloperExceptionPage();
                app.UseSwagger();
                app.UseSwaggerUI();
            }
            else
            {
                app.UseHsts();
            }

            app.UseHttpsRedirection();
            app.UseRouting();

            //DON'T CHANGE ORDER
            app.UseAuthentication();
            app.UseAuthorization();

            app.UseEndpoints(endpoints =>
            {
                endpoints.MapControllers();
            });

            SeedData(app);
        }

        public void ConfigureMyServices(IServiceCollection services)
        {
            services.AddTransient<IUnitOfWork, UnitOfWork>();

            services.AddTransient<ICategoryRepository, CategoryRepository>();
            services.AddTransient<CategoryService>();

            services.AddTransient<IProductRepository, ProductRepository>();
            services.AddTransient<ProductService>();

            services.AddTransient<IFamilyRepository, FamilyRepository>();
            services.AddTransient<FamilyService>();

            services.AddTransient<EmailService>();

            services.AddTransient<ITokenRepository, TokenRepository>();
            services.AddTransient<TokenService>();
            services.AddTransient<IEmailAdapter, SendEmailGoogleAdapter>();

            services.AddTransient<IStaffRepository, StaffRepository>();
            services.AddTransient<StaffService>();

            services.AddTransient<IPatientRepository, PatientRepository>();
            services.AddTransient<PatientService>();

            services.AddTransient<IOperationTypeRepository, OperationTypeRepository>();
            services.AddTransient<OperationTypeService>();

            services.AddTransient<ISpecializationRepository, SpecializationRepository>();
            services.AddTransient<SpecializationService>();

            services.AddTransient<IOperationRequestRepository, OperationRequestRepository>();
            services.AddTransient<OperationRequestService>();
            services.AddTransient<UserService>();
        }


    }
}
