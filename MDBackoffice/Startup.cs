using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.EntityFrameworkCore;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using Microsoft.EntityFrameworkCore.Storage.ValueConversion;
using MDBackoffice.Infrastructure;
using MDBackoffice.Infrastructure.Tokens;
using MDBackoffice.Infrastructure.Shared;
using MDBackoffice.Infrastructure.StaffProfiles;
using MDBackoffice.Domain.Shared;
using MDBackoffice.Domain.Tokens;
using MDBackoffice.Domain.Emails;
using MDBackoffice.Domain.StaffProfiles;
using MDBackoffice.Domain.Patients;
using Microsoft.AspNetCore.Identity;
using MDBackoffice.Domain.Users;
using Microsoft.IdentityModel.Tokens;
using System.Text;
using Microsoft.AspNetCore.Authentication.JwtBearer;
using MDBackoffice.Domain.OperationTypes;
using MDBackoffice.Infrastructure.OperationTypes;
using MDBackoffice.Domain.Specializations;
using MDBackoffice.Infrastructure.Specializations;
using MDBackoffice.Domain.OperationRequest;
using MDBackoffice.Infrastructure.OperationRequests;
using MDBackoffice.Infrastructure.Patients;
using MDBackoffice.Infrastructure.Emails;
using MDBackoffice.Domain.Logs;
using MDBackoffice.Infrastructure.Logs;
using MDBackoffice.Domain.OperationTypesRecords;
using MDBackoffice.Infrastructure.OperationTypeRecords;
using System;

namespace MDBackoffice
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
            services.AddIdentity<User,Role>(options =>
            {
                // Password settings
                options.Password.RequireDigit = true;
                options.Password.RequiredLength = 10; // Minimum length
                options.Password.RequireLowercase = true; // At least 1 lowercase letter
                options.Password.RequireUppercase = true; // At least 1 uppercase letter
                options.Password.RequireNonAlphanumeric = true; // At least 1 special character
            })
                .AddRoles<Role>()
                .AddEntityFrameworkStores<MDBackofficeDbContext>()
                .AddDefaultTokenProviders();

            services.Configure<IdentityOptions>(options =>
            {
                options.Lockout.DefaultLockoutTimeSpan = TimeSpan.FromMinutes(5);
                options.Lockout.MaxFailedAccessAttempts = 5;
                options.Lockout.AllowedForNewUsers = true;
            });


            services.AddDbContext<MDBackofficeDbContext>(opt =>
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
                // app.UseSwagger();
                // app.UseSwaggerUI();
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

            services.AddTransient<ILogRepository, LogRepository>();
            services.AddTransient<LogService>();

            services.AddTransient<IOperationTypeRecordRepository, OperationTypeRecordRepository>();
            services.AddTransient<OperationTypeRecordService>();

        }


    }
}
