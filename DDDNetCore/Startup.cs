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
            services.AddDbContext<DDDNetCoreDbContext>(opt =>
                opt.UseSqlite(Configuration.GetConnectionString("DefaultConnection"))
                .ReplaceService<IValueConverterSelector, StronglyEntityIdValueConverterSelector>());

            ConfigureMyServices(services);

            services.AddControllers().AddNewtonsoftJson();
            services.AddSwaggerGen();
        }

        // This method gets called by the runtime. Use this method to configure the HTTP request pipeline.
        public void Configure(IApplicationBuilder app, IWebHostEnvironment env)
        {
            if (env.IsDevelopment())
            {
                app.UseDeveloperExceptionPage();
            }
            else
            {
                // The default HSTS value is 30 days. You may want to change this for production scenarios, see https://aka.ms/aspnetcore-hsts.
                app.UseHsts();
            }

            // Enable middleware to serve Swagger-generated JSON endpoints
            app.UseSwagger();

            // Enable middleware to serve Swagger UI (HTML page)
            // Available at /swagger/index.html
            app.UseSwaggerUI();

            app.UseHttpsRedirection();

            app.UseRouting();

            app.UseAuthorization();

            app.UseEndpoints(endpoints =>
            {
                endpoints.MapControllers();
            });
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

            services.AddTransient<ITokenTypeRepository, TokenTypeRepository>();
            services.AddTransient<TokenTypeService>();

            services.AddTransient<IStaffRepository, StaffRepository>();
            services.AddTransient<StaffService>();
        }
    }
}
