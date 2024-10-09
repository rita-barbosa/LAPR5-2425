using DDDNetCore.Domain.Products;
using DDDNetCore.Infrastructure.Shared;

namespace DDDNetCore.Infrastructure.Products
{
    public class ProductRepository : BaseRepository<Product, ProductId>,IProductRepository
    {
        public ProductRepository(DDDNetCoreDbContext context):base(context.Products)
        {
           
        }
    }
}