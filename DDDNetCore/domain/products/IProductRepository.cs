using DDDNetCore.Domain.Shared;

namespace DDDNetCore.Domain.Products
{
    public interface IProductRepository: IRepository<Product,ProductId>
    {
    }
}