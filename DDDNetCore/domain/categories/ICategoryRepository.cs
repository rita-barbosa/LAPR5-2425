
using DDDNetCore.Domain.Shared;

namespace DDDNetCore.Domain.Categories
{
    public interface ICategoryRepository: IRepository<Category, CategoryId>
    {
    }
}