using DDDNetCore.Domain.Categories;
using DDDNetCore.Infrastructure.Shared;

namespace DDDNetCore.Infrastructure.Categories
{
    public class CategoryRepository : BaseRepository<Category, CategoryId>, ICategoryRepository
    {
    
        public CategoryRepository(DDDNetCoreDbContext context):base(context.Categories)
        {
           
        }


    }
}