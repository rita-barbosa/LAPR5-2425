using DDDNetCore.Domain.Shared;


namespace DDDNetCore.Domain.Specializations
{
    public interface ISpecializationRepository:IRepository<Specialization,SpecializationDenomination>
    {

    }
}