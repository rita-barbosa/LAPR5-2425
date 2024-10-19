using DDDNetCore.Domain.Shared;

namespace DDDNetCore.Domain.Patients
{
    public class PatientService
    {
        private readonly IUnitOfWork _unitOfWork;
        private readonly IPatientRepository _repo;

         public PatientService(IUnitOfWork unitOfWork, IPatientRepository repo)
        {
            this._unitOfWork = unitOfWork;
            this._repo = repo;
        }
    }
}