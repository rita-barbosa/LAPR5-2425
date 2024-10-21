using System;
using System.Threading.Tasks;
using DDDNetCore.Domain.Shared;
using DDDNetCore.Domain.Specializations;

namespace DDDNetCore.Domain.StaffProfiles
{
    public class StaffService
    {
        private readonly IUnitOfWork _unitOfWork;
        private readonly IStaffRepository _repo;
        private readonly ISpecializationRepository _repoSpec;

        public StaffService(IUnitOfWork unitOfWork, IStaffRepository repo, ISpecializationRepository repoSpec)
        {
            this._unitOfWork = unitOfWork;
            this._repo = repo;
            this._repoSpec = repoSpec;
        }

        public async Task<StaffDto> CreateStaffProfile(CreatingStaffDto dto)
        {

            if (await _repo.ExistsStaffWithEmailOrPhone(dto.Email, dto.Phone.Split(' ')[0], dto.Phone.Split(' ')[1]))
            {
                throw new BusinessRuleValidationException("There already exists a staff member with that email or phone.");
            }

            Specialization spec = await _repoSpec.GetByIdAsync(new SpecializationDenomination(dto.SpecializationId)) ??
                throw new BusinessRuleValidationException("Invalid specialization.");

            var seqNumber = await getSequentialNumber();

            Function? function = Function.GetFunctionByDescription(dto.Function) ??
                throw new BusinessRuleValidationException("Invalid function.");

            var staff = new Staff(seqNumber, dto.LicenseNumber, dto.FirstName, dto.LastName, dto.Email,
            dto.Phone, function, spec.Id);

            await this._repo.AddAsync(staff);

            await this._unitOfWork.CommitAsync();

            return new StaffDto(staff.Name.ToString(), staff.Phone.ToString(), staff.Email.ToString(), staff.Id.AsString());
        }

        private async Task<string> getSequentialNumber()
        {
            try
            {
                StaffId staffId = await _repo.FindLastStaffIdAsync();
                string lastSequentialNumber = staffId.AsString().Substring(5);
                int newSequentialNumber = int.Parse(lastSequentialNumber) + 1;
                return newSequentialNumber.ToString("D5");
            }
            catch (NullReferenceException)
            {
                return "00001";
            }
        }
    }
}
