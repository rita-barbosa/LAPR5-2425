using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using DDDNetCore.Domain.Logs;
using DDDNetCore.Domain.Shared;
using DDDNetCore.Domain.Specializations;
using DDDNetCore.Domain.Users;

namespace DDDNetCore.Domain.StaffProfiles
{
    public class StaffService
    {
        private readonly IUnitOfWork _unitOfWork;
        private readonly IStaffRepository _repo;
        private readonly LogService _logService;
        private readonly ISpecializationRepository _repoSpec;

        public StaffService(IUnitOfWork unitOfWork, LogService logService, IStaffRepository repo, ISpecializationRepository repoSpec)
        {
            this._unitOfWork = unitOfWork;
            this._repo = repo;
            this._logService = logService;
            this._repoSpec = repoSpec;
        }
        public async Task<StaffDto> GetByIdAsync(StaffId id)
        {
            var staff = await this._repo.GetByIdAsync(id);

            if (staff == null)
                return null;

            return new StaffDto(staff.Name.ToString(), staff.Phone.ToString(), staff.Email.ToString(), staff.Address.ToString(), staff.Id.AsString(), staff.SpecializationId.AsString());
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

            var staff = new Staff(seqNumber, dto.Address, dto.LicenseNumber, dto.FirstName, dto.LastName, dto.Email,
            dto.Phone, function, spec.Id);

            await this._repo.AddAsync(staff);

            await this._unitOfWork.CommitAsync();

            return new StaffDto(staff.Name.ToString(), staff.Phone.ToString(), staff.Email.ToString(), staff.Address.ToString(), staff.Id.AsString(), staff.SpecializationId.AsString());
        }

        public async void AddUser(User user, string email, string phone)
        {
            bool result = await _repo.ExistsStaffWithEmailOrPhone(email, phone.Split(' ')[0], phone.Split(' ')[1]);

            if (!result)
            {
                throw new InvalidOperationException("There isn't a staff member associated with this email/phone.");
            }

            Staff staff = await _repo.GetStaffWithEmail(email);

            staff.AddUser(user);

            await this._unitOfWork.CommitAsync();
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

        public async Task<List<StaffDto>> GetAllAsync()
        {
            var list = await this._repo.GetAllAsync();

            List<StaffDto> listDto = list.ConvertAll<StaffDto>(staff => new StaffDto(staff.Name.ToString(),
                staff.Phone.ToString(), staff.Email.ToString(), staff.Address.ToString(), staff.Id.AsString(), staff.SpecializationId.AsString()));

            return listDto;
        }

        public async Task<StaffDto> UpdateAsync(EditStaffDto dto)
        {
            var staff = await this._repo.GetByIdAsync(new StaffId(dto.StaffId));

            if (staff == null)
                return null;

            if (dto.Phone != null)
                staff.ChangePhone(dto.Phone);

            if (dto.Email != null)
                staff.ChangeEmail(dto.Email);

            if (dto.Address != null)
                staff.ChangeAddress(dto.Address);

            if (dto.SpecializationId != null)
                staff.ChangeSpecialization(dto.SpecializationId);

            if (dto.Slots != null)
                staff.ChangeSlots(dto.Slots);

            await this._unitOfWork.CommitAsync();

            if (dto.Slots != null)
            {
                List<SlotsDto> slotsDto = new List<SlotsDto>();

                foreach (var slot in dto.Slots)
                {
                    slotsDto.Add(new SlotsDto(slot.StartDate, slot.EndDate, slot.StartTime, slot.EndTime));
                }

                return new StaffDto(staff.Name.ToString(), staff.Phone.ToString(), staff.Email.ToString(),
                staff.Address.ToString(), staff.SpecializationId.AsString(), staff.Id.AsString(), slotsDto);

            }
            else
            {
                return new StaffDto(staff.Name.ToString(), staff.Phone.ToString(), staff.Email.ToString(),
                staff.Address.ToString(), staff.Id.AsString(), staff.SpecializationId.AsString());
            }

        }

        public async Task<List<StaffDto>> FilterStaffProfiles(StaffQueryParametersDto dto)
        {
            List<Staff> filteredStaffs = await _repo.FilterStaffProfiles(dto);
            List<StaffDto> StaffDtoListFiltered = [];

            foreach (Staff Staff in filteredStaffs)
            {
                //string name, string phone, string email, string address, string specializationDenomination
                StaffDtoListFiltered.Add(new StaffDto(Staff.Name.ToString(), Staff.Phone.ToString(), Staff.Email.ToString(), Staff.Address.ToString(), Staff.Id.AsString(), Staff.SpecializationId.AsString()));
            }

            return StaffDtoListFiltered;
        }

        public async Task<bool> DeactivateStaffProfile(string id)
        {
            var staff = await _repo.GetByIdAsync(new StaffId(id));
            if (staff == null)
            {
                throw new BusinessRuleValidationException("No staff exists with that Id.");
            }

            staff.DeactivateProfile();

            await _logService.CreateEditLog(id, staff.GetType(), "Deactivation of staff's profile.");

            await this._unitOfWork.CommitAsync();

            return true;
        }

        public async Task<string> GetProfileEmail(string email, string phone)
        {
            Staff staff = await _repo.FindStaffWithEmailOrPhone(email, phone.Split(' ')[0], phone.Split(' ')[1]);

            return staff.Email.EmailAddress;
        }
    }
}
