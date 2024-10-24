using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using DDDNetCore.Domain.Shared;
using DDDNetCore.Domain.StaffProfiles;
using Microsoft.AspNetCore.Authorization;
using Microsoft.AspNetCore.Mvc;
using Microsoft.IdentityModel.Tokens;

namespace DDDNetCore.Controllers
{
    [Route("api/[controller]")]
    [ApiController]
    // [Authorize(Policy = "Staff")]
    public class StaffController : ControllerBase
    {
        private readonly StaffService _service;

        public StaffController(StaffService service)
        {
            _service = service;
        }

        [HttpGet("{id}")]
        public async Task<ActionResult<StaffDto>> GetStaffById(StaffId id)
        {
            var staff = await _service.GetByIdAsync(id);
            if (staff == null)
            {
                return NotFound();
            }
            return Ok(staff);
        }

        // POST: api/Staff/Create-StaffProfile
        [HttpPost]
        [Route("create")]
        [Authorize(Policy = "Admin")]
        public async Task<ActionResult<StaffDto>> CreateStaffProfile(CreatingStaffDto dto)
        {
            try
            {
                var staff = await _service.CreateStaffProfile(dto);

                return Ok(staff);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { ex.Message });
            }
            catch (Exception ex)
            {
                return BadRequest(new { V = $"An unexpected error occurred: {ex.Message}" });
            }
        }

        // GET: api/Staff
        [HttpGet]
        public async Task<ActionResult<IEnumerable<StaffDto>>> GetStaffProfiles()
        {
            return await _service.GetAllAsync();
        }


        // PUT: api/Staff/5
        [HttpPut("{id}")]
        public async Task<ActionResult<StaffDto>> EditStaffProfile(string id, EditStaffDto dto)
        {
            if (id != dto.StaffId)
            {
                return BadRequest();
            }

            try
            {
                var staff = await _service.UpdateAsync(dto);

                if (staff == null)
                {
                    return NotFound();
                }

                //falta a parte de mandar o mail!!!!


                return Ok(staff);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }

        }

        // POST: api/Staff/Filtered-List
        [HttpPost("Filtered-List")]
        [Authorize(Policy = "Admin")]
        public async Task<ActionResult<List<StaffDto>>> GetFilteredStaffProfiles(StaffQueryParametersDto dto)
        {
            var staff = await _service.FilterStaffProfiles(dto);

            if (staff.IsNullOrEmpty())
            {
                return NotFound(new { Message = "No staff matching the filtering criteria." });
            }

            return Ok(staff);
        }

    }
}