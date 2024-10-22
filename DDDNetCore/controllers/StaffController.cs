using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using DDDNetCore.Domain.Shared;
using DDDNetCore.Domain.StaffProfiles;
using Microsoft.AspNetCore.Authorization;
using Microsoft.AspNetCore.Mvc;

namespace DDDNetCore.Controllers
{
    [Route("api/[controller]")]
    [ApiController]
    [Authorize(Policy = "Staff")]
    public class StaffController : ControllerBase
    {
        private readonly StaffService _service;

        public StaffController(StaffService service)
        {
            _service = service;
        }

        [HttpGet("{id}")]
        [Authorize(Policy = "Admin")]
        public async Task<ActionResult<StaffDto>> GetStaffById(StaffId id)
        {
            var staff = await _service.GetByIdAsync(id);
            if (staff == null)
            {
                return NotFound();
            }
            return Ok(staff);
        }

        // POST: api/Staff
        [HttpPost]
        [Authorize(Policy = "Admin")]
        public async Task<ActionResult<StaffDto>> CreateStaffProfile(CreatingStaffDto dto)
        {
            try
            {
                var staff = await _service.CreateStaffProfile(dto);

                return CreatedAtAction(nameof(GetStaffById), new { id = staff.Id }, staff); 
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { ex.Message });
            }
            catch (Exception ex)
            {
                return BadRequest(new { ex.Message});
            }
        }

    }
}