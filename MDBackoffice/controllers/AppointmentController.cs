using System;
using Microsoft.AspNetCore.Mvc;
using System.Collections.Generic;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Authorization;
using MDBackoffice.Domain.Appointments;
using MDBackoffice.Domain.Shared;

namespace MDBackoffice.Controllers
{
    [Route("api/[controller]")]
    [ApiController]
    public class AppointmentController : ControllerBase{
        private readonly AppointmentService _service;

        public AppointmentController(AppointmentService service){
            _service = service;
        }

        // POST: api/Appointment
        [HttpPost]
        public async Task<ActionResult<AppointmentDto>> Create(CreatingAppointmentDto dto){
            try
            {
            var appointment = await _service.AddAsync(dto);
            return Ok(appointment);
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

    }

}