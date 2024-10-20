using System;
using System.Threading.Tasks;
using DDDNetCore.Domain.Patients;
using DDDNetCore.Domain.Shared;
using DDDNetCore.Domain.StaffProfiles;
using Microsoft.AspNetCore.Mvc;
using MimeKit.Encodings;

namespace DDDNetCore.Controllers
{
    [Route("api/[controller]")]
    [ApiController]
    public class PatientController : ControllerBase
    {
        private readonly PatientService _service;

        public PatientController(PatientService service)
        {
            _service = service;
        }

        // POST: api/Staff
        [HttpPost]
        public async Task<ActionResult<PatientDto>> CreateStaffProfile(CreatingPatientDto dto)
        {
            try
            {
                var patient = await _service.CreatePatientProfile(dto);

                return Ok(patient);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { ex.Message });
            }
            catch (Exception)
            {
                return BadRequest(new { V = "An unexpected error occured." });
            }
        }
    }
}