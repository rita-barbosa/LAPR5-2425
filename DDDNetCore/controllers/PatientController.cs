using System;
using System.Collections.Generic;
using System.Security.Claims;
using System.Threading.Tasks;
using DDDNetCore.Domain.Patients;
using DDDNetCore.Domain.Shared;
using DDDNetCore.Domain.StaffProfiles;
using DDDNetCore.Domain.Users;
using Microsoft.AspNetCore.Authorization;
using Microsoft.AspNetCore.Mvc;
using Microsoft.IdentityModel.Tokens;

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

        [HttpGet("{id}")]
        [Route("Get-PatientWithId")]
        [Authorize(Policy = "Admin")]
        public async Task<ActionResult<PatientDto>> GetPatientById(MedicalRecordNumber id)
        {
            var staff = await _service.GetByIdAsync(id);
            if (staff == null)
            {
                return NotFound();
            }
            return Ok(staff);
        }
    
        [HttpPost]
        [Route("Create-PatientProfile")]
        [Authorize(Policy = "Admin")]
        public async Task<ActionResult<PatientDto>> CreatePatientProfile(CreatingPatientDto dto)
        {
            try
            {
                var patient = await _service.CreatePatientProfile(dto);

                return CreatedAtAction(nameof(GetPatientById), new { id = patient.PatientId }, patient);
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

    	[HttpDelete("{id}")]
        [Authorize(Policy = "Admin")]
        public async Task<IActionResult> DeletePatientProfile(string id)
        {
            try
            {
                await _service.DeletePatientProfile(id);

                return Ok("Patient profile and account succefully deleted");
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { ex.Message });
            }
            catch (Exception ex1)
            {
                return BadRequest(new { V = ex1.Message });
            }
        }
    

        [HttpPut]
        [Authorize(Policy = "Patient")]
        public async Task<ActionResult<PatientDto>> EditPatientProfile(EditPatientProfileDto dto)
        {
            try
            {
                var email = User.FindFirstValue(ClaimTypes.Email);
                if (email == null)
                {
                    return BadRequest(new { Message = "Email claim not found." });
                }

                var patient = await _service.EditProfile(email, dto);

                return Accepted(patient);
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


        //GET: api/Patient
        [HttpGet]
        public async Task<ActionResult<IEnumerable<PatientDto>>> GetPatientProfiles()
        {
            return await _service.GetAllAsysnc();
        }


        //PUT: api/Patient/5
        [HttpPut("{id}")]
        [Authorize(Policy = "Admin")]
        public async Task<ActionResult<PatientDto>> EditPatientProfile(string id, EditPatientDto dto)
        {
            if (id != dto.PatientId)
            {
                return BadRequest();
            }

            try
            {
                var patient = await _service.UpdateAsync(dto);

                if (patient == null)
                {
                    return NotFound();
                }

                return Ok(patient);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }

        // GET: api/Patients/Filtered-List
        [HttpPost("Filtered-List")]
        [Authorize(Policy = "Admin")]
        public async Task<ActionResult<List<PatientDto>>> GetFilteredPatientProfiles(PatientQueryParametersDto dto)
        {
            var patients = await _service.FilterPatientProfiles(dto);

            if (patients.IsNullOrEmpty())
            {
                return NotFound(new { Message = "No patients matching the filtering criteria." });
            }

            return Ok(patients);
        }
    }

}