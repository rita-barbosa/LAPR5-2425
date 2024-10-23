using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using DDDNetCore.Domain.Patients;
using DDDNetCore.Domain.Shared;
using DDDNetCore.Domain.StaffProfiles;
using Microsoft.AspNetCore.Authorization;
using Microsoft.AspNetCore.Mvc;

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
        // POST: api/Patient
        [HttpPost]
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


        //GET: api/Patient
        [HttpGet]
        public async Task<ActionResult<IEnumerable<PatientDto>>> GetPatientProfiles()
        {
            return await _service.GetAllAsysnc();
        }


        //PUT: api/Patient/5
        [HttpPut("{id}")]
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

                //falta a parte de mandar o mail!!!!

                return Ok(patient);
            }
            catch(BusinessRuleValidationException ex)
            {
                return BadRequest(new {Message = ex.Message});
            }
        }
    }



}