using Microsoft.AspNetCore.Mvc;
using System;
using System.Threading.Tasks;
using MDBackoffice.Domain.Specializations;
using System.Collections.Generic;

namespace MDBackoffice.Controllers
{
    [Route("api/[controller]")]
    [ApiController]
    public class SpecializationsController : ControllerBase
    {
        private readonly SpecializationService _service;

        public SpecializationsController(SpecializationService service)
        {
            _service = service;
        }

        // GET: api/Specializations/5
        [HttpGet("{id}")]
        public async Task<ActionResult<SpecializationDto>> GetGetById(string id)
        {
            var Specialization = await _service.GetByIdAsync(new SpecializationDenomination(id));

            if (Specialization == null)
            {
                return NotFound();
            }

            return Specialization;
        }

        [HttpGet()]
        public async Task<ActionResult<List<SpecializationDto>>> GetAllAvailable()
        {
            try
            {
                var Specialization = await _service.GetAllAsync();
                return Ok(Specialization);
            }
            catch (Exception ex)
            {
                return BadRequest(new { V = $"An unexpected error occurred: {ex.Message}" });
            }
        }


        // POST: api/Specializations
        [HttpPost]
        public async Task<ActionResult<SpecializationDto>> Create(SpecializationDto dto)
        {
            var Specialization = await _service.AddAsync(dto);

            return CreatedAtAction(nameof(GetGetById), new { id = new SpecializationDenomination(Specialization.Denomination) }, Specialization);
        }


    }
}