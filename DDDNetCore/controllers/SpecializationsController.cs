using Microsoft.AspNetCore.Mvc;
using System;
using System.Threading.Tasks;
using DDDNetCore.Domain.Specializations;

namespace DDDNetCore.Controllers
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

        // POST: api/Specializations
        [HttpPost]
        public async Task<ActionResult<SpecializationDto>> Create(SpecializationDto dto)
        {
            var Specialization = await _service.AddAsync(dto);

            return CreatedAtAction(nameof(GetGetById), new { id = new SpecializationDenomination(Specialization.Denomination) }, Specialization);
        }


    }
}