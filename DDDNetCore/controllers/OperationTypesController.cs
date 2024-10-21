using Microsoft.AspNetCore.Mvc;
using System;
using System.Threading.Tasks;
using DDDNetCore.Domain.OperationTypes;

namespace DDDNetCore.Controllers
{
    [Route("api/[controller]")]
    [ApiController]
    public class OperationTypesController : ControllerBase
    {
        private readonly OperationTypeService _service;

        public OperationTypesController(OperationTypeService service)
        {
            _service = service;
        }

        // GET: api/operationTypes/5
        [HttpGet("{id}")]
        public async Task<ActionResult<OperationTypeDto>> GetGetById(string id)
        {

            Console.WriteLine(id.ToString());

            var operationType = await _service.GetByIdAsync(new OperationTypeId(id));


            if (operationType == null)
            {
                return NotFound();
            }

            return operationType;
        }

        // POST: api/operationTypes
        [HttpPost]
        public async Task<ActionResult<OperationTypeDto>> Create(OperationTypeDto dto)
        {
            var operationType = await _service.AddAsync(dto);

            return CreatedAtAction(nameof(GetGetById), new { id = new OperationTypeId(operationType.Name) }, operationType);
        }


    }
}