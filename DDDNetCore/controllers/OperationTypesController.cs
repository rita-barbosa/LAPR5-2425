using Microsoft.AspNetCore.Mvc;
using System;
using System.Threading.Tasks;
using DDDNetCore.Domain.Shared;
using Microsoft.AspNetCore.Authorization;
using System.Collections.Generic;
using Microsoft.IdentityModel.Tokens;
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
        [Authorize(Policy = "Admin")]
        public async Task<ActionResult<OperationTypeDto>> Create(OperationTypeDto dto)
        {
            var operationType = await _service.AddAsync(dto);
            return CreatedAtAction(nameof(GetGetById), new { id = new OperationTypeId(operationType.Name) }, operationType);
        }

        //POST: api/operationTypes/Filtered-List
        [HttpPost("Filtered-List")]
        [Authorize(Policy = "Admin")]
        public async Task<ActionResult<List<OperationTypeDto>>> GetFilteredOperationTypes(OperationTypeQueryParametersDto dto)
        {
            var operationType = await _service.FilterOperationTypes(dto);

            if(operationType.IsNullOrEmpty()){
                return NotFound(new { Message = "No operation types matching the filtering criteria."});
            }

            return Ok(operationType);
        }

        [HttpPut("Edit-OperationType")]
        [Authorize(Policy = "Admin")]
        public async Task<IActionResult> EditOperationType([FromBody] EditOpTypeDto editOpTypeDto)
        {
            try
            {
                await _service.EditOperationType(editOpTypeDto);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { message = ex.Message });
            }
            catch (Exception ex)
            {
                return BadRequest(new { error = $"An unexpected error occurred: {ex.Message}" });
            }
            return Ok(new { message = "Operation successfully edited." });
        }


        [HttpDelete("{id}")]
        [Authorize(Policy = "Admin")]
        public async Task<ActionResult<OperationTypeDto>> RemoveOperationType(string id)
        {
            try
            {
                var prod = await _service.InactivateAsync(id);
                return Ok(prod);
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