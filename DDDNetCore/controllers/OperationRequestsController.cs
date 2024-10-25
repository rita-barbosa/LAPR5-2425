using System;
using Microsoft.AspNetCore.Mvc;
using System.Collections.Generic;
using System.Threading.Tasks;
using DDDNetCore.Domain.OperationRequest;
using DDDNetCore.Domain.Shared;
using Microsoft.AspNetCore.Authorization.Policy;
using Microsoft.AspNetCore.Authorization;
using System.Security.Claims;

namespace DDDNetCore.Controllers
{
    [Route("api/[controller]")]
    [ApiController]
    public class OperationRequestController : ControllerBase
    {
        private readonly OperationRequestService _service;

        public OperationRequestController(OperationRequestService service)
        {
            _service = service;
        }

        // GET: api/OperationRequest
        [HttpGet]
        public async Task<ActionResult<IEnumerable<OperationRequestDto>>> GetAll()
        {
            return await _service.GetAllAsysnc();
        }

        // GET: api/OperationRequest/5
        [HttpGet("{id}")]
        public async Task<ActionResult<OperationRequestDto>> GetGetById(string id)
        {
            var opReq = await _service.GetByIdAsync(new OperationRequestId(id));

            if (opReq == null)
            {
                return NotFound();
            }

            return opReq;
        }

        // POST: api/OperationRequest
        [HttpPost]
        public async Task<ActionResult<OperationRequestDto>> Create(CreatingOperationRequestDto dto)
        {
            try
            {
                var opReq = await _service.AddAsync(dto);

                return CreatedAtAction(nameof(GetGetById), new { id = opReq.Id }, opReq);
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


        // PUT: api/OperationRequest/F5
        [HttpPut("{id}")]
        public async Task<ActionResult<UpdateOperationRequestDto>> Update(string id, UpdateOperationRequestDto dto)
        {
            if (id != dto.Id)
            {
                return BadRequest();
            }

            try
            {
                var opr = await _service.UpdateAsync(dto);

                if (opr == null)
                {
                    return NotFound();
                }
                return Ok(opr);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }

        [HttpGet("filtered")]
        [Authorize(Policy = "Doctor")]
        public async Task<ActionResult<IEnumerable<OperationRequestDto>>> GetOperationRequestByFilters(
                                                                                [FromQuery] string? name = null,
                                                                                [FromQuery] string? priority = null,
                                                                                [FromQuery] string? operationType = null,
                                                                                [FromQuery] string? status = null,
                                                                                [FromQuery] string? dateofrequest = null,
                                                                                [FromQuery] string? deadlinedate = null)
        {
            try
            {
                var email = User.FindFirstValue(ClaimTypes.Email);
                if (email == null)
                {
                    return NotFound("Couldn't obtain the logged in user's email");
                }
                var operationRequests = await _service.GetOperationRequestByFiltersAsync(email, name, priority, operationType, status, dateofrequest, deadlinedate);

                if (operationRequests == null)
                {
                    return NotFound("No matching operation requests found.");
                }

                return Ok(operationRequests);
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

        // POST: api/OperationRequest/Delete-OperationRequest
        [HttpDelete]
        [Route("Delete-OperationRequest")]
        [Authorize(Policy = "Doctor")]
        public async Task<ActionResult> DeleteOperationRequest([FromBody] IdPassDto idPassDto) 
        {
            try
            {
                bool result = await _service.DeleteOperationRequest(idPassDto.Id, User.FindFirstValue(ClaimTypes.Email)); 
                if (result)
                {
                    return Ok("Operation request successfully removed.");
                }
                else
                {
                    return NotFound($"Operation request with ID {idPassDto.Id} not found or wrong authorization."); 
                }
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { ex.Message }); 
            }
            catch (Exception ex)
            {
                return StatusCode(500, new { ex.Message }); 
            }
        }

    }
}