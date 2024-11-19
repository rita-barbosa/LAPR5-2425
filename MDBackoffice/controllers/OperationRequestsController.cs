using System;
using Microsoft.AspNetCore.Mvc;
using System.Collections.Generic;
using System.Threading.Tasks;
using MDBackoffice.Domain.OperationRequests;
using MDBackoffice.Domain.Shared;
using Microsoft.AspNetCore.Authorization;
using System.Security.Claims;

namespace MDBackoffice.Controllers
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
        [Route("Get-AllOpRequests")]
        public async Task<ActionResult<IEnumerable<OperationRequestDto>>> GetAll()
        {
            return await _service.GetAllAsysnc();
        }

        // GET: api/OperationRequest
        [HttpGet]
        [Route("Get-AllOpRequestsFromDoctor")]
        public async Task<ActionResult<IEnumerable<OperationRequestDto>>> GetAllFromDoctor()
        {
            string? userEmail = User.FindFirstValue(ClaimTypes.Email);
            return await _service.GetAllFromDoctorAsysnc(userEmail);
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
        [Authorize(Policy = "Doctor")]
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


        // PUT: api/OperationRequest/Update
        [HttpPut("Update")]
        [Authorize(Policy = "Doctor")]
        public async Task<ActionResult<OperationRequestDto>> Update(UpdateOperationRequestDto dto)
        {
            string? userEmail = User.FindFirstValue(ClaimTypes.Email);

            // var userEmail = "ritabfbarbosa@gmail.com";

            if (!await _service.CheckDoctorIsRequestingDoctor(userEmail, dto.Id)){
                return BadRequest("You are not the requesting doctor for the choosen operation request.");
            }

            try
            {
                var opr = await _service.UpdateAsync(dto);

                if (opr == null)
                {
                    return NotFound("An operation request with the provided ID does not exist.");
                }
                return Ok(opr);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { ex.Message });
            }
        }

        [HttpGet("filtered")]
        // [Authorize(Policy = "Doctor")]
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

                // var email = "ritabfbarbosa@gmail.com";
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

        // POST: api/OperationRequest/Delete-OperationRequest
        [HttpDelete]
        [Route("Delete-OperationRequestFromPatient")]
        [Authorize(Policy = "Doctor")]
        public async Task<ActionResult> DeleteOperationRequestFromPatient([FromBody] AddOrRemoveFromPatientDto removingFromPatientDto) 
        {
            try
            {
                bool result = await _service.DeleteOperationRequestFromPatient(removingFromPatientDto.PatientId, removingFromPatientDto.OperationRequestId, User.FindFirstValue(ClaimTypes.Email)); 
                if (result)
                {
                    return Ok("Operation request successfully removed from patient profile.");
                }
                else
                {
                    return NotFound($"Operation request with ID {removingFromPatientDto.PatientId} not found or wrong authorization."); 
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

        // POST: api/OperationRequest/Add-OperationRequestToPatient
        [HttpPost]
        [Route("Add-OperationRequestToPatient")]
        [Authorize(Policy = "Doctor")]
        public async Task<ActionResult> AddOperationRequestToPatient([FromBody] AddOrRemoveFromPatientDto addOrRemoveFromPatientDto) 
        {
            try
            {
                bool result = await _service.AddOperationRequestToPatient(addOrRemoveFromPatientDto.PatientId, addOrRemoveFromPatientDto.OperationRequestId, User.FindFirstValue(ClaimTypes.Email)); 
                if (result)
                {
                    return Ok("Operation request successfully add.");
                }
                else
                {
                    return NotFound($"Operation request with ID {addOrRemoveFromPatientDto.PatientId} not found or wrong authorization."); 
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