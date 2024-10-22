using System;
using Microsoft.AspNetCore.Mvc;
using System.Collections.Generic;
using System.Threading.Tasks;
using DDDNetCore.Domain.OperationRequest;
using DDDNetCore.Domain.Shared;

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

            return CreatedAtAction(nameof(GetGetById), new {id = opReq.Id}, opReq);
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
            catch(BusinessRuleValidationException ex)
            {
                return BadRequest(new {Message = ex.Message});
            }
        }

    }
}