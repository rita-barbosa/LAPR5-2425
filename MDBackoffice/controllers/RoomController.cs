using System;
using Microsoft.AspNetCore.Mvc;
using System.Collections.Generic;
using System.Threading.Tasks;
using MDBackoffice.Domain.Rooms;
using MDBackoffice.Domain.Shared;

namespace MDBackoffice.Controllers
{
    [Route("api/[controller]")]
    [ApiController]
    public class RoomController : ControllerBase{
        private readonly RoomService _service;

        public RoomController(RoomService service){
            _service = service;
        }

        // POST: api/Room
        [HttpPost]
        public async Task<ActionResult<RoomDto>> Create(RoomDto dto){
            try
            {
            var room = await _service.AddAsync(dto);
            return Ok(room);
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

        [HttpGet]
        [Route("Get-All")]
        public async Task<List<RoomDto>> GetAll(){
            return await _service.GetAllAsync();
        }

    }

}