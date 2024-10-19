using Microsoft.AspNetCore.Authentication.JwtBearer;
using Microsoft.IdentityModel.Tokens;
using System.IdentityModel.Tokens.Jwt;
using System.Security.Claims;
using System.Text;

using DDDNetCore.Domain.Users;
using Microsoft.AspNetCore.Authorization;
using Microsoft.AspNetCore.Identity;
using Microsoft.AspNetCore.Mvc;
using System.Threading.Tasks;
using Microsoft.Extensions.Configuration;
using System;
using System.Collections.Generic;

namespace DDDNetCore.Controllers
{

    [ApiController]
    [Route("api/")]
    public class UserController : ControllerBase
    {
        private readonly UserManager<User> _userManager;
        private readonly RoleManager<Role> _roleManager;
        private readonly IConfiguration _configuration;

        public UserController(UserManager<User> userManager, RoleManager<Role> roleManager, IConfiguration configuration)
        {
            _userManager = userManager;
            _roleManager = roleManager;
            _configuration = configuration;
        }

        [HttpPost("register")]
        public async Task<IActionResult> RegisterUser([FromBody] RegisterUserDto registerUserDto)
        {
            // Check if the role exists
            bool roleExists = await _roleManager.RoleExistsAsync(registerUserDto.Role);
            if (!roleExists)
            {
                return BadRequest($"Role '{registerUserDto.Role}' does not exist.");
            }

            // Create the user
            var user = new User { UserName = registerUserDto.Email, Email = registerUserDto.Email };
            IdentityResult result = await _userManager.CreateAsync(user, registerUserDto.Password);

            if (!result.Succeeded)
            {
                return BadRequest(result.Errors);
            }

            // Assign the selected role to the user
            await _userManager.AddToRoleAsync(user, registerUserDto.Role);

            return Ok(new { Message = "User registered successfully." });
        }

        [HttpPost("login")]
        public async Task<IActionResult> Login([FromBody] LoginUserDto loginUserDto)
        {
            // Find the user by email
            var user = await _userManager.FindByEmailAsync(loginUserDto.Email);
            if (user == null)
            {
                return Unauthorized("Invalid email or password.");
            }

            // Check the password
            var result = await _userManager.CheckPasswordAsync(user, loginUserDto.Password);
            if (!result)
            {
                return Unauthorized("Invalid email or password.");
            }

            // Generate the token
            var token = GenerateJwtToken(user);

            return Ok(new { Token = token.Result });
        }

        private async Task<string> GenerateJwtToken(User user)
        {
            var roles = await _userManager.GetRolesAsync(user);
            var claims = new List<Claim>
            {
                new(ClaimTypes.NameIdentifier, user.Id.ToString()),
                new(ClaimTypes.Email, user.Email)
            };

            foreach (var role in roles)
            {
                claims.Add(new Claim(ClaimTypes.Role, role));
            }

            var key = new SymmetricSecurityKey(Encoding.UTF8.GetBytes(_configuration["Jwt:Key"]));
            var creds = new SigningCredentials(key, SecurityAlgorithms.HmacSha256);

            var token = new JwtSecurityToken(
                issuer: _configuration["Jwt:Issuer"],
                audience: _configuration["Jwt:Audience"],
                claims: claims,
                expires: DateTime.Now.AddMinutes(30), // THE TIME THE TOKEN IS ATIVE
                signingCredentials: creds);

            return new JwtSecurityTokenHandler().WriteToken(token);
        }

        [HttpGet("me")]
        [Authorize(Policy = "Admin")] // END POINT TO VERIFY IF REGISTER AND LOGIN ARE WORKING CORRECTLY
        public async Task<IActionResult> GetUserInfo()
        {
            string? userEmail = User.FindFirstValue(ClaimTypes.Email);

            if (string.IsNullOrEmpty(userEmail))
            {
                return NotFound("Email claim not found.");
            }
            User? user = await _userManager.FindByEmailAsync(userEmail);

            if (user != null)
            {
                return Ok(new
                {
                    user.Id,
                    user.Email,
                });
            }
            else
            {
                return NotFound("User not found.");
            }
        }
    }
}
