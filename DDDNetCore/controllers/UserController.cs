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
using DDDNetCore.Domain.Patients;
using DDDNetCore.Domain.StaffProfiles;
using DDDNetCore.Domain.Emails;
using Microsoft.AspNetCore.Identity.Data;

namespace DDDNetCore.Controllers
{

    [ApiController]
    [Route("api/")]
    public class UserController : ControllerBase
    {
        private readonly UserManager<User> _userManager;
        private readonly RoleManager<Role> _roleManager;
        private readonly PatientService _patientService;
        private readonly StaffService _staffService;
        private readonly EmailService _emailService;
        private readonly IConfiguration _configuration;

        public UserController(UserManager<User> userManager, RoleManager<Role> roleManager, PatientService patientService, StaffService staffService, EmailService emailService, IConfiguration configuration)
        {
            _userManager = userManager;
            _roleManager = roleManager;
            _patientService = patientService;
            _staffService = staffService;
            _emailService = emailService;
            _configuration = configuration;
        }

        [HttpPost("Login")]
        public async Task<IActionResult> Login([FromBody] LoginUserDto loginUserDto)
        {
            // Find the user by email
            var user = await _userManager.FindByEmailAsync(loginUserDto.Email);
            if (user == null)
            {
                return Unauthorized("Invalid email or password.");
            }
            else if (!user.Status) 
            {
                return Unauthorized("Account not yet activated.");
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

         [HttpPut("ConfirmEmail")]
        public async Task<IActionResult> ConfirmEmail([FromQuery] string userId, [FromQuery] string token, [FromBody] ConfirmEmailUserDto confirmEmailUserDto)
        {
            if (string.IsNullOrWhiteSpace(userId) || string.IsNullOrWhiteSpace(token))
            {
                return BadRequest("User Email and Token are required.");
            }

            var user = await _userManager.FindByIdAsync(userId);
            if (user == null)
            {
                return NotFound("User not found.");
            }

            var result = await _userManager.ConfirmEmailAsync(user, token);
            if (result.Succeeded)
            {

                var resetToken = await _userManager.GeneratePasswordResetTokenAsync(user);
                await _userManager.ResetPasswordAsync(user, resetToken, confirmEmailUserDto.NewPassword);
                user.Status = true;
                await _userManager.UpdateAsync(user);

                return Ok("Email confirmed successfully and password succefully changed.");
            }

            return BadRequest("Email confirmation failed.");
        }

        [HttpGet("Me")] // END POINT TO VERIFY IF REGISTER AND LOGIN ARE WORKING CORRECTLY
        [Authorize(Policy = "AuthenticatedUser")]
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

        [HttpPost("Register-Admins")]
        public async Task<IActionResult> RegisterUser([FromBody] RegisterUserDto registerUserDto)
        {
            // Check if the role exists
            bool roleExists = await _roleManager.RoleExistsAsync(registerUserDto.Role);
            if (!roleExists)
            {
                return BadRequest($"Role '{registerUserDto.Role}' does not exist.");
            }

            // Create the user
            var user = new User { UserName = registerUserDto.Email, Email = registerUserDto.Email, Status = true };
            IdentityResult result = await _userManager.CreateAsync(user, registerUserDto.Password);

            if (!result.Succeeded)
            {
                return BadRequest(result.Errors);
            }

            // Assign the selected role to the user
            await _userManager.AddToRoleAsync(user, registerUserDto.Role);

            return Ok(new { Message = "User registered successfully." });
        }

        [HttpPost("Register-Patient")]
        public async Task<IActionResult> RegisterPatientAndAssociateUser([FromBody] RegisterPatientUserDto registerPatientUserDto)
        {
            try{
                // Create the user
                var user = new User { UserName = registerPatientUserDto.Email, Email = registerPatientUserDto.Email, Status = false };
                IdentityResult result = await _userManager.CreateAsync(user, registerPatientUserDto.Password);

                if (!result.Succeeded)
                {
                    return BadRequest(result.Errors);
                }

                // Assign the selected role to the user
                await _userManager.AddToRoleAsync(user, "Patient");

                // Associate with it's profile
                 _patientService.AddUser(user, registerPatientUserDto.Email, registerPatientUserDto.Phone);

                SendConfirmationEmail(user);
            } 
            catch (Exception ex)
            {
                return StatusCode(500, $"An error occurred: {ex.Message} - {ex.StackTrace}");
            }

            return Ok(new { Message = "User registered successfully." });
        }

        [HttpPost("Register-Staff")]
        [Authorize(Policy = "Admin")]
        public async Task<IActionResult> RegisterStaffAndAssociateUser([FromBody] RegisterUserDto registerUserDto)
        {
            try{
                // Check if the role exists
                bool roleExists = await _roleManager.RoleExistsAsync(registerUserDto.Role);
                if (!roleExists)
                {
                    return BadRequest($"Role '{registerUserDto.Role}' does not exist.");
                }

                // Create the user
                var user = new User { UserName = registerUserDto.Email, Email = registerUserDto.Email, Status = false };
                IdentityResult result = await _userManager.CreateAsync(user, registerUserDto.Password);

                if (!result.Succeeded)
                {
                    return BadRequest(result.Errors);
                }

                // Assign the selected role to the user
                await _userManager.AddToRoleAsync(user, registerUserDto.Role);

                // Associate with it's profile
                if (registerUserDto.Role.Equals("Patient")){
                    _patientService.AddUser(user, registerUserDto.Email, registerUserDto.Phone);
                } else {
                    _staffService.AddUser(user, registerUserDto.Email, registerUserDto.Phone);
                }

                SendConfirmationEmail(user);
            } 
            catch (Exception ex)
            {
                return StatusCode(500, $"An error occurred: {ex.Message} - {ex.StackTrace}");
            }

            return Ok(new { Message = "User registered successfully." });
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

        private async void SendConfirmationEmail(User user)
        {
            // Send confirmation email
            // Step 1: Create a confirmation token to send to the user
            string token = await _userManager.GenerateEmailConfirmationTokenAsync(user);

            var confirmationLink = Url.Action("ConfirmEmail", "User", new { userId = user.Id, token = token }, Request.Scheme);

            // Step 2: Create emailDto to send all the info to the user
            EmailMessageDto emailDto = new EmailMessageDto(
                "noreply.healthcare.dg38@gmail.com", 
                user.Email, 
                "Account Activation", 
                "<p>Hello,</p><p>This email was sent to inform you that your user account in the HealthCare Clinic System has been created. </p><p><a href='" + confirmationLink + "'>Click in the link to confirm the activation of the account and to set your own password.</a></p><p>Thank you for choosing us,<br>HealthCare Clinic</p></body></html>"
            );
            // Step 3: Send an email to the user
            await _emailService.SendConfirmationEmail(emailDto);
        }
    }


}
