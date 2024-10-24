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
using System.ComponentModel;
using DDDNetCore.Domain.Tokens;
using DDDNetCore.Domain.Shared;

namespace DDDNetCore.Controllers
{

    [ApiController]
    [Route("api/")]
    public class UserController : ControllerBase
    {
        private static string hospitalEmail = "noreply.healthcare.dg38@gmail.com";
        private readonly UserManager<User> _userManager;
        private readonly RoleManager<Role> _roleManager;
        private readonly PatientService _patientService;
        private readonly StaffService _staffService;
        private readonly EmailService _emailService;
        private readonly IConfiguration _configuration;

        private readonly UserService _userService;
        public UserController(UserService userService, UserManager<User> userManager, RoleManager<Role> roleManager, PatientService patientService, StaffService staffService, EmailService emailService, IConfiguration configuration)
        {
            _userService = userService;
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
            try
            {
                var token = await _userService.Login(loginUserDto);
                return Ok(new { Token = token });
            }
            catch (BusinessRuleValidationException ex)
            {
                return Unauthorized(new { ex.Message });
            }
            catch (Exception)
            {
                return BadRequest(new { V = "An unexpected error occured." });
            }
        }

        [HttpPut("Activate-StaffAccount")]
        public async Task<IActionResult> ConfirmEmailStaff([FromQuery] string userId, [FromQuery] string token, [FromBody] ConfirmEmailUserDto confirmEmailUserDto)
        {
            if (string.IsNullOrWhiteSpace(userId) || string.IsNullOrWhiteSpace(token))
            {
                return BadRequest("User Email and Token are required.");
            }

            if (!await _userService.UserExistsById(userId))
            {
                return NotFound("User not found.");
            }

            if (await _userService.ConfirmEmailStaff(userId, token, confirmEmailUserDto.NewPassword))
            {
                return Ok("Email confirmed successfully and account activated.");
            }
            else
            {
                return BadRequest("Email confirmation failed.");
            }
        }

        [HttpPut("Activate-PatientAccount")]
        public async Task<IActionResult> ConfirmEmailPatient([FromQuery] string userId, [FromQuery] string token)
        {
            if (string.IsNullOrWhiteSpace(userId) || string.IsNullOrWhiteSpace(token))
            {
                return BadRequest("User Email and Token are required.");
            }

            if (!await _userService.UserExistsById(userId))
            {
                return NotFound("User not found.");
            }

            if (await _userService.ConfirmEmailPatient(userId, token))
            {
                return Ok("Email confirmed successfully and account activated.");
            }
            else
            {
                return BadRequest("Email confirmation failed.");
            }
        }

        [HttpGet("Get-UserInfo")] // END POINT TO VERIFY IF REGISTER AND LOGIN ARE WORKING CORRECTLY
        [Authorize(Policy = "AuthenticatedUser")]
        public async Task<IActionResult> GetUserInfo()
        {
            string? userEmail = User.FindFirstValue(ClaimTypes.Email);

            if (string.IsNullOrEmpty(userEmail))
            {
                return NotFound("Email claim not found.");
            }
            User? user = await _userService.FindByEmailAsync(userEmail);

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

        [HttpPost("Create-UserAdmin")]
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
            IdentityResult result = await _userService.CreateAsync(user, registerUserDto.Password);

            if (!result.Succeeded)
            {
                return BadRequest(result.Errors);
            }

            // Assign the selected role to the user
            _userService.AddToRoleAsync(user, registerUserDto.Role);

            return Ok(new { Message = "User registered successfully." });
        }

        [HttpPost("Create-UserPatient")]
        public async Task<IActionResult> RegisterPatientAndAssociateUser([FromBody] RegisterPatientUserDto registerPatientUserDto)
        {
            try
            {
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

                SendConfirmationEmail(user, "Patient"); 
            }
            catch (Exception ex)
            {
                return StatusCode(500, $"An error occurred: {ex.Message} - {ex.StackTrace}");
            }

            return Ok(new { Message = "User registered successfully." });
        }

        [HttpPost("Create-UserStaff")]
        [Authorize(Policy = "Admin")]
        public async Task<IActionResult> RegisterStaffAndAssociateUser([FromBody] RegisterUserDto registerUserDto)
        {
            try
            {
                // Check if the role exists
                bool roleExists = await _roleManager.RoleExistsAsync(registerUserDto.Role);
                if (!roleExists)
                {
                    return BadRequest($"Role '{registerUserDto.Role}' does not exist.");
                }

                // Create the user
                var user = new User { UserName = registerUserDto.Email, Email = registerUserDto.Email, Status = false };
                IdentityResult result = await _userService.CreateAsync(user, registerUserDto.Password);

                if (!result.Succeeded)
                {
                    return BadRequest(result.Errors);
                }

                // Assign the selected role to the user
                _userService.AddToRoleAsync(user, registerUserDto.Role);

                _staffService.AddUser(user, registerUserDto.Email, registerUserDto.Phone);
            
                SendConfirmationEmail(user, registerUserDto.Role);
            }
            catch (InvalidOperationException ex1)
            {
                return BadRequest(ex1.Message); 
            }
            catch (Exception ex)
            {
                return BadRequest("An error occurred.");
            }

            return Ok(new { Message = "User registered successfully." });
        }

        

        private async void SendConfirmationEmail(User user, string role)
        {
            string token = await _userManager.GenerateEmailConfirmationTokenAsync(user);

            var confirmationLink = "";

            if(role.Equals("Patient"))
            {
                confirmationLink = Url.Action("Activate-PatientAccount", "User", new { userId = user.Id, token = token }, Request.Scheme);
            } 
            else
            {
                confirmationLink = Url.Action("Activate-StaffAccount", "User", new { userId = user.Id, token = token }, Request.Scheme);
            }
            
            EmailMessageDto emailDto = new EmailMessageDto(
                hospitalEmail,
                user.Email,
                "Account Activation",
                "<p>Hello,</p><p>This email was sent to inform you that your user account in the HealthCare Clinic System has been created. </p><p><a href='" + confirmationLink + "'>Click in the link to confirm the activation of the account.</a></p><p>Thank you for choosing us,<br>HealthCare Clinic</p></body></html>"
            );
            
            await _emailService.SendConfirmationEmail(emailDto);
        }

    }

}
