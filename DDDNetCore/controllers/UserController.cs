using System.Security.Claims;
using DDDNetCore.Domain.Users;
using Microsoft.AspNetCore.Authorization;
using Microsoft.AspNetCore.Identity;
using Microsoft.AspNetCore.Mvc;
using System.Threading.Tasks;
using Microsoft.Extensions.Configuration;
using System;
using DDDNetCore.Domain.Patients;
using DDDNetCore.Domain.StaffProfiles;
using DDDNetCore.Domain.Emails;
using DDDNetCore.Domain.Tokens;
using DDDNetCore.Domain.Shared;
using Microsoft.AspNetCore.Authentication;

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
        private readonly TokenService _tokenService;
        private readonly IConfiguration _configuration;

        private readonly UserService _userService;
        public UserController(UserService userService, UserManager<User> userManager, RoleManager<Role> roleManager, PatientService patientService, StaffService staffService, EmailService emailService, TokenService tokenService, IConfiguration configuration)
        {
            _userService = userService;
            _userManager = userManager;
            _roleManager = roleManager;
            _patientService = patientService;
            _staffService = staffService;
            _emailService = emailService;
            _tokenService = tokenService;
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
                return BadRequest(new { ex.Message });
            }
            catch (Exception ex)
            {
                return BadRequest(new { V = $"An unexpected error occurred: {ex.Message}" });
            }
        }

        [HttpPut("activate-staff")]
        public async Task<IActionResult> ConfirmEmailStaff([FromQuery] string userId, [FromQuery] string token, [FromBody] ConfirmEmailUserDto confirmEmailUserDto)
        {
            try
            {
                await _userService.ConfirmEmailStaff(userId, token, confirmEmailUserDto.NewPassword);
                return Ok("Email confirmed successfully and account activated.");
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(ex.Message);
            }
            catch (Exception ex)
            {
                return BadRequest(new { V = $"An unexpected error occurred: {ex.Message}" });
            }
        }

        [HttpPut("activate-patient")]
        public async Task<IActionResult> ConfirmEmailPatient([FromQuery] string userId, [FromQuery] string token)
        {
            try
            {
                await _userService.ConfirmEmailPatient(userId, token);
                return Ok("Email confirmed successfully and account activated.");
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(ex.Message);
            }
            catch (Exception ex)
            {
                return BadRequest(new { V = $"An unexpected error occurred: {ex.Message}" });
            }
        }

        [HttpPost("create-admin")]
        public async Task<IActionResult> RegisterAdminUser([FromBody] RegisterUserDto registerUserDto)
        {
            try
            {
                await _userService.CreateUserAsync(registerUserDto);
                return Ok("Email confirmed successfully and account activated.");
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(ex.Message);
            }
            catch (Exception ex)
            {
                return BadRequest(new { V = $"An unexpected error occurred: {ex.Message}" });
            }
        }

        [HttpPost("create-patient")]
        public async Task<IActionResult> RegisterPatientUser([FromBody] RegisterPatientUserDto registerPatientUserDto)
        {
            try
            {
                var user = await _userService.CreatePatientUserAsync(registerPatientUserDto);
                _patientService.AddUser(user, registerPatientUserDto.Email, registerPatientUserDto.Phone);
                await _userService.SendConfirmationEmail(user);
                return Ok("The user has been successfully created. Please verify your email to complete the registration.");
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(ex.Message);
            }
            catch (Exception ex)
            {
                return BadRequest(new { V = $"An unexpected error occurred: {ex.Message}" });
            }
        }

        [HttpPost("create-staff")]
        [Authorize(Policy = "Admin")]
        public async Task<IActionResult> RegisterStaffUser([FromBody] RegisterUserDto registerUserDto)
        {
            try
            {
                var user = await _userService.CreateStaffUserAsync(registerUserDto);
                _staffService.AddUser(user, registerUserDto.Email, registerUserDto.Phone);
                await _userService.SendConfirmationEmail(user);
                return Ok("The user has been successfully created. Please verify your email to complete the registration.");
            }
            catch (InvalidOperationException ex1)
            {
                return BadRequest(ex1.Message);
            }
            catch (Exception ex)
            {
                return BadRequest(new { V = $"An unexpected error occurred: {ex.Message}" });
            }
        }


        // Delete Patient Account Request: api/Delete-PatientAccountDeletionRequest
        [HttpDelete("Delete-PatientAccountDeletionRequest")]
        [Authorize(Policy = "Patient")]
        public async Task<ActionResult> DeletePatientAccountRequest()
        {
            IActionResult result = await GetUserInfo();

            if (result is OkObjectResult okResult)
            {
                var userInfo = okResult.Value;

                string? email = userInfo?.GetType().GetProperty("Email")?.GetValue(userInfo, null)?.ToString();
                string? userId = userInfo?.GetType().GetProperty("Id")?.GetValue(userInfo, null)?.ToString();

                Console.WriteLine("EMAIL " + email);
                Console.WriteLine("USERID " + userId);

                var token = await _tokenService.CreateAccountDeletionToken(email);

                var confirmationLink = Url.Action("ConfirmPatientAccountDeletionNotProfile", "User", new { userId, token = token.TokenId }, Request.Scheme);

                Console.WriteLine("CONFIRMATION LINK: " + confirmationLink);

                _patientService.ConfirmPatientAccountDeletionEmail(confirmationLink, email);

                return Ok("An email asking for your account's deletion confirmation was sent.");
            }
            else
            {
                return BadRequest("User info could not be retrieved.");
            }
        }

        private async Task<IActionResult> GetUserInfo()
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


        // Update Deletion of Patient Account Confirmation: api/Update-PatientAccountDeletionConfirmation
        [HttpPut("Update-PatientAccountDeletionConfirmation")]
        public async Task<ActionResult> ConfirmPatientAccountDeletionNotProfile([FromQuery] string userId, [FromQuery] string token)
        {
            if (string.IsNullOrEmpty(userId) || string.IsNullOrEmpty(token))
            {
                return BadRequest("User Email and Token are required.");
            }

            if (!await _userService.UserExistsById(userId))
            {
                return NotFound("User not found.");
            }

            if (_tokenService.TokenExistsById(token) == null)
            {
                return NotFound("Token not found.");
            }

            if (await _tokenService.IsTokenExpired(token))
            {
                return Unauthorized("The provided token has expired.");
            }

            if (!await _tokenService.IsTokenActive(token))
            {
                return Unauthorized("The provided token was already used.");
            }

            var userEmail = await _userService.DeletePatientAccount(userId, token);
            // anonymize the patient's profile according to GDPR policies -> readme info for now
            _patientService.AnonymizeProfile(userEmail);

            return Ok("Patient account successfully deleted!\nSome of your non-identifiable data will be retained, as per our GDPR policies.");
        }

    }

}
