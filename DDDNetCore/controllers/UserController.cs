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

                string profileEmail = await _patientService.GetProfileEmail(user.Email.ToString(), registerPatientUserDto.Phone);

                SendConfirmationEmail(user, profileEmail, "Patient"); 
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

                string profileEmail = await _staffService.GetProfileEmail(user.Email.ToString(), registerUserDto.Phone);
            
                SendConfirmationEmail(user, profileEmail, registerUserDto.Role);
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



        private async void SendConfirmationEmail(User user, string email, string role)
        {
            string token = await _userManager.GenerateEmailConfirmationTokenAsync(user);

            var confirmationLink = "";

            if (role.Equals("Patient"))
            {
                confirmationLink = Url.Action("ConfirmEmailPatient", "User", new { userId = user.Id, token = token }, Request.Scheme);
            }
            else
            {
                confirmationLink = Url.Action("ConfirmEmailStaff", "User", new { userId = user.Id, token = token }, Request.Scheme);
            }

            EmailMessageDto emailDto = new EmailMessageDto(
                hospitalEmail,
                email,
                "Account Activation",
                "<p>Hello,</p><p>This email was sent to inform you that your user account in the HealthCare Clinic System has been created. </p><p><a href='" + confirmationLink + "'>Click in the link to confirm the activation of the account.</a></p><p>Thank you for choosing us,<br>HealthCare Clinic</p></body></html>"
            );

            await _emailService.SendConfirmationEmail(emailDto);
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
