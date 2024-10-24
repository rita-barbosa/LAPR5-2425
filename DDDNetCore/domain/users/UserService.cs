using System;
using System.Collections.Generic;
using System.IdentityModel.Tokens.Jwt;
using System.Linq;
using System.Security.Claims;
using System.Text;
using System.Threading.Tasks;
using DDDNetCore.Domain.Emails;
using DDDNetCore.Domain.Logs;
using DDDNetCore.Domain.Shared;
using DDDNetCore.Domain.StaffProfiles;
using DDDNetCore.Domain.Tokens;
using Microsoft.AspNetCore.Identity;
using Microsoft.Extensions.Configuration;
using Microsoft.IdentityModel.Tokens;

namespace DDDNetCore.Domain.Users
{
    public class UserService 
    {
        private readonly UserManager<User> _userManager;
        private readonly RoleManager<Role> _roleManager;
        private readonly StaffService _staffService;
        private readonly EmailService _emailService;
        private readonly TokenService _tokenService;
        private readonly LogService _logService;
        private readonly IConfiguration _configuration;
        public UserService(UserManager<User> userManager, RoleManager<Role> roleManager,
                                 StaffService staffService, LogService logService,
                                EmailService emailService, IConfiguration configuration,
                                TokenService tokenService)
        {
            _userManager = userManager;
            _roleManager = roleManager;
            _staffService = staffService;
            _emailService = emailService;
            _logService = logService;
            _tokenService = tokenService;
            _configuration = configuration;
        }

        public async Task<bool> ConfirmEmailPatient(string userId, string token)
        {
            var decodedToken = Uri.UnescapeDataString(token);
            var result = await ConfirmEmailToken(userId, decodedToken);
            if (result)
            {
                User user = await FindByIdAsync(userId);
                user.changeStatus(true);
                await UpdateAsync(user);
                return true;
            }

            return false;
        }

        public async Task<bool> ConfirmEmailStaff(string userId, string token, string newPassword)
        {
            var result = await ConfirmEmailToken(userId, token);
            if (result)
            {
                User user = await FindByIdAsync(userId);
                var resetToken = await _tokenService.GeneratePasswordResetTokenAsync(user);
                await ResetPasswordAsync(user, resetToken, newPassword);
                user.changeStatus(true);
                await UpdateAsync(user);
                return true;
            }

            return false;
        }

        public async Task<string> Login(LoginUserDto loginUserDto)
        {

            var user = await _userManager.FindByEmailAsync(loginUserDto.Email)
              ?? throw new BusinessRuleValidationException("Invalid email.");

            if (!user.Status)
            {
                throw new BusinessRuleValidationException("Account not yet activated.");
            }
            if (!await _userManager.CheckPasswordAsync(user, loginUserDto.Password))
            {
                throw new BusinessRuleValidationException("Invalid password.");
            }

            return await GenerateJwtToken(user);
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

        private async Task<bool> ConfirmEmailToken(string userId, string token)
        {
            return await _tokenService.ConfirmEmailToken(userId, token);
        }

        public async Task<bool> UserExistsById(string userId)
        {
            User user = await _userManager.FindByIdAsync(userId);
            if (user == null)
            {
                return false;
            }
            return true;
        }

        public async Task<bool> UserExistsByEmail(string email)
        {
            User user = await _userManager.FindByEmailAsync(email);
            if (user == null)
            {
                return false;
            }
            return true;
        }

        public async Task<User> FindByIdAsync(string userId)
        {
            return await _userManager.FindByIdAsync(userId);
        }

        public async Task<User> FindByEmailAsync(string userEmail)
        {
            return await _userManager.FindByEmailAsync(userEmail);
        }

        private async Task<IdentityResult> UpdateAsync(User user)
        {
            return await _userManager.UpdateAsync(user);
        }

        private async Task<IdentityResult> ResetPasswordAsync(User user, string resetToken, string newPassword)
        {
            return await _userManager.ResetPasswordAsync(user, resetToken, newPassword);
        }

        internal async void AddToRoleAsync(User user, string role)
        {
            await _userManager.AddToRoleAsync(user, role);
        }

        internal async Task<IdentityResult> CreateAsync(User user, string password)
        {
            return await _userManager.CreateAsync(user, password);
        }

        internal async Task<IdentityResult> DeleteByIdAsync(string userReference)
        {
            return await _userManager.DeleteAsync(await _userManager.FindByIdAsync(userReference));
        }

        public async Task EditUserProfile(string oldEmail, string newEmail)
        {
            User user = await _userManager.FindByEmailAsync(oldEmail) ?? throw new BusinessRuleValidationException("Can't find the currently logged in user.");
            string token = await _userManager.GenerateChangeEmailTokenAsync(user, newEmail);
            var result = await _userManager.ChangeEmailAsync(user, newEmail, token);

            if (!result.Succeeded)
            {
                throw new BusinessRuleValidationException("Unable to update the user's email.");
            }
            
            user.changeStatus(false);
            await _userManager.UpdateAsync(user);
            await SendConfirmationChangeEmail(user, oldEmail, await _userManager.GenerateEmailConfirmationTokenAsync(user));
        }

        private async Task SendConfirmationChangeEmail(User user, string email, string token)
        {
            string confirmationLink = await ConfigureUrlConfirmation(token, user);
            var body = "<p>Hello,</p><p>This email was sent to notify you that your email address has been updated in the HealthCare Clinic System. </p><p><a href='" + confirmationLink + "'>Click in the link to confirm your email change.</a></p><p>Thank you for choosing us,<br>HealthCare Clinic</p></body></html>";

            await SendEmail(email, "Update Email Confirmation", body);
        }
        private async Task SendConfirmationEmail(User user, string token)
        {
            string confirmationLink = await ConfigureUrlConfirmation(token, user);
            var body = "<p>Hello,</p><p>This email was sent to inform you that your user account in the HealthCare Clinic System has been created. </p><p><a href='" + confirmationLink + "'>Click in the link to confirm the activation of the account.</a></p><p>Thank you for choosing us,<br>HealthCare Clinic</p></body></html>";

            await SendEmail(user.Email, "Account Activation", body);
        }

        private async Task SendPasswordEmail(User user, string token)
        {
            string confirmationLink = await ConfigureUrlPasswordConfirmation(token, user);
            var body = "<p>Hello,</p><p>This email was sent to allow you to change your password in your HealthCare Clinic System account. </p><p><a href='" + confirmationLink + "'>Click in the link to change your password.</a></p><p>Thank you for choosing us,<br>HealthCare Clinic</p></body></html>";

            await SendEmail(user.Email, "Password Reset", body);
        }

        private async Task<string> ConfigureUrlConfirmation(string token, User user)
        {
            string? role = (await _userManager.GetRolesAsync(user)).FirstOrDefault();

            if (role == null) throw new NullReferenceException("Can't obtain the user role.");

            var encodedToken = Uri.EscapeDataString(token);
            var baseUrl = _configuration["App:BaseUrl"];
            return role.Equals("Patient")
            ? $"{baseUrl}/Activate-PatientAccount?userId={user.Id}&token={encodedToken}"
            : $"{baseUrl}/Activate-StaffAccount?userId={user.Id}&token={encodedToken}";
        }

        private async Task<string> ConfigureUrlPasswordConfirmation(string token, User user)
        {
            string? role = (await _userManager.GetRolesAsync(user)).FirstOrDefault();

            if (role == null) throw new NullReferenceException("Can't obtain the user role.");

            var encodedToken = Uri.EscapeDataString(token);
            var baseUrl = _configuration["App:BaseUrl"];
            if(role.Equals("Admin") || role.Equals("Technician") || role.Equals("Doctor") || role.Equals("Nurse"))
                return $"{baseUrl}/Update-UserPassword?email={user.Email}&token={encodedToken}";


            throw new BusinessRuleValidationException("Invalid user role. Only Admin, Doctor, Technician, or Nurse are allowed.");
        }

        private async Task SendEmail(string receiver, string subject, string body)
        {
            if (receiver == null) throw new NullReferenceException("The recipient's email cannot be null.");

            EmailMessageDto emailDto = new(
                           _configuration["App:Email"] ?? throw new NullReferenceException("Hospital email not configured."),
                           receiver,
                           subject,
                           body
                       );
            await _emailService.SendConfirmationEmail(emailDto);
        }

        public async Task ResetPassword(string email)
        {
            User user = await _userManager.FindByEmailAsync(email) ?? throw new BusinessRuleValidationException("Can't find the currently logged in user.");
            user.changeStatus(false);
            await _userManager.UpdateAsync(user);
            await SendPasswordEmail(user,  await _userManager.GenerateEmailConfirmationTokenAsync(user));
        }

        public async Task<bool> UpdatePassword(string email, string token, string newPassword)
        {
            if (string.IsNullOrWhiteSpace(email) || string.IsNullOrWhiteSpace(token))
                {
                    throw new Exception("User Email and Token are required.");
                }

            if (!await UserExistsByEmail(email))
            {
                throw new Exception("User not found.");
            }

            string userId = (await FindByEmailAsync(email)).Id;
            var result = await ConfirmEmailToken(userId, token);
            if (result)
            {
                User user = await FindByIdAsync(userId);
                var resetPasswordToken = await _tokenService.GeneratePasswordResetTokenAsync(user);
                await ResetPasswordAsync(user, resetPasswordToken, newPassword);
                user.changeStatus(true);
                await UpdateAsync(user);
                return true;
            }
            return false;
        }


        public async Task<string> DeleteAsync(string userId)
        {
            var user = await _userManager.FindByIdAsync(userId);

            if (user == null)
                return null;

            await _userManager.DeleteAsync(user);
            await _logService.CreateDeletionLog(user.Id.ToString(), user.GetType().Name, "Deletion of user account.");

            return user.Email;
        }

        public async Task<string> DeletePatientAccount(string userId, string token)
        {
            //token was used so make it inactive
            await _tokenService.InactivateAsync(new TokenId(token));
            //delete account
            return await DeleteAsync(userId);
        }


    }
}
