using DDDNetCore.Controllers;
using DDDNetCore.Domain.Emails;
using DDDNetCore.Domain.Logs;
using DDDNetCore.Domain.Patients;
using DDDNetCore.Domain.Shared;
using DDDNetCore.Domain.Specializations;
using DDDNetCore.Domain.StaffProfiles;
using DDDNetCore.Domain.Tokens;
using DDDNetCore.Domain.Users;
using DDDNetCore.Infrastructure.Emails;
using Microsoft.AspNetCore.Identity;
using Microsoft.Extensions.Logging;
using Microsoft.Extensions.Options;
using Moq;
using Xunit;
using Microsoft.Extensions.Configuration;
using Microsoft.AspNetCore.Http.HttpResults;
using Newtonsoft.Json.Linq;
using Microsoft.AspNetCore.Mvc;
using Microsoft.AspNetCore.Authentication;
using Microsoft.AspNetCore.Http;
using SignInResult = Microsoft.AspNetCore.Identity.SignInResult;

namespace MDBackofficeTests.controllertests
{
    public class UserControllerTests
    {
        private readonly UserController _controller;
        private readonly Mock<UserService> _userServiceMock;
        private readonly Mock<IUnitOfWork> _unitOfWorkMock = new();
        private readonly Mock<LogService> _logServiceMock = new(new Mock<IUnitOfWork>().Object, new Mock<ILogRepository>().Object);
        private readonly Mock<UserManager<User>> _userManagerMock;
        private readonly Mock<RoleManager<Role>> _roleManagerMock;
        private readonly Mock<IConfiguration> _configurationMock;
        private readonly Mock<TokenService> _tokenServiceMock;
        private readonly Mock<PatientService> _patientServiceMock;
        private readonly Mock<SignInManager<User>> _signinManagerMock;
        private readonly Mock<StaffService> _staffServiceMock;
        private readonly Mock<IStaffRepository> _staffRepoMock;
        private readonly Mock<ISpecializationRepository> _specRepoMock;

        public UserControllerTests() {
            var identityOptionsMock = new Mock<IOptions<IdentityOptions>>();
            identityOptionsMock.Setup(o => o.Value).Returns(new IdentityOptions());
            var identityErrorDescriberMock = new Mock<IdentityErrorDescriber>();

            _userManagerMock = new Mock<UserManager<User>>(
                new Mock<IUserStore<User>>().Object,
                identityOptionsMock.Object,
                new Mock<IPasswordHasher<User>>().Object,
                new List<IUserValidator<User>> { new Mock<IUserValidator<User>>().Object },
                new List<IPasswordValidator<User>> { new Mock<IPasswordValidator<User>>().Object },
                new Mock<ILookupNormalizer>().Object,
                identityErrorDescriberMock.Object,
                new Mock<IServiceProvider>().Object,
                new Mock<ILogger<UserManager<User>>>().Object
            );

            _roleManagerMock = new Mock<RoleManager<Role>>(
                new Mock<IRoleStore<Role>>().Object,
                new List<IRoleValidator<Role>>(),
                new Mock<ILookupNormalizer>().Object,
                identityErrorDescriberMock.Object,
                 new Mock<ILogger<RoleManager<Role>>>().Object
            );

            _tokenServiceMock = new Mock<TokenService>(_unitOfWorkMock.Object, new Mock<ITokenRepository>().Object, _userManagerMock.Object);
            var _emailServiceMock = new Mock<EmailService>(_tokenServiceMock.Object, new Mock<IEmailAdapter>().Object);
            _configurationMock = new Mock<IConfiguration>();

             _signinManagerMock = new Mock<SignInManager<User>>(_userManagerMock.Object,
                                                               new Mock<IHttpContextAccessor>().Object,
                                                               new Mock<IUserClaimsPrincipalFactory<User>>().Object,
                                                               identityOptionsMock.Object,
                                                               new Mock<ILogger<SignInManager<User>>>().Object,
                                                               new Mock<IAuthenticationSchemeProvider>().Object,
                                                               new Mock<IUserConfirmation<User>>().Object);

            _userServiceMock = new Mock<UserService>(_userManagerMock.Object, _roleManagerMock.Object, _logServiceMock.Object, _signinManagerMock.Object, _emailServiceMock.Object, _configurationMock.Object, _tokenServiceMock.Object);

            _patientServiceMock = new Mock<PatientService>(_unitOfWorkMock.Object, _logServiceMock.Object, _configurationMock.Object, new Mock<IPatientRepository>().Object,
                    _userServiceMock.Object, _emailServiceMock.Object);
            _staffServiceMock = new Mock<StaffService>(_unitOfWorkMock.Object, _logServiceMock.Object, new Mock<IStaffRepository>().Object, new Mock<ISpecializationRepository>().Object,
                    _userManagerMock.Object, _configurationMock.Object, _emailServiceMock.Object, _userServiceMock.Object);

            _staffRepoMock = new Mock<IStaffRepository>();
            _specRepoMock = new Mock<ISpecializationRepository>();
             _controller = new UserController(_userServiceMock.Object, _patientServiceMock.Object, _staffServiceMock.Object, _tokenServiceMock.Object);
        }

        [Fact]
        public async Task ResetPassword_ReturnsOkResult()
        {
            // Arrange
            var email = "test@email.com";
            var userMock = new Mock<User>();
            userMock.Setup(u => u.UserName).Returns(email);
            userMock.Setup(u => u.Email).Returns(email);
            userMock.Setup(u => u.Status).Returns(true);

            var resultMock = IdentityResult.Success;
            _userManagerMock.Setup(um => um.FindByEmailAsync(email)).ReturnsAsync(userMock.Object);
            _userManagerMock.Setup(um => um.UpdateAsync(userMock.Object)).ReturnsAsync(resultMock);
            _userManagerMock.Setup(um => um.GenerateEmailConfirmationTokenAsync(userMock.Object)).ReturnsAsync("test token");
            _userManagerMock.Setup(um => um.GetRolesAsync(userMock.Object)).ReturnsAsync(["Doctor"]);
            _configurationMock.Setup(c => c["App:Email"]).Returns("testemail@email.com");
            _configurationMock.Setup(c => c["App:BaseUrl"]).Returns("https://test/api");
            // Act
            var result = await _controller.ResetPassword(email);

            // Assert
            Assert.IsType<OkObjectResult>(result);
            userMock.Verify(u => u.changeStatus(false), Times.Once); // Verify that changeStatus was called
            _userManagerMock.Verify(um => um.UpdateAsync(userMock.Object), Times.Once); // Verify UpdateAsync was called
            _userManagerMock.Verify(um => um.GenerateEmailConfirmationTokenAsync(userMock.Object), Times.Once); // Verify token generation
        }

        [Fact]
        public async Task UpdatePassword_ReturnsOkResult()
        {
            // Arrange
            var email = "test@email.com";
            var id = "testid";
            var token = "test-token";
            var password = "NewPass00_d";
            var userMock = new Mock<User>();

            var dtoMock = new ConfirmEmailUserDto { NewPassword = password };

            userMock.Setup(u => u.Id).Returns(id);
            userMock.Setup(u => u.UserName).Returns(email);
            userMock.Setup(u => u.Email).Returns(email);
            userMock.Setup(u => u.Status).Returns(true);

            var resultMock = IdentityResult.Success;
            _userManagerMock.Setup(_userManagerMock => _userManagerMock.FindByEmailAsync(email)).ReturnsAsync(userMock.Object);
            _userManagerMock.Setup(um => um.FindByIdAsync(id)).ReturnsAsync(userMock.Object);
            _userManagerMock.Setup(um => um.ConfirmEmailAsync(userMock.Object, token)).ReturnsAsync(resultMock);
            _tokenServiceMock.Setup(_tokenServiceMock => _tokenServiceMock.ConfirmEmailToken(id, token)).ReturnsAsync(true);
            _tokenServiceMock.Setup(_tokenServiceMock => _tokenServiceMock.GeneratePasswordResetTokenAsync(userMock.Object)).ReturnsAsync(token);
            _userManagerMock.Setup(um => um.ResetPasswordAsync(userMock.Object, token, password)).ReturnsAsync(resultMock);
            _userManagerMock.Setup(um => um.UpdateAsync(userMock.Object)).ReturnsAsync(resultMock);
            // Act
            var result = await _controller.UpdatePassword(email, token, dtoMock);

            // Assert
            Assert.IsType<OkObjectResult>(result);
            _userManagerMock.Verify(um => um.UpdateAsync(userMock.Object), Times.Once);
            _userManagerMock.Verify(um => um.ResetPasswordAsync(userMock.Object, token, password), Times.Once);
        }

        [Fact]
        public async Task LoginCorrectCredentials_ReturnsTokenSuccessfully()
        {
            //Arrange
            var email = "test@email.com";
            var id = "testid";
            var password = "NewPass00_d";

            var dtoMock = new LoginUserDto { Email = email, Password = password };

            var userMock = new Mock<User>();


            userMock.Setup(u => u.Id).Returns(id);
            userMock.Setup(u => u.UserName).Returns(email);
            userMock.Setup(u => u.Email).Returns(email);
            userMock.Setup(u => u.Status).Returns(true);
            userMock.Setup(u => u.PasswordHash).Returns(password);

            
            _userManagerMock.Setup(_userManagerMock => _userManagerMock.FindByEmailAsync(email)).ReturnsAsync(userMock.Object);
            _signinManagerMock.Setup(sm => sm.PasswordSignInAsync(userMock.Object, password, false, true)).ReturnsAsync(SignInResult.Success);
            _userManagerMock.Setup(um => um.GetRolesAsync(userMock.Object)).ReturnsAsync(["Doctor"]);
            _configurationMock.Setup(c => c["Jwt:Key"]).Returns("AHSVFOSDYUDASJFNhvOUVu897GB876arbvn568n6CN865Vn5NFFn86f87Cb76cNvnVNYvhgvnu7676");
            _configurationMock.Setup(c => c["Jwt:Issuer"]).Returns("testissuer");
            _configurationMock.Setup(c => c["Jwt:Audiend"]).Returns("testaudience");

            //Act
            var result = await _controller.Login(dtoMock);

            //Assert
            Assert.IsType<OkObjectResult>(result);
            _signinManagerMock.Verify(um => um.PasswordSignInAsync(userMock.Object, password, false, true), Times.Once);
        }


        [Fact]
        public async Task LoginInvalidCredentials_ReturnsTokenSuccessfully()
        {
            //Arrange
            var email = "test@email.com";
            var id = "testid";
            var password = "NewPass00_d";

            var dtoMock = new LoginUserDto { Email = email, Password = "wrong-password" };

            var userMock = new Mock<User>();
            userMock.Setup(u => u.Id).Returns(id);
            userMock.Setup(u => u.UserName).Returns(email);
            userMock.Setup(u => u.Email).Returns(email);
            userMock.Setup(u => u.Status).Returns(true);
            userMock.Setup(u => u.PasswordHash).Returns(password);

            
            _userManagerMock.Setup(_userManagerMock => _userManagerMock.FindByEmailAsync(email)).ReturnsAsync(userMock.Object);
            _signinManagerMock.Setup(sm => sm.PasswordSignInAsync(userMock.Object, dtoMock.Password, false, true)).ReturnsAsync(SignInResult.Failed);
            _userManagerMock.Setup(um => um.GetRolesAsync(userMock.Object)).ReturnsAsync(["Doctor"]);
            _configurationMock.Setup(c => c["Jwt:Key"]).Returns("AHSVFOSDYUDASJFNhvOUVu897GB876arbvn568n6CN865Vn5NFFn86f87Cb76cNvnVNYvhgvnu7676");
            _configurationMock.Setup(c => c["Jwt:Issuer"]).Returns("testissuer");
            _configurationMock.Setup(c => c["Jwt:Audiend"]).Returns("testaudience");


            //Act
            var result = await _controller.Login(dtoMock);

            //Assert
            Assert.IsType<BadRequestObjectResult>(result);
        }

        [Fact]
        public async Task ConfirmPatientAccountDeletionNotProfile_SuccessReturnsOk()
        {
            // Arrange
            var userId = "testUserId";
            var token = "validToken";
            var userEmail = "user@example.com";

            _userServiceMock.Setup(u => u.UserExistsById(userId)).ReturnsAsync(true);
            _tokenServiceMock.Setup(t => t.TokenExistsById(token)).ReturnsAsync(true);
            _tokenServiceMock.Setup(t => t.IsTokenExpired(token)).ReturnsAsync(false);
            _tokenServiceMock.Setup(t => t.IsTokenActive(token)).ReturnsAsync(true);
            _userServiceMock.Setup(u => u.DeletePatientAccount(userId, token)).ReturnsAsync(userEmail);

            // Act
            var result = await _controller.ConfirmPatientAccountDeletionNotProfile(userId, token);

            // Assert
            _patientServiceMock.Verify(p => p.AnonymizeProfile(userEmail), Times.Once);
            Assert.IsType<OkObjectResult>(result);
            Assert.Equal("Patient account successfully deleted!\nSome of your non-identifiable data will be retained, as per our GDPR policies.", ((OkObjectResult)result).Value);
        }

        
        // [Fact]
        // public async Task RegisterStaffUser_ReturnsOkResult()
        // {
        //     // Arrange
        //     var email = "test@email.com";
        //     var password = "NewPass00_d";
        //     var role = "doctor";
        //     var id = "testid";
        //     var phone = "+351 960444772";

        //     var dtoMock = new RegisterUserDto 
        //     { 
        //         Email = email, 
        //         Password = password, 
        //         Phone = phone, 
        //         Role = role 
        //     };

        //     var userMock = new Mock<User>();
        //     userMock.Setup(u => u.Id).Returns(id);
        //     userMock.Setup(u => u.UserName).Returns(email);
        //     userMock.Setup(u => u.Email).Returns(email);
        //     userMock.Setup(u => u.Status).Returns(true);
        //     userMock.Setup(u => u.PasswordHash).Returns(password);


        //     var resultMock = IdentityResult.Success;
        //     var address = "Portugal, 4570-860, Rua das Oliveiras";
        //     var specialization = "Orthopedics";
        //     var seqNumber = "00001";

        //                 var specMock = new Mock<Specialization>(specialization);

        //     var staffMock = new Mock<Staff>(seqNumber, address, "12345", "Rita", "Barbosa", "Rita Barbosa", email, "+351", "987654321", "Doctor", specialization);
        //     var staffDto = new StaffDto(staffMock.Object.Id.Value, staffMock.Object.Name.ToString(), staffMock.Object.Phone.ToString(), staffMock.Object.Email.ToString(), staffMock.Object.Address.ToString(), staffMock.Object.SpecializationId.Value, [] );
        //     var createStaffProfile = new CreatingStaffDto(
        //         staffMock.Object.LicenseNumber.Number,
        //         address,
        //         staffMock.Object.Name.FirstName,
        //         staffMock.Object.Name.LastName,
        //         phone,
        //         email,
        //         role,
        //         specialization
        //     );

        //     _roleManagerMock.Setup(rm => rm.RoleExistsAsync(role)).ReturnsAsync(true);
        //     _userManagerMock.Setup(um => um.CreateAsync(It.IsAny<User>(), password)).ReturnsAsync(resultMock);
        //     _userManagerMock.Setup(um => um.AddToRoleAsync(It.IsAny<User>(), role)).ReturnsAsync(resultMock);

        //     var phoneParts = phone.Split(' ');
        //     var token = "CfDJ8KD8LY591fpNkZ3b0mdMEjS%2FwICNeSVZtBH%2BuWjgbM9nRHXVdZ4FfMKoz3IgZCOKrqHJ174qTbTu643CvatPrcm0CL2n7nUZD7WabvBsKinP%2BUH1miQtuGKYMMuVkospyKh1uY9QI1eWqNHqqt77EOMpk9fQdR65DqPFbetshw1zgC7PYptcxP90DSDJ9n85EJ9tfvDtyvjxIL6l7wLwBzEhHazvOSZGVob%2FlbPetI6SU9vH%2FTrHUvinVzf4XtXLOg%3D%3D";


        //     _staffRepoMock.Setup(repo => repo.ExistsStaffWithEmailOrPhone(email, phoneParts[0], phoneParts[1])).ReturnsAsync(true);
        //     _staffRepoMock.Setup(repo => repo.GetStaffWithEmail(email)).ReturnsAsync(staffMock.Object);
        //     staffMock.Setup(s => s.AddUser(userMock.Object));
        //     _unitOfWorkMock.Setup(unit => unit.CommitAsync());

        //     _staffRepoMock.Setup(s => s.FindStaffWithEmailOrPhone(email, phoneParts[0], phoneParts[1])).ReturnsAsync(staffMock.Object);
        //     _userManagerMock.Setup(u => u.GenerateEmailConfirmationTokenAsync(userMock.Object)).ReturnsAsync(token);

        //     // Act
        //     var result = await _controller.RegisterStaffUser(dtoMock);

        //     // Assert
        //     Assert.IsType<OkObjectResult>(result); // Verify we get an Ok result
        //     var okResult = result as OkObjectResult;
        //     Assert.Equal("The user has been successfully created. Please verify your email to complete the registration.", okResult.Value);

        //     // Verify interactions
        //     _userManagerMock.Verify(um => um.CreateAsync(It.IsAny<User>(), password), Times.Once);
        //     _userManagerMock.Verify(um => um.AddToRoleAsync(It.IsAny<User>(), role), Times.Once);
        //     _staffServiceMock.Verify(staffService => staffService.AddUser(It.IsAny<User>(), email, phone), Times.Once);
        //     _staffServiceMock.Verify(staffService => staffService.GetProfileEmail(email, phone), Times.Once);
        //     _userServiceMock.Verify(userService => userService.SendConfirmationEmail(It.IsAny<User>(), email), Times.Once);
        // }

    }
}