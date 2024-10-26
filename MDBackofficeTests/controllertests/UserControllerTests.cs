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

namespace MDBackofficeTests.controllertests
{
    public class UserControllerTests
    {
        private readonly UserController _controller;
        private readonly Mock<UserService> _userServiceMock;
        private readonly Mock<IUnitOfWork> _unitOfWorkMock = new();
        private readonly Mock<LogService> _logServiceMock = new(new Mock<IUnitOfWork>().Object, new Mock<ILogRepository>().Object);
        private readonly Mock<UserManager<User>> _userManagerMock;
        private readonly Mock<IConfiguration> _configurationMock;
        private readonly Mock<TokenService> _tokenServiceMock;


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

            var roleManagerMock = new Mock<RoleManager<Role>>(
                new Mock<IRoleStore<Role>>().Object,
                new List<IRoleValidator<Role>>(),
                new Mock<ILookupNormalizer>().Object,
                identityErrorDescriberMock.Object,
                 new Mock<ILogger<RoleManager<Role>>>().Object
            );

            _tokenServiceMock = new Mock<TokenService>(_unitOfWorkMock.Object, new Mock<ITokenRepository>().Object, _userManagerMock.Object);
            var _emailServiceMock = new Mock<EmailService>(_tokenServiceMock.Object, new Mock<IEmailAdapter>().Object);
            _configurationMock = new Mock<IConfiguration>();
              
            _userServiceMock = new Mock<UserService>(
                _userManagerMock.Object,
                roleManagerMock.Object,
                _logServiceMock.Object,
                _emailServiceMock.Object,
                _configurationMock.Object,
                _tokenServiceMock.Object
            );
            var _patientServiceMock = new Mock<PatientService>(_unitOfWorkMock.Object, _logServiceMock.Object, _configurationMock.Object, new Mock<IPatientRepository>().Object,
                    _userServiceMock.Object, _emailServiceMock.Object);
            var _staffServiceMock = new Mock<StaffService>(_unitOfWorkMock.Object, _logServiceMock.Object, new Mock<IStaffRepository>().Object, new Mock<ISpecializationRepository>().Object,
                    _userManagerMock.Object, _configurationMock.Object, _emailServiceMock.Object, _userServiceMock.Object);

            
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

            _userManagerMock.Setup(_userManagerMock => _userManagerMock.FindByEmailAsync(email)).ReturnsAsync(userMock.Object);//
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
    }
}