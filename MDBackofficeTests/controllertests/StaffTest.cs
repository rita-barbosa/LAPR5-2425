using DDDNetCore.Domain.Logs;
using DDDNetCore.Domain.Emails;
using DDDNetCore.Domain.Shared;
using DDDNetCore.Domain.Specializations;
using DDDNetCore.Domain.Users;
using DDDNetCore.Domain.Tokens;
using DDDNetCore.Infrastructure.Emails;
using Microsoft.AspNetCore.Identity;
using Microsoft.Extensions.Logging;
using Microsoft.Extensions.Options;
using Moq;
using Xunit;
using IConfiguration = Microsoft.Extensions.Configuration.IConfiguration;
using DDDNetCore.Domain.StaffProfiles;
using DDDNetCore.Controllers;
using Microsoft.AspNetCore.Mvc;

namespace MDBackofficeTests.controllertests
{
    public class StaffTest
    {
        private readonly Mock<StaffService> _service;
        private readonly Mock<IUnitOfWork> _unitOfWorkMock = new Mock<IUnitOfWork>();
        private readonly Mock<IStaffRepository> _repoMock = new Mock<IStaffRepository>();
        private readonly Mock<ISpecializationRepository> _repoSpecMock = new Mock<ISpecializationRepository>();

        public StaffTest()
        {
            Mock<LogService> _logServiceMock = new Mock<LogService>(new Mock<IUnitOfWork>().Object, new Mock<ILogRepository>().Object);

            var identityOptionsMock = new Mock<IOptions<IdentityOptions>>();
            identityOptionsMock.Setup(o => o.Value).Returns(new IdentityOptions());
            var identityErrorDescriberMock = new Mock<IdentityErrorDescriber>();

            var userManagerMock = new Mock<UserManager<User>>(new Mock<IUserStore<User>>().Object, identityOptionsMock.Object, new Mock<IPasswordHasher<User>>().Object, new List<IUserValidator<User>> { new Mock<IUserValidator<User>>().Object }, new List<IPasswordValidator<User>> { new Mock<IPasswordValidator<User>>().Object }, new Mock<ILookupNormalizer>().Object, identityErrorDescriberMock.Object, new Mock<IServiceProvider>().Object, new Mock<ILogger<UserManager<User>>>().Object);
            var roleManagerMock = new Mock<RoleManager<Role>>(new Mock<IRoleStore<Role>>().Object, new List<IRoleValidator<Role>>(), new Mock<ILookupNormalizer>().Object, identityErrorDescriberMock.Object, new Mock<ILogger<RoleManager<Role>>>().Object);

            var tokenServiceMock = new Mock<TokenService>(_unitOfWorkMock.Object, new Mock<ITokenRepository>().Object, userManagerMock.Object);
            var _emailServiceMock = new Mock<EmailService>(tokenServiceMock.Object, new Mock<IEmailAdapter>().Object);
            var _configurationMock = new Mock<IConfiguration>();

            var _userServiceMock = new Mock<UserService>(userManagerMock.Object, roleManagerMock.Object, _logServiceMock.Object, _emailServiceMock.Object, _configurationMock.Object, tokenServiceMock.Object);
            
            _service = new Mock<StaffService>(_unitOfWorkMock.Object, _logServiceMock.Object,
                                            _repoMock.Object, _repoSpecMock.Object,
                                            userManagerMock.Object, _configurationMock.Object, _emailServiceMock.Object,
                                            _userServiceMock.Object);
        }

        [Fact]
        public async Task CreateStaffProfile_ReturnsOkResult()
        {
            // Arrange
            var controller = new StaffController(_service.Object);

            var specializationId = "Ortopethics";
            var dtoMock = new CreatingStaffDto
            ("12345",
                "Portugal, 4590-850, Rua da Sardinha",
                "Rita",
                "Barbosa",
                "+351 910000022",
                "something@email.com",
                "doctor",
                specializationId
                );

            var specializationMock = new Mock<Specialization>("Ortopethics");

            _repoSpecMock.Setup(_repoSpecMock => _repoSpecMock.GetByIdAsync(It.IsAny<SpecializationDenomination>()))
                .ReturnsAsync(specializationMock.Object);

            _unitOfWorkMock.Setup(u => u.CommitAsync()).ReturnsAsync(1);

            // Act
            var result = await controller.CreateStaffProfile(dtoMock);

            // Assert
            var actionResult = Assert.IsType<ActionResult<StaffDto>>(result); // Ensure it's the correct ActionResult type
            var okObjectResult = Assert.IsType<OkObjectResult>(actionResult.Result);
            Assert.NotNull(okObjectResult.Value);
            
        }

    }
}