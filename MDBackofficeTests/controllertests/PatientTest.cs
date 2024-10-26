using DDDNetCore.Domain.Patients;
using DDDNetCore.Domain.Emails;
using DDDNetCore.Domain.Shared;
using DDDNetCore.Domain.Users;
using DDDNetCore.Domain.Logs;
using DDDNetCore.Domain.Tokens;
using DDDNetCore.Infrastructure.Emails;
using Microsoft.AspNetCore.Identity;
using Microsoft.Extensions.Logging;
using Microsoft.Extensions.Options;
using Moq;
using Xunit;
using IConfiguration = Microsoft.Extensions.Configuration.IConfiguration;
using DDDNetCore.Controllers;
using Microsoft.AspNetCore.Mvc;


namespace MDBackofficeTests.controllertests
{
    public class PatientTests{

        private readonly Mock<PatientService> _service;
        private readonly Mock<IUnitOfWork> _unitOfWorkMock = new Mock<IUnitOfWork>();
        private readonly Mock<IPatientRepository> _repoMock = new Mock<IPatientRepository>();

        public PatientTests()
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
            
            _service = new Mock<PatientService>(_unitOfWorkMock.Object, _logServiceMock.Object, 
                                            _configurationMock.Object, _repoMock.Object, 
                                            _userServiceMock.Object, _emailServiceMock.Object);

        }

        [Fact]
        public async Task CreatePatientProfile_Returns_CreatedResult()
        {
            //Arrage
            var controller = new PatientController(_service.Object);

            var dtoMock = new CreatingPatientDto
                ("Rita",
                "Barbosa",
                "Portugal, 4590-850, Rua da Sardinha",
                "+351 910000000",
                "ritabarbosa@email.com",
                "+351 912345678",
                "Female",
                "2004-12-15");

            _unitOfWorkMock.Setup(u => u.CommitAsync()).ReturnsAsync(1);

            //Act
            var result = await controller.CreatePatientProfile(dtoMock);

            //Assert
            var actionResult = Assert.IsType<ActionResult<PatientDto>>(result);
            var createdAtActionResult = Assert.IsType<CreatedAtActionResult>(actionResult.Result);
            Assert.Equal("GetPatientById", createdAtActionResult.ActionName);
        }
    }
}

