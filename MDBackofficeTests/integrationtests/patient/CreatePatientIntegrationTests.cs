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


namespace MDBackofficeTests.integrationtests.patient
{
    public class CreatePatientIntegrationTests
    {
        private readonly Mock<IUnitOfWork> _unitOfWorkMock = new Mock<IUnitOfWork>();
        private readonly Mock<IPatientRepository> _repoMock = new Mock<IPatientRepository>();
        private readonly Mock<IConfiguration> _configurationMock = new Mock<IConfiguration>();
        private readonly Mock<LogService> _logServiceMock;
        private readonly Mock<UserService> _userServiceMock;
        private readonly Mock<EmailService> _emailServiceMock;


        public CreatePatientIntegrationTests()
        {
            _logServiceMock = new Mock<LogService>(new Mock<IUnitOfWork>().Object, new Mock<ILogRepository>().Object);

            var identityOptionsMock = new Mock<IOptions<IdentityOptions>>();
            identityOptionsMock.Setup(o => o.Value).Returns(new IdentityOptions());
            var identityErrorDescriberMock = new Mock<IdentityErrorDescriber>();

            
            var userManagerMock = new Mock<UserManager<User>>(new Mock<IUserStore<User>>().Object, identityOptionsMock.Object, new Mock<IPasswordHasher<User>>().Object, new List<IUserValidator<User>> { new Mock<IUserValidator<User>>().Object }, new List<IPasswordValidator<User>> { new Mock<IPasswordValidator<User>>().Object }, new Mock<ILookupNormalizer>().Object, identityErrorDescriberMock.Object, new Mock<IServiceProvider>().Object, new Mock<ILogger<UserManager<User>>>().Object);
            var roleManagerMock = new Mock<RoleManager<Role>>(new Mock<IRoleStore<Role>>().Object, new List<IRoleValidator<Role>>(), new Mock<ILookupNormalizer>().Object, identityErrorDescriberMock.Object, new Mock<ILogger<RoleManager<Role>>>().Object);

            var tokenServiceMock = new Mock<TokenService>(_unitOfWorkMock.Object, new Mock<ITokenRepository>().Object, userManagerMock.Object);
            var _emailServMock = new Mock<EmailService>(tokenServiceMock.Object, new Mock<IEmailAdapter>().Object);
            var _configurationMock = new Mock<IConfiguration>();

            _userServiceMock = new Mock<UserService>(userManagerMock.Object, roleManagerMock.Object, _logServiceMock.Object, _emailServMock.Object, _configurationMock.Object, tokenServiceMock.Object);
            _emailServiceMock = new Mock<EmailService>(tokenServiceMock.Object, new Mock<IEmailAdapter>().Object);

        }
        

        [Fact]
        public async Task CreateWithValidData_ReturnsCreatedResult_IntegrationControllerService()
        {
            // Pass mocked dependencies to PatientService
            var _service = new PatientService(_unitOfWorkMock.Object, _logServiceMock.Object, 
                                            _configurationMock.Object, _repoMock.Object, 
                                            _userServiceMock.Object, _emailServiceMock.Object);

            var _controller = new PatientController(_service);

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
            var result = await _controller.CreatePatientProfile(dtoMock);

            //Assert
            var actionResult = Assert.IsType<ActionResult<PatientDto>>(result);
            var createdAtActionResult = Assert.IsType<CreatedAtActionResult>(actionResult.Result);
            Assert.Equal("GetPatientById", createdAtActionResult.ActionName);
            _unitOfWorkMock.Verify(u => u.CommitAsync(), Times.Once);

        }

        [Fact]
        public async Task CreatePatientProfile_ReturnsPatient_IntegrationServiceDomain()
        {
            // Pass mocked dependencies to PatientService
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

            var service = new PatientService(_unitOfWorkMock.Object, _logServiceMock.Object,
                                                _configurationMock.Object, _repoMock.Object,
                                                _userServiceMock.Object, _emailServiceMock.Object);     

            //Act
            var result = await service.CreatePatientProfile(dtoMock);

            //Assert
            Assert.NotNull(result);
            Assert.Equal(dtoMock.Phone, result.Phone);
            Assert.Equal(dtoMock.Email, result.Email);
            Assert.Equal(dtoMock.Address, result.Address);
            Assert.Equal(dtoMock.DateBirth, result.DateBirth);        
            _repoMock.Verify(r => r.AddAsync(It.IsAny<Patient>()), Times.Once);
            _unitOfWorkMock.Verify(u => u.CommitAsync(), Times.Once);

        }
    }
}