using DDDNetCore.Controllers;
using DDDNetCore.Domain.Emails;
using DDDNetCore.Domain.Logs;
using DDDNetCore.Domain.Patients;
using DDDNetCore.Domain.Shared;
using DDDNetCore.Domain.Tokens;
using DDDNetCore.Domain.Users;
using DDDNetCore.Infrastructure.Emails;
using Microsoft.AspNetCore.Identity;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.Logging;
using Microsoft.Extensions.Options;
using Moq;
using Xunit;

namespace MDBackofficeTests.integrationtests.patient
{
    public class UpdatePatientProfileIntegrationTests
    {
        private readonly Mock<IUnitOfWork> _unitOfWorkMock = new Mock<IUnitOfWork>();
        private readonly Mock<IPatientRepository> _repoMock = new Mock<IPatientRepository>();
        private readonly Mock<IConfiguration> _configurationMock = new Mock<IConfiguration>();
        private readonly Mock<LogService> _logServiceMock;
        private readonly Mock<UserService> _userServiceMock;
        private readonly Mock<EmailService> _emailServiceMock;
        private readonly PatientService _service;


        public UpdatePatientProfileIntegrationTests()
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
            _service = new PatientService(_unitOfWorkMock.Object, _logServiceMock.Object,
                                         _configurationMock.Object, _repoMock.Object,
                                         _userServiceMock.Object, _emailServiceMock.Object);
        }

        [Fact]
        public async Task EditPatientProfile_ReturnsOkPatientDto_IntegrationControllerService()
        {
            //Arrange
            var _controller = new PatientController(_service);


            var dtoMock = new EditPatientDto
            ("Rita Barbosa",
              "+351 910000000",
              "ritabarbosa@email.com",
              "Test, 1234-234, Test Test",
              "2004-12-15");

            var patientMock = new Mock<Patient>("first", "last", "first last", "country, 12345, street test", "female", "+123", "12345678", "98765432", "email@email.com", "2000-10-10", "000001");
            var id = "202410000001";

            var dtoResult = new PatientDto("Rita Barbosa", "+351 910000000", "ritabarbosa@email.com", "Test, 1234-234, Test Test", "2004-12-15", id);

            _repoMock.Setup(_repoPatMock => _repoPatMock.GetByIdAsync(It.IsAny<MedicalRecordNumber>()))
                .ReturnsAsync(patientMock.Object);
            _unitOfWorkMock.Setup(u => u.CommitAsync()).ReturnsAsync(1);

            //Act
            var result = await _controller.EditPatientProfile(id, dtoMock);

            //Assert
            var okResult = Assert.IsType<OkObjectResult>(result.Result);
            var returnedPatient = Assert.IsType<PatientDto>(okResult.Value);
            Assert.Equal(dtoResult.Name, returnedPatient.Name);
            Assert.Equal(dtoResult.Phone, returnedPatient.Phone);
            Assert.Equal(dtoResult.Email, returnedPatient.Email);
            Assert.Equal(dtoResult.Address, returnedPatient.Address);
            Assert.Equal(dtoResult.DateBirth, returnedPatient.DateBirth);
            Assert.Equal(dtoResult.PatientId, returnedPatient.PatientId);
        }


        [Fact]
        public async Task UpdateAsync_ReturnsPatientDto_IntegrationServiceDomain()
        {
            //Arrange
      
            var dtoMock = new EditPatientDto
            ("Rita Barbosa",
              "+351 910000000",
              "ritabarbosa@email.com",
              "Test, 1234-234, Test Test",
              "2004-12-15");

            var patient = new Patient("first", "last", "first last", "country, 12345, street test", "female", "+123", "12345678", "98765432", "email@email.com", "2000-10-10", "000001");
            var id = "202410000001";

            var dtoResult = new PatientDto("Rita Barbosa", "+351 910000000", "ritabarbosa@email.com", "Test, 1234-234, Test Test", "2004-12-15", id);

            _repoMock.Setup(_repoPatMock => _repoPatMock.GetByIdAsync(It.IsAny<MedicalRecordNumber>()))
                .ReturnsAsync(patient);
            _unitOfWorkMock.Setup(u => u.CommitAsync()).ReturnsAsync(1);

            //Act
            var result = await _service.UpdateAsync(id, dtoMock);

            //Assert
            Assert.NotNull(result);
            Assert.Equal(dtoResult.Name, result.Name);
            Assert.Equal(dtoResult.Phone, result.Phone);
            Assert.Equal(dtoResult.Email, result.Email);
            Assert.Equal(dtoResult.Address, result.Address);
            Assert.Equal(dtoResult.DateBirth, result.DateBirth);
            Assert.Equal(dtoResult.PatientId, result.PatientId);
        }
    }
}
