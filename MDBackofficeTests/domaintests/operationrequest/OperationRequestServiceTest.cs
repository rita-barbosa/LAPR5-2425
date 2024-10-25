using Castle.Core.Configuration;
using DDDNetCore.Domain.Emails;
using DDDNetCore.Domain.Logs;
using DDDNetCore.Domain.OperationRequest;
using DDDNetCore.Domain.OperationTypes;
using DDDNetCore.Domain.OperationTypes.ValueObjects.Phase;
using DDDNetCore.Domain.OperationTypes.ValueObjects.RequiredStaff;
using DDDNetCore.Domain.Patients;
using DDDNetCore.Domain.Shared;
using DDDNetCore.Domain.Specializations;
using DDDNetCore.Domain.StaffProfiles;
using DDDNetCore.Domain.Tokens;
using DDDNetCore.Domain.Users;
using DDDNetCore.Infrastructure;
using DDDNetCore.Infrastructure.Emails;
using Microsoft.AspNetCore.Identity;
using Microsoft.AspNetCore.JsonPatch.Internal;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.Logging;
using Microsoft.Extensions.Options;
using Moq;
using Xunit;
using IConfiguration = Microsoft.Extensions.Configuration.IConfiguration;

namespace MDBackofficeTests.domaintests.operationrequest;

public class OperationRequestServiceTests
{
    private readonly Mock<IUnitOfWork> _unitOfWorkMock = new Mock<IUnitOfWork>();
    private readonly Mock<IOperationRequestRepository> _repoMock = new Mock<IOperationRequestRepository>();
    private readonly Mock<IStaffRepository> _repoStaMock = new Mock<IStaffRepository>();
    private readonly Mock<IPatientRepository> _repoPatMock = new Mock<IPatientRepository>();
    private readonly Mock<IOperationTypeRepository> _repoOpTypeMock = new Mock<IOperationTypeRepository>();
    private readonly Mock<ISpecializationRepository> _repoSpecMock = new Mock<ISpecializationRepository>();
    private readonly Mock<LogService> _logServiceMock = new Mock<LogService>(new Mock<IUnitOfWork>().Object, new Mock<ILogRepository>().Object);
    private readonly UserService _userServiceMock;
    private readonly PatientService _patientServiceMock;

    public OperationRequestServiceTests()
    {
        var identityOptionsMock = new Mock<IOptions<IdentityOptions>>();
        identityOptionsMock.Setup(o => o.Value).Returns(new IdentityOptions());
        var identityErrorDescriberMock = new Mock<IdentityErrorDescriber>();

        var userManagerMock = new UserManager<User>(
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

        var roleManagerMock = new RoleManager<Role>(
            new Mock<IRoleStore<Role>>().Object,
            new List<IRoleValidator<Role>>(),
            new Mock<ILookupNormalizer>().Object,
            identityErrorDescriberMock.Object,
             new Mock<ILogger<RoleManager<Role>>>().Object
        );

        var tokenServiceMock = new Mock<TokenService>(_unitOfWorkMock.Object, new Mock<ITokenRepository>().Object, userManagerMock);
        var _emailServiceMock = new Mock<EmailService>(tokenServiceMock.Object, new Mock<IEmailAdapter>().Object);
        var _configurationMock = new Mock<IConfiguration>();

        _userServiceMock = new UserService(
            userManagerMock,
            roleManagerMock,
            _logServiceMock.Object,
            _emailServiceMock.Object,
           _configurationMock.Object,
            tokenServiceMock.Object 
        );
        _patientServiceMock = new PatientService(_unitOfWorkMock.Object, _logServiceMock.Object, _configurationMock.Object, _repoPatMock.Object,
                    _userServiceMock, _emailServiceMock.Object);


    }

    [Fact]
    public async Task AddAsync_ReturnsOperationRequest()
    {
        // Arrange
        var staffId = "D202400001";
        var opTyId = "tumor removal";
        var patientId = "202410000001";
        var dtoMock = new CreatingOperationRequestDto
        ("2024-12-31",
        "Elective",
            "2024-10-25",
            "Pending",
            staffId,
            "descript",
            patientId,
            opTyId
            );

        var staffMock = new Mock<Staff>("00001", "country, 12345, street test", "12345", "first", "last", "first last", "email@email.com", "+123", "12345678", "doctor", "ortho");
        var patientMock = new Mock<Patient>("first", "last", "first last", "country, 12345, street test", "female", "+123", "12345678", "98765432", "email@email.com", "2000-10-10", "000001");
        var phases = new List<PhaseDto>
        {
            new PhaseDto {
                        Description = "descrip",
                        Duration = 25
                        },
            new PhaseDto {
                        Description = "descrip2",
                        Duration = 50
                        },
            new PhaseDto {
                        Description = "descrip3",
                        Duration = 25
                        }
        };

        var reqStaff = new List<RequiredStaffDto>
        {
            new RequiredStaffDto{
                StaffQuantity = 1,
                Function = "doctor",
                Specialization = "ortho"
            }
        };

        var operationTypeMock = new Mock<OperationType>(opTyId, 100, true, reqStaff,phases);

        _repoStaMock.Setup(_repoMock => _repoMock.GetByIdAsync(It.IsAny<StaffId>())).ReturnsAsync(staffMock.Object);
        _repoPatMock.Setup(_repoPatMock => _repoPatMock.GetByIdAsync(It.IsAny<MedicalRecordNumber>()))
            .ReturnsAsync(patientMock.Object);
        _repoOpTypeMock.Setup(_repoOpTypeMock => _repoOpTypeMock.GetByIdWithStaffAsync(It.IsAny<OperationTypeId>())).ReturnsAsync(operationTypeMock.Object);

        _unitOfWorkMock.Setup(u => u.CommitAsync()).ReturnsAsync(1);


        var service = new OperationRequestService(_unitOfWorkMock.Object, _repoMock.Object,
                                                    _repoStaMock.Object, _logServiceMock.Object, _patientServiceMock,
                                                    _repoPatMock.Object, _repoOpTypeMock.Object, _userServiceMock);

        //Act
        var result = await service.AddAsync(dtoMock);

        //Assert
        Assert.NotNull(result);
        Assert.NotNull(result);
        Assert.Equal(staffId, result.StaffId);
        Assert.Equal(patientId, result.PatientId);
        Assert.Equal(opTyId, result.OperationTypeId);
        Assert.Equal(dtoMock.Description, result.Description);
        _repoMock.Verify(r => r.AddAsync(It.IsAny<OperationRequest>()), Times.Once);
        _unitOfWorkMock.Verify(u => u.CommitAsync(), Times.Once);

    }
}

