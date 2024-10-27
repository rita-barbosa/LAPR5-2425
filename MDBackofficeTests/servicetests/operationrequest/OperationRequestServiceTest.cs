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
using Microsoft.AspNetCore.Authentication;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Identity;
using Microsoft.AspNetCore.JsonPatch.Internal;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.Logging;
using Microsoft.Extensions.Options;
using Moq;
using Xunit;

namespace MDBackofficeTests.servicetests.operationrequest;
public class OperationRequestServiceTests
{
    private readonly Mock<IUnitOfWork> _unitOfWorkMock = new Mock<IUnitOfWork>();
    private readonly Mock<IOperationRequestRepository> _repoMock = new Mock<IOperationRequestRepository>();
    private readonly Mock<IStaffRepository> _repoStaMock = new Mock<IStaffRepository>();
    private readonly Mock<IPatientRepository> _repoPatMock = new Mock<IPatientRepository>();
    private readonly Mock<IOperationTypeRepository> _repoOpTypeMock = new Mock<IOperationTypeRepository>();
    private readonly Mock<LogService> _logServiceMock = new Mock<LogService>(new Mock<IUnitOfWork>().Object, new Mock<ILogRepository>().Object);
    private readonly Mock<UserService> _userServiceMock;
    private readonly Mock<PatientService> _patientServiceMock;
    private readonly OperationRequestService _service;

    public OperationRequestServiceTests()
    {
        var identityOptionsMock = new Mock<IOptions<IdentityOptions>>();
        identityOptionsMock.Setup(o => o.Value).Returns(new IdentityOptions());
        var identityErrorDescriberMock = new Mock<IdentityErrorDescriber>();

        var userManagerMock = new Mock<UserManager<User>>(
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

        var tokenServiceMock = new Mock<TokenService>(_unitOfWorkMock.Object, new Mock<ITokenRepository>().Object, userManagerMock.Object);
        var _emailServiceMock = new Mock<EmailService>(tokenServiceMock.Object, new Mock<IEmailAdapter>().Object);
        var _configurationMock = new Mock<IConfiguration>();

        var signinManagerMock = new Mock<SignInManager<User>>(userManagerMock.Object,
                                                               new Mock<IHttpContextAccessor>().Object,
                                                               new Mock<IUserClaimsPrincipalFactory<User>>().Object,
                                                               identityOptionsMock.Object,
                                                               new Mock<ILogger<SignInManager<User>>>().Object,
                                                               new Mock<IAuthenticationSchemeProvider>().Object,
                                                               new Mock<IUserConfirmation<User>>().Object);
        _userServiceMock = new Mock<UserService>(
                userManagerMock.Object,
                roleManagerMock.Object,
                _logServiceMock.Object,
                signinManagerMock.Object,
                _emailServiceMock.Object,
                _configurationMock.Object,
                tokenServiceMock.Object
            );
        _patientServiceMock = new Mock<PatientService>(_unitOfWorkMock.Object, _logServiceMock.Object, _configurationMock.Object, _repoPatMock.Object,
                    _userServiceMock.Object, _emailServiceMock.Object);

        _service = new OperationRequestService(_unitOfWorkMock.Object, _repoMock.Object,
                                                    _repoStaMock.Object, _logServiceMock.Object, _patientServiceMock.Object,
                                                    _repoPatMock.Object, _repoOpTypeMock.Object, _userServiceMock.Object);
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
                                                    _repoStaMock.Object, _logServiceMock.Object, _patientServiceMock.Object,
                                                    _repoPatMock.Object, _repoOpTypeMock.Object, _userServiceMock.Object);

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


    [Fact]
    public async Task GetOperationRequestByFiltersAsync_ReturnsCorrectOperationRequestDtos()
    {
        // Arrange
        var staffId = "D202400001";
        var opTyId = "tumor removal";
        var patientId = "202410000001";
        var email = "email@email.com";
        var priority = "Elective";
        var name = "John Doe";
        var operationType = "tumor removal";
        var status = "Requested";
        var dateOfRequest = "2024-10-25";
        var deadLineDate = "2024-12-31";

        var staffMock = new Staff("00001", "country, 12345, street test", "12345", "first", "last", "first last", "email@email.com", "+123", "12345678", "doctor", "ortho");
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

        var operationRequests = new List<Mock<OperationRequest>>
        {
            new Mock<OperationRequest>("TestCode","2024-12-31","Elective","2024-10-25",
            staffId,"descript",patientId,opTyId),
        };

        var expectedDtos = new List<ListOperationRequestDto> 
        {
            new ListOperationRequestDto("first last", "tumor removal", "Requested"),
        };

        _repoStaMock.Setup(repo => repo.GetStaffWithEmail(email)).ReturnsAsync(staffMock);

        var operationRequestObjects = operationRequests.Select(mock => mock.Object).ToList();
        _repoMock.Setup(repo => repo.FindAllConditioned(new StaffId(staffId), name, priority, operationType, status, dateOfRequest, deadLineDate))
            .ReturnsAsync(operationRequestObjects);
        _repoPatMock.Setup(repo => repo.GetByIdAsync(new MedicalRecordNumber(patientId))).ReturnsAsync(patientMock.Object);


        // Act
        var result = await _service.GetOperationRequestByFiltersAsync(email, name, priority, operationType, status, dateOfRequest, deadLineDate);

        // Assert
        Assert.Equal(expectedDtos.Count, result.Count);
        for (int i = 0; i < expectedDtos.Count; i++)
        {
            Assert.Equal(expectedDtos[i].PatientName, result[i].PatientName);
            Assert.Equal(expectedDtos[i].OperationType, result[i].OperationType);
            Assert.Equal(expectedDtos[i].Status, result[i].Status);
        }

        _repoMock.Verify(repo => repo.FindAllConditioned(new StaffId(staffId), name, priority, operationType, status, dateOfRequest, deadLineDate), Times.Once);

    }

    
}

