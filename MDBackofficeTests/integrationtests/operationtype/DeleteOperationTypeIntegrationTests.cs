using DDDNetCore.Controllers;
using DDDNetCore.Domain.Logs;
using DDDNetCore.Domain.OperationTypes.ValueObjects.Phase;
using DDDNetCore.Domain.OperationTypes.ValueObjects.RequiredStaff;
using DDDNetCore.Domain.OperationTypes;
using DDDNetCore.Domain.OperationTypesRecords;
using DDDNetCore.Domain.Shared;
using Microsoft.AspNetCore.Mvc;
using Moq;
using Xunit;

namespace MDBackofficeTests.integrationtests.operationtype
{
    public class DeletOperationTypesIntegrationTests
    {
        private readonly Mock<IUnitOfWork> _unitOfWorkMock = new();
        private readonly Mock<LogService> _logServiceMock = new(new Mock<IUnitOfWork>().Object, new Mock<ILogRepository>().Object);
        private readonly Mock<IOperationTypeRepository> _repoMock = new Mock<IOperationTypeRepository>();
        private readonly Mock<OperationTypeRecordService> _opRecordService;
        private readonly OperationTypeService _service;
      

        public DeletOperationTypesIntegrationTests()
        {
            _opRecordService = new Mock<OperationTypeRecordService>(_unitOfWorkMock.Object, _logServiceMock.Object, new Mock<IOperationTypeRecordRepository>().Object);

            _service = new OperationTypeService(_unitOfWorkMock.Object, _repoMock.Object, _logServiceMock.Object, _opRecordService.Object);
        }

        [Fact]
        public async Task RemoveOperationType_ValidId_ReturnsOkResult_IntegrationControllerService()
        {
            var _controller = new OperationTypesController(_service);

            // Arrange
            var operationTypeId = "test type 1";
             var phasesDto = new List<PhaseDto>
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

            var reqStaffDto = new List<RequiredStaffDto>
            {
                new RequiredStaffDto{
                    StaffQuantity = 1,
                    Function = "doctor",
                    Specialization = "ortho"
                }
            };

            var operationType = new OperationType("test type 1", 100, true, reqStaffDto,phasesDto);

            var expectedDto = new OperationTypeDto {Name ="test type 1",EstimatedDuration = 100, Status = true,RequiredStaff = reqStaffDto,Phases = phasesDto };

            _repoMock.Setup(repo => repo.GetByIdAsync(new OperationTypeId(operationTypeId))).ReturnsAsync(operationType);
            _unitOfWorkMock.Setup(u => u.CommitAsync()).ReturnsAsync(1);

            // Act
            var result = await _controller.RemoveOperationType(operationTypeId);

            // Assert
            var okResult = Assert.IsType<OkObjectResult>(result.Result);
            var actualDto = Assert.IsType<OperationTypeDto>(okResult.Value);
            Assert.Equal(expectedDto.Name, actualDto.Name);
            Assert.Equal(expectedDto.EstimatedDuration, actualDto.EstimatedDuration);
            Assert.NotEqual(expectedDto.Status, actualDto.Status);
        }

        [Fact]
        public async Task InactivateAsync_ReturnsOperationTypeDTO_IntegrationServiceDomain()
        {
            // Arrange
            var operationTypeId = "test type 1";
             var phasesDto = new List<PhaseDto>
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

            var reqStaffDto = new List<RequiredStaffDto>
            {
                new RequiredStaffDto{
                    StaffQuantity = 1,
                    Function = "doctor",
                    Specialization = "ortho"
                }
            };

            var operationTypeMock = new Mock<OperationType>("test type 1", 100, true, reqStaffDto,phasesDto);

            var expectedDto = new OperationTypeDto {Name ="test type 1",EstimatedDuration = 100, Status = true,RequiredStaff = reqStaffDto,Phases = phasesDto };

            _repoMock.Setup(repo => repo.GetByIdAsync(It.IsAny<OperationTypeId>())).ReturnsAsync(operationTypeMock.Object);
            _unitOfWorkMock.Setup(u => u.CommitAsync()).ReturnsAsync(1);

            // Act
            var result = await _service.InactivateAsync(operationTypeId);

            // Assert 
            Assert.NotNull(result);
            Assert.Equal(expectedDto.Name, result.Name);
            Assert.Equal(expectedDto.EstimatedDuration, result.EstimatedDuration);
            Assert.NotEqual(expectedDto.Status, result.Status);
        }
    }
}