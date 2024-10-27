using DDDNetCore.Domain.Logs;
using DDDNetCore.Domain.OperationTypes;
using DDDNetCore.Domain.OperationTypes.ValueObjects;
using DDDNetCore.Domain.OperationTypes.ValueObjects.Phase;
using DDDNetCore.Domain.OperationTypes.ValueObjects.RequiredStaff;
using DDDNetCore.Domain.OperationTypesRecords;
using DDDNetCore.Domain.Shared;
using DDDNetCore.Infrastructure.OperationTypeRecords;
using Moq;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Xunit;

namespace MDBackofficeTests.servicetests.operationtype
{
    public class OperationTypeServiceTests
    {
        private readonly Mock<IUnitOfWork> _unitOfWorkMock = new();
        private readonly Mock<LogService> _logServiceMock = new(new Mock<IUnitOfWork>().Object, new Mock<ILogRepository>().Object);
        private readonly Mock<IOperationTypeRepository> _repoMock = new Mock<IOperationTypeRepository>();
        private readonly Mock<OperationTypeRecordService> _opRecordService;
        private readonly OperationTypeService _service;

        public OperationTypeServiceTests()
        {
            _opRecordService = new Mock<OperationTypeRecordService>(_unitOfWorkMock.Object, _logServiceMock.Object, new Mock<IOperationTypeRecordRepository>().Object);

            _service = new OperationTypeService(_unitOfWorkMock.Object, _repoMock.Object,_logServiceMock.Object,_opRecordService.Object);
        }

        [Fact]
        public async Task FilterOperationTypes_ReturnsCorrectOperationTypeDtos()
        {
            // Arrange
            var queryParameters = new OperationTypeQueryParametersDto
            {
                queryFilters = new List<OperationTypeListingFilterParametersDto>
                {
                    new OperationTypeListingFilterParametersDto
                    {
                        Name = "test type 1",
                        Specialization = "ortho",
                        Status = "Active"
                    }
                }
            };

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

            var operationTypes = new List<Mock<OperationType>>
            {
                new Mock<OperationType>("test type 1", 100, true, reqStaffDto,phasesDto),
            };
       

            var expectedDtos = new List<OperationTypeDto>
            {
                new OperationTypeDto{Name ="test type 1",EstimatedDuration = 100, Status = true,RequiredStaff =reqStaffDto,Phases = phasesDto }
            };

            var operationTypeObjects = operationTypes.Select(mock => mock.Object).ToList();
            _repoMock.Setup(repo => repo.FilterOperationTypes(queryParameters))
                    .ReturnsAsync(operationTypeObjects);
           
            // Act
            var result = await _service.FilterOperationTypes(queryParameters);
  
            // Assert
            Assert.Equal(expectedDtos.Count, result.Count);
            for (int i = 0; i < expectedDtos.Count; i++)
            {
                Assert.Equal(expectedDtos[i].Name, result[i].Name);
                Assert.Equal(expectedDtos[i].EstimatedDuration, result[i].EstimatedDuration);
                Assert.Equal(expectedDtos[i].Status, result[i].Status);                

            }
            _repoMock.Verify(repo => repo.FilterOperationTypes(queryParameters), Times.Once);
        }


        [Fact]
        public async Task InactivateAsync_ReturnsOperationTypeDTO()
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

