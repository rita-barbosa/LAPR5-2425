using MDBackoffice.Domain.Logs;
using MDBackoffice.Domain.OperationTypes.ValueObjects.Phase;
using MDBackoffice.Domain.OperationTypes.ValueObjects.RequiredStaff;
using MDBackoffice.Domain.OperationTypes;
using MDBackoffice.Domain.OperationTypesRecords;
using MDBackoffice.Domain.Shared;
using Moq;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Xunit;
using MDBackoffice.Controllers;
using Microsoft.AspNetCore.Mvc;

namespace MDBackofficeTests.controllertests
{
    public class OperationTypeControllerTests
    {
        private readonly Mock<IUnitOfWork> _unitOfWorkMock = new();
        private readonly Mock<LogService> _logServiceMock = new(new Mock<IUnitOfWork>().Object, new Mock<ILogRepository>().Object);
        private readonly Mock<IOperationTypeRepository> _repoMock = new Mock<IOperationTypeRepository>();
        private readonly Mock<OperationTypeRecordService> _opRecordService;
        private readonly Mock<OperationTypeService> _service;
        private readonly OperationTypesController _controller;

        public OperationTypeControllerTests()
        {
            _opRecordService = new Mock<OperationTypeRecordService>(_unitOfWorkMock.Object, _logServiceMock.Object, new Mock<IOperationTypeRecordRepository>().Object);

            _service = new Mock<OperationTypeService>(_unitOfWorkMock.Object, _repoMock.Object, _logServiceMock.Object, _opRecordService.Object);

            _controller = new OperationTypesController(_service.Object);
        }

        [Fact]
        public async Task GetFilteredOperationTypes_ReturnsOkOperationTypeDtos()
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
            var result = await _controller.GetFilteredOperationTypes(queryParameters);

            // Assert
            var okResult = Assert.IsType<OkObjectResult>(result.Result);
            var actualDtos = Assert.IsType<List<OperationTypeDto>>(okResult.Value);
            Assert.Equal(expectedDtos.Count, actualDtos.Count);

            for (int i = 0; i < expectedDtos.Count; i++)
            {
                Assert.Equal(expectedDtos[i].Name, actualDtos[i].Name);
                Assert.Equal(expectedDtos[i].EstimatedDuration, actualDtos[i].EstimatedDuration);
                Assert.Equal(expectedDtos[i].Status, actualDtos[i].Status);
            }

            _repoMock.Verify(repo => repo.FilterOperationTypes(queryParameters), Times.Once);
        }


        [Fact]
        public async Task RemoveOperationType_ValidId_ReturnsOkResult()
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


    }
}
