using System;
using DDDNetCore.Domain.OperationRequest;
using Xunit;

namespace DDDNetCore.Tests.Domain.OperationRequest
{
    public class OperationRequestTests
    {
        private const string TestCode = "OP123";
        private const string TestDeadLineDate = "2024-12-31";
        private const string TestPriority = "High";
        private const string TestDateOfRequest = "2024-10-25";
        private readonly StaffId TestStaffId = new StaffId("N202400001"); // Assuming a valid format
        private const string TestDescription = "Test operation request";
        private readonly MedicalRecordNumber TestPatientId = new MedicalRecordNumber("MRN12345");
        private readonly OperationTypeId TestOperationTypeId = new OperationTypeId("OT123");

        [Fact]
        public void Constructor_WithAllParameters_ShouldSetProperties()
        {
            // Arrange
            var operationRequest = new OperationRequest(TestCode, TestDeadLineDate, TestPriority, TestDateOfRequest, TestStaffId, TestDescription, TestPatientId, TestOperationTypeId);

            // Assert
            Assert.Equal(TestCode, operationRequest.Id.Value);
            Assert.Equal(TestDeadLineDate, operationRequest.DeadLineDate.Value);
            Assert.Equal(TestPriority, operationRequest.Priority.Name);
            Assert.Equal(TestDateOfRequest, operationRequest.DateOfRequest.Value);
            Assert.Equal(OperationRequestStatus.Requested, operationRequest.Status);
            Assert.Equal(TestStaffId.Value, operationRequest.StaffId.Value);
            Assert.Equal(TestDescription, operationRequest.Description.Value);
            Assert.Equal(TestPatientId.Value, operationRequest.PatientId.Value);
            Assert.Equal(TestOperationTypeId.Value, operationRequest.OperationTypeId.Value);
        }

        [Fact]
        public void ChangeDeadLineDate_ShouldUpdateDeadLineDate()
        {
            // Arrange
            var operationRequest = new OperationRequest(TestDeadLineDate, Priority.GetPriorityByName(TestPriority), new Date(TestDateOfRequest), TestStaffId, TestDescription, TestPatientId, TestOperationTypeId);

            // Act
            operationRequest.ChangeDeadLineDate("2025-01-01");

            // Assert
            Assert.Equal("2025-01-01", operationRequest.DeadLineDate.Value);
        }

        [Fact]
        public void ChangePriority_ShouldUpdatePriority()
        {
            // Arrange
            var operationRequest = new OperationRequest(TestDeadLineDate, Priority.GetPriorityByName(TestPriority), new Date(TestDateOfRequest), TestStaffId, TestDescription, TestPatientId, TestOperationTypeId);

            // Act
            operationRequest.ChangePriority("Medium");

            // Assert
            Assert.Equal("Medium", operationRequest.Priority.Name);
        }

        [Fact]
        public void ChangeDateOfRequest_ShouldUpdateDateOfRequest()
        {
            // Arrange
            var operationRequest = new OperationRequest(TestDeadLineDate, Priority.GetPriorityByName(TestPriority), new Date(TestDateOfRequest), TestStaffId, TestDescription, TestPatientId, TestOperationTypeId);

            // Act
            operationRequest.ChangeDateOfRequest("2024-10-26");

            // Assert
            Assert.Equal("2024-10-26", operationRequest.DateOfRequest.Value);
        }

        [Fact]
        public void ChangeStatus_ShouldUpdateStatus()
        {
            // Arrange
            var operationRequest = new OperationRequest(TestDeadLineDate, Priority.GetPriorityByName(TestPriority), new Date(TestDateOfRequest), TestStaffId, TestDescription, TestPatientId, TestOperationTypeId);

            // Act
            operationRequest.ChangeStatus("Approved");

            // Assert
            Assert.Equal("Approved", operationRequest.Status.Description);
        }

        [Fact]
        public void ChangeDescription_ShouldUpdateDescription()
        {
            // Arrange
            var operationRequest = new OperationRequest(TestDeadLineDate, Priority.GetPriorityByName(TestPriority), new Date(TestDateOfRequest), TestStaffId, TestDescription, TestPatientId, TestOperationTypeId);

            // Act
            operationRequest.ChangeDescription("Updated operation request description");

            // Assert
            Assert.Equal("Updated operation request description", operationRequest.Description.Value);
        }
    }
}
