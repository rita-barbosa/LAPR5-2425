using DDDNetCore.Domain.StaffProfiles;
using DDDNetCore.Domain.Shared;
using Xunit;

namespace MDBackofficeTests.domaintests.staff;

public class StaffTests
{
    [Theory]
    [InlineData("00001", "Portugal, 4570-860, Rua das Oliveiras", "12345", "Rita", "Barbosa", "Rita Barbosa", "ritabarbosa@email.com", "+351", "910000000", "Doctor", "Orthopedics")]
    [InlineData("00002", "Portugal, 4440-440, Rua das Azeitonas", "12346", "Ana", "Guterres", "Ana Algo Guterres", "anaguterres@example.com", "+44", "91000000111", "Nurse", "Anesthesia")]
    [InlineData("00003", "Portugal, 5550-220, Rua das Amoreiras", "56784", "José", "Santos", "José Santos", "josesantos@example.com", "+2", "24510000001", "Intern", "Instrumenting")]
    [InlineData("00004", "Portugal, 3780-452, Rua das Aguias", "93734", "Matilde", "Varela", "Matilde Varela", "matildevarela@example.com", "+351", "7840567001", "Assistant", "Surgical-Assistance")]
    public void WhenPassingCorrectData_ThenStaffInstantiated(string seqNumber, string address, string licenseNumber, string firstName, string lastName, string fullName, string email, string countryCode, string phoneNumber, string function, string specializationId)
    {
        new Staff(
            seqNumber,
            address,
            licenseNumber,
            firstName,
            lastName,
            fullName,
            email,
            countryCode,
            phoneNumber,
            function,
            specializationId
        );
    }

    [Theory]
    [InlineData("00001", "", "12345", "Rita", "Barbosa", "Rita Barbosa", "ritabarbosa@email.com", "+351", "910000000", "Doctor", "Orthopedics")]
    [InlineData("00001", null, "12345", "Rita", "Barbosa", "Rita Barbosa", "ritabarbosa@email.com", "+351", "910000000", "Doctor", "Orthopedics")]
    [InlineData("00002", "Portugal, 4440-440", "12346", "Ana", "Guterres", "Ana Algo Guterres", "anaguterres@example.com", "+44", "91000000111", "Nurse", "Anesthesia")]
    [InlineData("00002", "Portugal, Rua das Azeitonas", "12346", "Ana", "Guterres", "Ana Guterres", "anaguterres@example.com", "+44", "91000000111", "Nurse", "Anesthesia")]
    [InlineData("00003", "5550-220, Rua das Amoreiras", "56784", "José", "Santos", "José Santos", "josesantos@example.com", "+2", "24510000001", "Intern", "Instrumenting")]
    [InlineData("00003", " , 5550-220, Rua das Amoreiras", "56784", "José", "Santos", "José Santos", "josesantos@example.com", "+2", "24510000001", "Intern", "Instrumenting")]
    [InlineData("00004", "Portugal, 3780-452, ", "93734", "Matilde", "Varela", "Matilde Varela", "matildevarela@example.com", "+351", "7840567001", "Assistant", "Surgical-Assistance")]
    [InlineData("00004", "Portugal,  , Rua das Amoreiras", "93734", "Matilde", "Varela", "Matilde Varela", "matildevarela@example.com", "+351", "7840567001", "Assistant", "Surgical-Assistance")]
    public void WhenPassingInvalidAddress_ThenThrowsException(string seqNumber, string address, string licenseNumber, string firstName, string lastName, string fullName, string email, string countryCode, string phoneNumber, string function, string specializationId)
    {
        Assert.Throws<BusinessRuleValidationException>(() =>
            new Staff(
                seqNumber,
                address,
                licenseNumber,
                firstName,
                lastName,
                fullName,
                email,
                countryCode,
                phoneNumber,
                function,
                specializationId
            )
        );
    }
    
    [Theory]
    [InlineData("00001", "Portugal, 4570-860, Rua das Oliveiras", "12345", "", "Barbosa", "Rita Barbosa", "ritabarbosa@email.com", "+351", "910000000", "Doctor", "Orthopedics")]
    [InlineData("00001", "Portugal, 4570-860, Rua das Oliveiras", "12345", null, "Barbosa", "Rita Barbosa", "ritabarbosa@email.com", "+351", "910000000", "Doctor", "Orthopedics")]
    [InlineData("00002", "Portugal, 4440-440, Rua das Azeitonas", "12346", "Ana", "", "Ana Guterres", "anaguterres@example.com", "+44", "91000000111", "Nurse", "Anesthesia")]
    [InlineData("00002", "Portugal, 4440-440, Rua das Azeitonas", "12346", "Ana", null, "Ana Guterres", "anaguterres@example.com", "+44", "91000000111", "Nurse", "Anesthesia")]
    [InlineData("00003", "Portugal, 5550-220, Rua das Amoreiras", "56784", "José", "Santos", "", "josesantos@example.com", "+2", "24510000001", "Intern", "Instrumenting")]
    [InlineData("00003", "Portugal, 5550-220, Rua das Amoreiras", "56784", "José", "Santos", null, "josesantos@example.com", "+2", "24510000001", "Intern", "Instrumenting")]
    [InlineData("00004", "Portugal, 3780-452, Rua das Aguias", "93734", "", "Varela", "", "matildevarela@example.com", "+351", "7840567001", "Assistant", "Surgical-Assistance")]
    [InlineData("00004", "Portugal, 3780-452, Rua das Aguias", "93734", null, "", "Matilde Varela", "matildevarela@example.com", "+351", "7840567001", "Assistant", "Surgical-Assistance")]
    [InlineData("00004", "Portugal, 3780-452, Rua das Aguias", "93734", "", "Varela", null, "matildevarela@example.com", "+351", "7840567001", "Assistant", "Surgical-Assistance")]
    public void WhenPassingInvalidName_ThenThrowsException(string seqNumber, string address, string licenseNumber, string firstName, string lastName, string fullName, string email, string countryCode, string phoneNumber, string function, string specializationId)
    {
        Assert.Throws<BusinessRuleValidationException>(() =>
            new Staff(
                seqNumber,
                address,
                licenseNumber,
                firstName,
                lastName,
                fullName,
                email,
                countryCode,
                phoneNumber,
                function,
                specializationId
            )
        );
    }


    [Theory]
    [InlineData("00001", "Portugal, 4570-860, Rua das Oliveiras", "12345", "Rita", "Barbosa", "Rita Barbosa", "ritabarbosa@email.com", "", "910000000", "Doctor", "Orthopedics")]
    [InlineData("00001", "Portugal, 4570-860, Rua das Oliveiras", "12345", "Rita", "Barbosa", "Rita Barbosa", "ritabarbosa@email.com", null, "910000000", "Doctor", "Orthopedics")]
    [InlineData("00002", "Portugal, 4440-440, Rua das Azeitonas", "12346", "Ana", "Guterres", "Ana Guterres", "anaguterres@example.com", "+44", "", "Nurse", "Anesthesia")]
    [InlineData("00002", "Portugal, 4440-440, Rua das Azeitonas", "12346", "Ana", "Guterres", "Ana Guterres", "anaguterres@example.com", "+44", null, "Nurse", "Anesthesia")]
    [InlineData("00003", "Portugal, 5550-220, Rua das Amoreiras", "56784", "José", "Santos", "José Santos", "josesantos@example.com", "2", "24510000001", "Intern", "Instrumenting")]
    [InlineData("00003", "Portugal, 5550-220, Rua das Amoreiras", "56784", "José", "Santos", "José Santos", "josesantos@example.com", "+2567854", "24510000001", "Intern", "Instrumenting")]
    [InlineData("00003", "Portugal, 5550-220, Rua das Amoreiras", "56784", "José", "Santos", "José Santos", "josesantos@example.com", "+2a3", "24510000001", "Intern", "Instrumenting")]
    [InlineData("00003", "Portugal, 5550-220, Rua das Amoreiras", "56784", "José", "Santos", "José Santos", "josesantos@example.com", "+bac", "24510000001", "Intern", "Instrumenting")]
    [InlineData("00004", "Portugal, 3780-452, Rua das Aguias", "93734", "Matilde", "Varela", "Matilde Varela", "matildevarela@example.com", "+351", "78405677799879001", "Assistant", "Surgical-Assistance")]
    [InlineData("00004", "Portugal, 3780-452, Rua das Aguias", "93734", "Matilde", "Varela", "Matilde Varela", "matildevarela@example.com", "+351", "784", "Assistant", "Surgical-Assistance")]
    [InlineData("00004", "Portugal, 3780-452, Rua das Aguias", "93734", "Matilde", "Varela", "Matilde Varela", "matildevarela@example.com", "+351", "7840Abd7001", "Assistant", "Surgical-Assistance")]
    public void WhenPassingInvalidPhone_ThenThrowsException(string seqNumber, string address, string licenseNumber, string firstName, string lastName, string fullName, string email, string countryCode, string phoneNumber, string function, string specializationId)
    {
        Assert.Throws<BusinessRuleValidationException>(() =>
            new Staff(
                seqNumber,
                address,
                licenseNumber,
                firstName,
                lastName,
                fullName,
                email,
                countryCode,
                phoneNumber,
                function,
                specializationId
            )
        );
    }

    [Theory]
    [InlineData("00001", "Portugal, 4570-860, Rua das Oliveiras", "12345", "Rita", "Barbosa", "Rita Barbosa", "", "+351", "910000000", "Doctor", "Orthopedics")]
    [InlineData("00001", "Portugal, 4570-860, Rua das Oliveiras", "12345", "Rita", "Barbosa", "Rita Barbosa", null, "+351", "910000000", "Doctor", "Orthopedics")]
    [InlineData("00002", "Portugal, 4440-440, Rua das Azeitonas", "12346", "Ana", "Guterres", "Ana Guterres", "anaguterresexample.com", "+44", "91000000111", "Nurse", "Anesthesia")]
    [InlineData("00002", "Portugal, 4440-440, Rua das Azeitonas", "12346", "Ana", "Guterres", "Ana Guterres", "anaguterres@examplecom", "+44", "91000000111", "Nurse", "Anesthesia")]
    [InlineData("00003", "Portugal, 5550-220, Rua das Amoreiras", "56784", "José", "Santos", "José Santos", "josesantos-example.com", "+2", "24510000001", "Intern", "Instrumenting")]
    [InlineData("00003", "Portugal, 5550-220, Rua das Amoreiras", "56784", "José", "Santos", "José Santos", "josesantos@example*com", "+2", "24510000001", "Intern", "Instrumenting")]
    [InlineData("00004", "Portugal, 3780-452, Rua das Aguias", "93734", "Matilde", "Varela", "Matilde Varela", "matildevarela+example=com", "+351", "7840567001", "Assistant", "Surgical-Assistance")]
    public void WhenPassingInvalidEmail_ThenThrowsException(string seqNumber, string address, string licenseNumber, string firstName, string lastName, string fullName, string email, string countryCode, string phoneNumber, string function, string specializationId)
    {
        Assert.Throws<BusinessRuleValidationException>(() =>
            new Staff(
                seqNumber,
                address,
                licenseNumber,
                firstName,
                lastName,
                fullName,
                email,
                countryCode,
                phoneNumber,
                function,
                specializationId
            )
        );
    }


    [Theory]
    [InlineData("00001", "Portugal, 4570-860, Rua das Oliveiras", "12345", "Rita", "Barbosa", "Rita Barbosa", "ritabarbosa@email.com", "+351", "910000000", "", "Orthopedics")]
    [InlineData("00001", "Portugal, 4570-860, Rua das Oliveiras", "12345", "Rita", "Barbosa", "Rita Barbosa", "ritabarbosa@email.com", "+351", "910000000", null, "Orthopedics")]
    [InlineData("00002", "Portugal, 4440-440, Rua das Azeitonas", "12346", "Ana", "Guterres", "Ana Guterres", "anaguterres@example.com", "+44", "91000000111", "something", "Anesthesia")]
    [InlineData("00003", "Portugal, 5550-220, Rua das Amoreiras", "56784", "José", "Santos", "José Santos", "josesantos@example.com", "+2", "24510000001", "a342", "Instrumenting")]
    [InlineData("00004", "Portugal, 3780-452, Rua das Aguias", "93734", "Matilde", "Varela", "Matilde Varela", "matildevarela@example.com", "+351", "7840567001", "D", "Surgical-Assistance")]
    public void WhenPassingInvalidFunction_ThenThrowsException(string seqNumber, string address, string licenseNumber, string firstName, string lastName, string fullName, string email, string countryCode, string phoneNumber, string function, string specializationId)
    {
         var exception = Assert.ThrowsAny<Exception>(() =>
            new Staff(
                seqNumber,
                address,
                licenseNumber,
                firstName,
                lastName,
                fullName,
                email,
                countryCode,
                phoneNumber,
                function,
                specializationId
            )
        );
        Assert.True(exception is BusinessRuleValidationException || exception is NullReferenceException,
            $"Expected exception of type BusinessRuleValidationException or NullReferenceException, but got {exception.GetType()}.");
    }

    [Theory]
    [InlineData("00001", "Portugal, 4570-860, Rua das Oliveiras", "12345", "Rita", "Barbosa", "Rita Barbosa", "ritabarbosa@email.com", "+351", "910000000", "Doctor", "")]
    [InlineData("00002", "Portugal, 4570-860, Rua das Oliveiras", "12346", "Ana", "Guterres", "Ana Guterres", "anaguterres@email.com", "+351", "910000000", "Doctor", null)]
    public void WhenPassingInvalidSpecialization_ThenThrowsException(string seqNumber, string address, string licenseNumber, string firstName, string lastName, string fullName, string email, string countryCode, string phoneNumber, string function, string specializationId)
    {
         var exception = Assert.ThrowsAny<Exception>(() =>
            new Staff(
                seqNumber,
                address,
                licenseNumber,
                firstName,
                lastName,
                fullName,
                email,
                countryCode,
                phoneNumber,
                function,
                specializationId
            )
        );
        Assert.True(exception is BusinessRuleValidationException || exception is NullReferenceException,
            $"Expected exception of type BusinessRuleValidationException or NullReferenceException, but got {exception.GetType()}.");
    }

}
