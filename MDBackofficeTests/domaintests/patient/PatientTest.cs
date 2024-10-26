using DDDNetCore.Domain.Patients;
using DDDNetCore.Domain.Shared;
using Xunit;

namespace MDBackofficeTests.domaintests.patient;

public class PatientTests
{

    [Theory]
    [InlineData("Rita", "Barbosa", "Rita Barbosa", "Portugal, 4590-850, Rua da Sardinha", "Female", "+351", "910000000", "9100087827", "ritabarbosa@email.com", "20/10/2004", "000001")]
    [InlineData("Ana", "Guterres", "Ana Guterres", "Portugal, 4580-856, Rua da Faneca", "FEMALE", "+351", "910000001", "9100087845", "anaguterres@example.com", "15/12/2004", "000002")]
    [InlineData("Ana", "Guterres", "Ana Guterres", "Portugal, 4580-856, Rua da Faneca", "fEMALE", "+351", "910000001", "9100087845", "anaguterres@example.com", "15/12/2004", "000005")]
    [InlineData("Test", "Second", "Test First Second", "Portugal, 4580-856, Rua do Salmão", "Male", "+351", "910000002", "9104987845", "test@example.com", "09/04/2006", "000003")]
    [InlineData("João", "Last", "João Last", "Portugal, 4580-856, Rua do Bacalhau", "MALE", "+351", "910000003", "91120871245", "lastTest@example.com", "15/12/2000", "000004")]
    [InlineData("João", "Last", "João Last", "Portugal, 4580-856, Rua do Bacalhau", "mALE", "+351", "910000003", "91120871245", "lastTest@example.com", "15/12/2000", "000004")]
    public void WhenPassingCorrectData_ThenPatientInstantiated(string firstName, string lastName, string fullName, string address, string gender, string countryCode, string phoneNumber, string emergencyContact, string email, string dateBirth, string seqNumber)
    {
        new Patient(
            firstName,
            lastName,
            fullName,
            address,
            gender,
            countryCode,
            phoneNumber,
            emergencyContact,
            email,
            dateBirth,
            seqNumber
        );
    }

    [Theory]
    [InlineData("Rita", "Barbosa", "Rita Barbosa", "Portugal, 4590-850, Rua da Sardinha", "Female", "+351", "910000000", "9100087827", "ritabarbosa@email.com", "20/10/2004", "001")]
    [InlineData("Ana", "Guterres", "Ana Guterres", "Portugal, 4580-856, Rua da Faneca", "Female", "+351", "910000001", "9100087845", "anaguterres@example.com", "15/12/2004", "11000002")]
    [InlineData("Test", "Second", "Test First Second", "Portugal, 4580-856, Rua do Salmão", "Male", "+351", "910000002", "9104987845", "test@example.com", "09/04/2006", "ABCdef")]
    [InlineData("João", "Last", "João Last", "Portugal, 4580-856, Rua do Bacalhau", "Male", "+351", "910000003", "91120871245", "lastTest@example.com", "15/12/2000", "GhijkLetip")]
    [InlineData("João", "Last", "João Last", "Portugal, 4580-856, Rua do Bacalhau", "Male", "+351", "910000003", "91120871245", "lastTest@example.com", "15/12/2000", "JkJ")]
    public void WhenPassingInvalidSeqNumber_ThenThrowsException(string firstName, string lastName, string fullName, string address, string gender, string countryCode, string phoneNumber, string emergencyContact, string email, string dateBirth, string seqNumber)
    {
        Assert.Throws<BusinessRuleValidationException>(() =>
            new Patient(
                firstName,
                lastName,
                fullName,
                address,
                gender,
                countryCode,
                phoneNumber,
                emergencyContact,
                email,
                dateBirth,
                seqNumber
            )
        );
    }

    [Theory]
    [InlineData("", "Barbosa", "Rita Barbosa", "Portugal, 4590-850, Rua da Sardinha", "Female", "+351", "910000000", "9100087827", "ritabarbosa@email.com", "20/10/2004", "000001")]
    [InlineData("Ana", null, "Ana Guterres", "Portugal, 4580-856, Rua da Faneca", "Female", "+351", "910000001", "9100087845", "anaguterres@example.com", "15/12/2004", "000002")]
    [InlineData(null, "Second", "null", "Portugal, 4580-856, Rua do Salmão", "Male", "+351", "910000002", "9104987845", "test@example.com", "09/04/2006", "000003")]
    [InlineData("João", "", "João Last", "Portugal, 4580-856, Rua do Bacalhau", "Male", "+351", "910000003", "91120871245", "lastTest@example.com", "15/12/2000", "000004")]
    [InlineData("João", "Last", "", "Portugal, 4580-856, Rua do Bacalhau", "Male", "+351", "910000003", "91120871245", "lastTest@example.com", "15/12/2000", "000004")]
    [InlineData("João", "Last", null, "Portugal, 4580-856, Rua do Bacalhau", "Male", "+351", "910000003", "91120871245", "lastTest@example.com", "15/12/2000", "000004")]
    public void WhenPassingInvalidName_ThenThrowsException(string firstName, string lastName, string fullName, string address, string gender, string countryCode, string phoneNumber, string emergencyContact, string email, string dateBirth, string seqNumber)
    {
        Assert.Throws<BusinessRuleValidationException>(() =>
            new Patient(
                firstName,
                lastName,
                fullName,
                address,
                gender,
                countryCode,
                phoneNumber,
                emergencyContact,
                email,
                dateBirth,
                seqNumber
            )
        );
    }

    [Theory]
    [InlineData("Rita", "Barbosa", "Rita Barbosa", "", "Female", "+351", "910000000", "9100087827", "ritabarbosa@email.com", "20/10/2004", "P123456")]
    [InlineData("Rita", "Barbosa", "Rita Barbosa", null, "Female", "+351", "910000000", "9100087827", "ritabarbosa@email.com", "20/10/2004", "P123456")]
    [InlineData("Ana", "Guterres", "Ana Guterres", "Portugal, 4580-856", "Female", "+351", "910000001", "9100087845", "anaguterres@example.com", "15/12/2004", "P654321")]
    [InlineData("Ana", "Guterres", "Ana Guterres", "Portugal, Rua do Salmão", "Female", "+351", "910000001", "9100087845", "anaguterres@example.com", "15/12/2004", "P654321")]
    [InlineData("Ana", "Guterres", "Ana Guterres", "4580-856, Rua do Salmão", "Female", "+351", "910000001", "9100087845", "anaguterres@example.com", "15/12/2004", "P654321")]
    [InlineData("Test", "Second", "Test First Second", " , 4580-856, Rua do Salmão", "Male", "+351", "910000002", "9104987845", "test@example.com", "09/04/2006", "P984321")]
    [InlineData("João", "Last", "João Last", "Portugal, 4580-856,  ", "Male", "+351", "910000003", "91120871245", "lastTest@example.com", "15/12/2000", "P444321")]
    [InlineData("João", "Last", "João Last", "Portugal,  , Rua do Salmão", "Male", "+351", "910000003", "91120871245", "lastTest@example.com", "15/12/2000", "P444321")]
    public void WhenPassingInvalidAddress_ThenThrowsException(string firstName, string lastName, string fullName, string address, string gender, string countryCode, string phoneNumber, string emergencyContact, string email, string dateBirth, string seqNumber)
    {
        Assert.Throws<BusinessRuleValidationException>(() =>
            new Patient(
                firstName,
                lastName,
                fullName,
                address,
                gender,
                countryCode,
                phoneNumber,
                emergencyContact,
                email,
                dateBirth,
                seqNumber
            )
        );
    }

    [Theory]
    [InlineData("Rita", "Barbosa", "Rita Barbosa", "Portugal, 4590-850, Rua da Sardinha", "Female", "", "910000000", "910000000", "ritabarbosa@email.com", "20/10/2004", "P123456")]
    [InlineData("Rita", "Barbosa", "Rita Barbosa", "Portugal, 4590-850, Rua da Sardinha", "Female", null, "910000000", "910000000", "ritabarbosa@email.com", "20/10/2004", "P123456")]
    [InlineData("Ana", "Guterres", "Ana Guterres", "Portugal, 4580-856, Rua da Faneca", "Female", "+351456", "910000001", "910000001", "anaguterres@example.com", "15/12/2004", "P654321")]
    [InlineData("Ana", "Guterres", "Ana Guterres", "Portugal, 4580-856, Rua da Faneca", "Female", "+35As", "910000001", "910000001", "anaguterres@example.com", "15/12/2004", "P654321")]
    [InlineData("Ana", "Guterres", "Ana Guterres", "Portugal, 4580-856, Rua da Faneca", "Female", "+351", "91000AB01", "910000001", "anaguterres@example.com", "15/12/2004", "P654321")]
    [InlineData("Ana", "Guterres", "Ana Guterres", "Portugal, 4580-856, Rua da Faneca", "Female", "51", "910000001", "910000001", "anaguterres@example.com", "15/12/2004", "P654321")]
    [InlineData("Ana", "Guterres", "Ana Guterres", "Portugal, 4580-856, Rua da Faneca", "Female", "+351", "123456789123456789", "910000001", "anaguterres@example.com", "15/12/2004", "P654321")]
    [InlineData("Test", "Second", "Test Second", "Portugal, 4580-856, Rua do Salmão", "Male", "+351", "9100000005", "", "test@example.com", "09/04/2006", "P984321")]
    [InlineData("Test", "Second", "Test Second", "Portugal, 4580-856, Rua do Salmão", "Male", "+351", "9100000005", null, "test@example.com", "09/04/2006", "P984321")]
    [InlineData("João", "Last", "João Last", "Portugal, 4580-856, Rua do Bacalhau", "Male", "+4444444444", "910000003", "91120871245", "lastTest@example.com", "15/12/2000", "P444321")]
    [InlineData("João", "Last", "João Last", "Portugal, 4580-856, Rua do Bacalhau", "Male", "+351", "910000003", "9112087124512345678", "lastTest@example.com", "15/12/2000", "P444321")]
    [InlineData("João", "Last", "João Last", "Portugal, 4580-856, Rua do Bacalhau", "Male", "+351", "910000003", "911EFs678", "lastTest@example.com", "15/12/2000", "P444321")]
    public void WhenPassingInvalidPhone_ThenThrowsException(string firstName, string lastName, string fullName, string address, string gender, string countryCode, string phoneNumber, string emergencyContact, string email, string dateBirth, string seqNumber)
    {
        Assert.Throws<BusinessRuleValidationException>(() =>
            new Patient(
                firstName,
                lastName,
                fullName,
                address,
                gender,
                countryCode,
                phoneNumber,
                emergencyContact,
                email,
                dateBirth,
                seqNumber
            )
        );
    }

    [Theory]
    [InlineData("Rita", "Barbosa", "Rita Barbosa", "Portugal, 4590-850, Rua da Sardinha", "Female", "+351", "910000000", "910000000", "ritabarbosa@email.com", "20/10/2004", "P123456")]
    [InlineData("Ana", "Guterres", "Ana Guterres", "Portugal, 4580-856, Rua da Sardinha", "Female", "+351", "910000001", "910000001", "anaguterres@example.com", "15/12/2004", "P654321")]
    [InlineData("Test", "Second", "Test First Second", "Portugal, 4580-856, Rua do Salmão", "Male", "+351", "9104987845", "9104987845", "test@example.com", "09/04/2006", "P984321")]
    [InlineData("João", "Last", "João Last", "Portugal, 4580-856, Rua do Bacalhau", "Male", "+351", "9112087124576", "9112087124576", "lastTest@example.com", "15/12/2000", "P444321")]
    public void WhenPhoneNumberAndEmergencyContactAreSame_ThenThrowsException(string firstName, string lastName, string fullName, string address, string gender, string countryCode, string phoneNumber, string emergencyContact, string email, string dateBirth, string seqNumber)
    {
        Assert.Throws<BusinessRuleValidationException>(() =>
            new Patient(
                firstName,
                lastName,
                fullName,
                address,
                gender,
                countryCode,
                phoneNumber,
                emergencyContact,
                email,
                dateBirth,
                seqNumber
            )
        );
    }

    [Theory]
    [InlineData("Rita", "Barbosa", "Rita Barbosa", "Portugal, 4590-850, Rua da Sardinha", "Female", "+351", "910000000", "910000000", "ritabarbosa@email.com", "40/10/2004", "P123456")]
    [InlineData("Rita", "Barbosa", "Rita Barbosa", "Portugal, 4590-850, Rua da Sardinha", "Female", "+351", "910000000", "910000000", "ritabarbosa@email.com", "20/15/2004", "P123456")]
    [InlineData("Rita", "Barbosa", "Rita Barbosa", "Portugal, 4590-850, Rua da Sardinha", "Female", "+351", "910000000", "910000000", "ritabarbosa@email.com", "data inválida", "P123456")]
    [InlineData("Rita", "Barbosa", "Rita Barbosa", "Portugal, 4590-850, Rua da Sardinha", "Female", "+351", "910000000", "910000000", "ritabarbosa@email.com", "dez de janeiro", "P123456")]
    [InlineData("Ana", "Guterres", "Ana Guterres", "Portugal, 4580-856, Rua da Sardinha", "Female", "+351", "910000001", "910000001", "anaguterres@example.com", "15-48-2004", "P654321")]
    [InlineData("Ana", "Guterres", "Ana Guterres", "Portugal, 4580-856, Rua da Sardinha", "Female", "+351", "910000001", "910000001", "anaguterres@example.com", "100-10-206", "P654321")]
    [InlineData("Ana", "Guterres", "Ana Guterres", "Portugal, 4580-856, Rua da Sardinha", "Female", "+351", "910000001", "910000001", "anaguterres@example.com", "16.12.2007", "P654321")]
    [InlineData("Ana", "Guterres", "Ana Guterres", "Portugal, 4580-856, Rua da Sardinha", "Female", "+351", "910000001", "910000001", "anaguterres@example.com", "15*11*2001", "P654321")]
    [InlineData("Test", "Second", "Test Second", "Portugal, 4580-856, Rua do Salmão", "Male", "+351", "910000002", "9104987845", "test@example.com", "09/04/2006 27:00", "P984321")]
    [InlineData("Test", "Second", "Test Second", "Portugal, 4580-856, Rua do Salmão", "Male", "+351", "910000002", "9104987845", "test@example.com", "09/04/2006 12:100", "P984321")]
    [InlineData("Test", "Second", "Test Second", "Portugal, 4580-856, Rua do Salmão", "Male", "+351", "910000002", "9104987845", "test@example.com", "09-04-2006 30:01", "P984321")]
    [InlineData("Test", "Second", "Test Second", "Portugal, 4580-856, Rua do Salmão", "Male", "+351", "910000002", "9104987845", "test@example.com", "09-04-2006 09:50:00", "P984321")]
    [InlineData("Test", "Second", "Test Second", "Portugal, 4580-856, Rua do Salmão", "Male", "+351", "910000002", "9104987845", "test@example.com", "09-04-2006 09:20:666", "P984321")]
    [InlineData("Test", "Second", "Test Second", "Portugal, 4580-856, Rua do Salmão", "Male", "+351", "910000002", "9104987845", "test@example.com", "09-04-2006 177:20:00", "P984321")]
    public void WhenPassingInvalidDateOfBirth_ThenThrowsException(string firstName, string lastName, string fullName, string address, string gender, string countryCode, string phoneNumber, string emergencyContact, string email, string dateBirth, string seqNumber)
    {
        Assert.Throws<BusinessRuleValidationException>(() =>
            new Patient(
                firstName,
                lastName,
                fullName,
                address,
                gender,
                countryCode,
                phoneNumber,
                emergencyContact,
                email,
                dateBirth,
                seqNumber
            )
        );
    }

    [Theory]
    [InlineData("Rita", "Barbosa", "Rita Barbosa", "Portugal, 4590-850, Rua da Sardinha", "Female", "+351", "910000000", "910000000", "", "20/10/2004", "P123456")]
    [InlineData("Rita", "Barbosa", "Rita Barbosa", "Portugal, 4590-850, Rua da Sardinha", "Female", "+351", "910000000", "910000000", null, "20/10/2004", "P123456")]
    [InlineData("Ana", "Guterres", "Ana Guterres", "Portugal, 4580-856, Rua da Sardinha", "Female", "+351", "910000001", "910000001", "anaguterresexample.com", "15/12/2004", "P654321")]
    [InlineData("Test", "Second", "Test First Second", "Portugal, 4580-856, Rua do Salmão", "Male", "+351", "910000002", "9104987845", "test@examplecom", "09/04/2006", "P984321")]
    [InlineData("João", "Last", "João Last", "Portugal, 4580-856, Rua do Bacalhau", "Male", "+351", "910000003", "91120871245", "lastTest@example.com", "15/12/2000", "P444321")]
    [InlineData("João", "Last", "João Last", "Portugal, 4580-856, Rua do Bacalhau", "Male", "+351", "910000003", "91120871245", "lastTest@example-com", "15/12/2000", "P444321")]
    [InlineData("João", "Last", "João Last", "Portugal, 4580-856, Rua do Bacalhau", "Male", "+351", "910000003", "91120871245", "lastTest*example.com", "15/12/2000", "P444321")]
    public void WhenPassingInvalidEmail_ThenThrowsException(string firstName, string lastName, string fullName, string address, string gender, string countryCode, string phoneNumber, string emergencyContact, string email, string dateBirth, string seqNumber)
    {
        Assert.Throws<BusinessRuleValidationException>(() =>
            new Patient(
                firstName,
                lastName,
                fullName,
                address,
                gender,
                countryCode,
                phoneNumber,
                emergencyContact,
                email,
                dateBirth,
                seqNumber
            )
        );
    }


     [Theory]
    [InlineData("Rita", "Barbosa", "Rita Barbosa", "Portugal, 4590-850, Rua da Sardinha", "", "+351", "910000000", "9100087827", "ritabarbosa@email.com", "20/10/2004", "000001")]
    [InlineData("Rita", "Barbosa", "Rita Barbosa", "Portugal, 4590-850, Rua da Sardinha", null, "+351", "910000000", "9100087827", "ritabarbosa@email.com", "20/10/2004", "000001")]
    [InlineData("Rita", "Barbosa", "Rita Barbosa", "Portugal, 4590-850, Rua da Sardinha", "F7male", "+351", "910000000", "9100087827", "ritabarbosa@email.com", "20/10/2004", "000001")]
    [InlineData("Ana", "Guterres", "Ana Guterres", "Portugal, 4580-856, Rua da Faneca", "Something", "+351", "910000001", "9100087845", "anaguterres@example.com", "15/12/2004", "000002")]
    [InlineData("Ana", "Guterres", "Ana Guterres", "Portugal, 4580-856, Rua da Faneca", "ALGO", "+351", "910000001", "9100087845", "anaguterres@example.com", "15/12/2004", "000002")]
    [InlineData("Ana", "Guterres", "Ana Guterres", "Portugal, 4580-856, Rua da Faneca", "FEMA5E", "+351", "910000001", "9100087845", "anaguterres@example.com", "15/12/2004", "000002")]
    [InlineData("Test", "Second", "Test First Second", "Portugal, 4580-856, Rua do Salmão", "M2LE", "+351", "910000002", "9104987845", "test@example.com", "09/04/2006", "000003")]
    [InlineData("Test", "Second", "Test First Second", "Portugal, 4580-856, Rua do Salmão", "M4le", "+351", "910000002", "9104987845", "test@example.com", "09/04/2006", "000003")]
    [InlineData("João", "Last", "João Last", "Portugal, 4580-856, Rua do Bacalhau", "M/le", "+351", "910000003", "91120871245", "lastTest@example.com", "15/12/2000", "000004")]
    [InlineData("João", "Last", "João Last", "Portugal, 4580-856, Rua do Bacalhau", "Mal*e", "+351", "910000003", "91120871245", "lastTest@example.com", "15/12/2000", "000004")]
    [InlineData("João", "Last", "João Last", "Portugal, 4580-856, Rua do Bacalhau", "Ma-le", "+351", "910000003", "91120871245", "lastTest@example.com", "15/12/2000", "000004")]
    public void WhenPassingInvalidGender_ThenThrowsException(string firstName, string lastName, string fullName, string address, string gender, string countryCode, string phoneNumber, string emergencyContact, string email, string dateBirth, string seqNumber)
    {
        Assert.Throws<BusinessRuleValidationException>(() =>
            new Patient(
                firstName,
                lastName,
                fullName,
                address,
                gender,
                countryCode,
                phoneNumber,
                emergencyContact,
                email,
                dateBirth,
                seqNumber
            )
        );
    }



}