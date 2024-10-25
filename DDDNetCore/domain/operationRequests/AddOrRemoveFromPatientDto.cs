namespace DDDNetCore.Domain.OperationRequest
{
    public class AddOrRemoveFromPatientDto(string patId, string opRequestId)
    {
        public string PatientId { get; set; } = patId;

        public string OperationRequestId { get; set; } = opRequestId;

    }
}