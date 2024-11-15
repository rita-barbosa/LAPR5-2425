namespace MDBackoffice.Domain.OperationRequests
{
    public class ListOperationRequestDto(string name, string operationType, string status)
    {
        public string PatientName { get; set; } = name;

        public string OperationType { get; set; } = operationType;

        public string Status { get; set; } = status;
    }
}