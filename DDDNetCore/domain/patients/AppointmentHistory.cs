using System;
using DDDNetCore.Domain.Patients;
using DDDNetCore.Domain.Shared;

public class AppointmentHistory
{
    public string Id { get; set; } 

    public AppointmentHistoryStatus Status { get; set; } 

    public AppointmentHistoryType Type { get; set; } 

    public Date CreatedAt { get; set; } 

    public MedicalRecordNumber PatientId { get; set; }

    public AppointmentHistory() 
    {
    }

    public AppointmentHistory(string objectId, int status, int type, MedicalRecordNumber patientId)
    {
        this.Id = objectId;
        this.Status = (AppointmentHistoryStatus)status;
        this.Type = (AppointmentHistoryType)type;
        this.CreatedAt = new Date(DateTime.Now.ToString());
        this.PatientId = patientId; 
    }
}
