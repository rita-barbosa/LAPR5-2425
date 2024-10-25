using System;
using DDDNetCore.Domain.Patients;
using DDDNetCore.Domain.Shared;

public class AppointmentHistory : Entity<AppointmentHistoryId> 
{
    public AppointmentHistoryStatus Status { get; set; }
    public AppointmentHistoryType Type { get; set; }
    public Date CreatedAt { get; set; }
    public MedicalRecordNumber PatientId { get; set; }

    public AppointmentHistory()
    {
    
    }

    public AppointmentHistory(string status, string type, MedicalRecordNumber patientId)
    { 
        Status = (AppointmentHistoryStatus)Enum.Parse(typeof(AppointmentHistoryStatus), status);
        Type = (AppointmentHistoryType)Enum.Parse(typeof(AppointmentHistoryType), type);
        CreatedAt = new Date(DateTime.Now.ToString());
        PatientId = patientId;
    }

    public void UpdateStatus(AppointmentHistoryStatus newStatus)
    {
        Status = newStatus;
    }

    public void UpdateType(AppointmentHistoryType newType)
    {
        Type = newType;
    }
}
