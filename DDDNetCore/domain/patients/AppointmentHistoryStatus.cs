using System.ComponentModel;

public enum AppointmentHistoryStatus
{
    [Description("Planned")]
    Planned = 1,
    [Description("Scheduled")]    
    Scheduled = 2,    
    [Description("Completed")]
    Completed = 3,   

}
