using System.Collections.Generic;
using DDDNetCore.Domain.Patients;

namespace DDDNetCore.Domain.Shared
{
    public class PatientQueryParametersDto
    {

        public required List<PatientListingFilterParametersDto> queryFilters {get; set;}

    }
}
