using System.Collections.Generic;
using DDDNetCore.Domain.Patients;

namespace DDDNetCore.Domain.Shared
{
    public class PatientQueryParametersDto
    {

        public List<PatientListingFilterParametersDto> QueryFilters { get; set; }

        public PatientQueryParametersDto(List<PatientListingFilterParametersDto> _queryFilters)
        {
            QueryFilters = _queryFilters;  
        }

    }
}
