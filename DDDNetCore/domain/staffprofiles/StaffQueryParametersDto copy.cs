using System.Collections.Generic;
using DDDNetCore.Domain.StaffProfiles;

namespace DDDNetCore.Domain.Shared
{
    public class StaffQueryParametersDto
    {

        public List<StaffListingFilterParametersDto> queryFilters {get; set;}

        public StaffQueryParametersDto(List<StaffListingFilterParametersDto> QueryFilters)
        {
            queryFilters = QueryFilters;
        }

    }
}
