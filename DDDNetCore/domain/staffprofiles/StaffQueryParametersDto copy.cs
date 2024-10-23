using System.Collections.Generic;
using DDDNetCore.Domain.StaffProfiles;

namespace DDDNetCore.Domain.Shared
{
    public class StaffQueryParametersDto
    {

        public required List<StaffListingFilterParametersDto> queryFilters {get; set;}

    }
}
