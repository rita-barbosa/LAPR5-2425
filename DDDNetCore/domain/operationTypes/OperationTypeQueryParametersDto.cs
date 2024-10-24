
using System.Collections.Generic;

namespace DDDNetCore.Domain.OperationTypes
{
    public class OperationTypeQueryParametersDto
    {
        public required List<OperationTypeListingFilterParametersDto> queryFilters { get; set;}
    }
}