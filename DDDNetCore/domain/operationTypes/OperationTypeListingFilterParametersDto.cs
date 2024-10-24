
using System.Collections.Generic;
using DDDNetCore.Domain.Shared;

namespace DDDNetCore.Domain.OperationTypes
{
    public class OperationTypeListingFilterParametersDto
    {
        internal IEnumerable<OperationTypeListingFilterParametersDto> queryFilters;

        public string Name { get; set; }
        public string Specialization { get; set;}
        public string Status { get; set;}
    }
}