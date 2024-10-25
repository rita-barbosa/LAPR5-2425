using System.Threading.Tasks;
using System.Collections.Generic;
using DDDNetCore.Domain.Shared;
using DDDNetCore.Domain.OperationTypes.ValueObjects.RequiredStaff;
using DDDNetCore.Domain.OperationTypes.ValueObjects.Phase;
using System;
using DDDNetCore.Domain.Logs;
using DDDNetCore.Domain.OperationTypesRecords;

namespace DDDNetCore.Domain.OperationTypes
{
    public class OperationTypeRecordService
    {

        private readonly IUnitOfWork _unitOfWork;
        private readonly IOperationTypeRecordRepository _repo;

        private readonly LogService _logService;

        public OperationTypeRecordService(IUnitOfWork unitOfWork, LogService logService, IOperationTypeRecordRepository operationTypeRecordRepository)
        {
            _unitOfWork = unitOfWork;
            _logService = logService;
            _repo = operationTypeRecordRepository;
        }

        public async Task<OperationTypeRecordDto> AddAsync(OperationType operationType)
        {
            var operationRecord = new OperationTypeRecord(new Date(DateTime.Now), 0, operationType.Id, operationType.Name, operationType.EstimatedDuration, operationType.Status, operationType.RequiredStaff, operationType.Phases);
            await _repo.AddAsync(operationRecord);
          //  await _unitOfWork.CommitAsync();

            await _logService.CreateCreationLog(operationRecord.Id.Value, operationRecord.GetType().Name, "New operation type record added.");

            List<RequiredStaffDto> requiredStaffDtos = [];
            foreach (RequiredStaffRecord requiredStaffRecord in operationRecord.RequiredStaffRecords){
                requiredStaffDtos.Add(new RequiredStaffDto{StaffQuantity =requiredStaffRecord.StaffQuantity.NumberRequired,
                 Function = requiredStaffRecord.Function.Description, Specialization = requiredStaffRecord.SpecializationId.ToString()});
            }

            List<PhaseDto> phaseDtos = [];
            foreach (PhaseRecord phase in operationRecord.Phases){
                phaseDtos.Add(new PhaseDto{Description = phase.Description.Description, Duration = phase.Duration.DurationMinutes});
            }

           return  new OperationTypeRecordDto(operationRecord.Id.ToString(), operationRecord.Version.Version,
             operationRecord.EffectiveDate.ToString(), operationRecord.OperationTypeId.ToString(),
              operationRecord.Name.OperationName, operationRecord.EstimatedDuration.TotalDurationMinutes,
               operationRecord.Status.Active, requiredStaffDtos, phaseDtos);
        }

    }
}
