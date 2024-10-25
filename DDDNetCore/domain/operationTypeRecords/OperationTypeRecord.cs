using System;
using System.Collections.Generic;
using DDDNetCore.Domain.OperationTypes;
using DDDNetCore.Domain.OperationTypes.ValueObjects;
using DDDNetCore.Domain.OperationTypes.ValueObjects.Phase;
using DDDNetCore.Domain.OperationTypes.ValueObjects.RequiredStaff;
using DDDNetCore.Domain.Shared;


namespace DDDNetCore.Domain.OperationTypesRecords
{
    public class OperationTypeRecord : Entity<OperationTypeRecordId>, IAggregateRoot
    {

        public OperationTypeRecordVersion Version { get;}
        public Date EffectiveDate { get;}
        public OperationTypeId OperationTypeId { get;}
        public OperationTypeName Name { get;}
        public EstimatedDuration EstimatedDuration { get;}
        public OperationTypeStatus Status { get;}
        public List<RequiredStaffRecord> RequiredStaffRecords { get;}
        public List<PhaseRecord> Phases { get;}

        private static readonly int NUMBER_OF_OPERATION_PHASES = 3;

        public OperationTypeRecord()
        {
            //for ORM
        }


        public OperationTypeRecord(Date date, int previousVersionNumber, OperationTypeId operationTypeId, OperationTypeName name, EstimatedDuration duration, OperationTypeStatus status, List<RequiredStaff> staff, List<Phase> phases)
        {
            Id = new OperationTypeRecordId(new Guid().ToString());
            EffectiveDate = date;
            Version = new OperationTypeRecordVersion(previousVersionNumber + 1);
            OperationTypeId = operationTypeId;
            Name = name;
            EstimatedDuration = duration;
            Status = status;

            RequiredStaffRecords = new List<RequiredStaffRecord>();
            foreach (RequiredStaff requiredStaff in staff){
                RequiredStaffRecords.Add(new RequiredStaffRecord(requiredStaff.StaffQuantity.NumberRequired, requiredStaff.Function.Description, requiredStaff.SpecializationId.ToString()));
            }

            if (phases.Count != NUMBER_OF_OPERATION_PHASES)
            {
                throw new BusinessRuleValidationException("An operation type must have" + NUMBER_OF_OPERATION_PHASES + "phases.");
            }

            Phases = new List<PhaseRecord>();
            foreach (Phase stage in phases)
            {
                Phases.Add(new PhaseRecord(stage.Description.Description, stage.Duration.DurationMinutes));
            }
        }


    }
}