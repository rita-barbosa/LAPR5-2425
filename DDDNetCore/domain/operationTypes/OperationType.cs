using System.Collections.Generic;
using DDDNetCore.Domain.OperationTypes.ValueObjects;
using DDDNetCore.Domain.OperationTypes.ValueObjects.Phase;
using DDDNetCore.Domain.OperationTypes.ValueObjects.RequiredStaff;
using DDDNetCore.Domain.Shared;

namespace DDDNetCore.Domain.OperationTypes
{
    public class OperationType : Entity<OperationTypeId>, IAggregateRoot
    {

        public EstimatedDuration EstimatedDuration { get;  private set; }
        public OperationTypeStatus Status { get;  private set; }
        public OperationTypeName Name { get;  private set; }
        public List<RequiredStaff> RequiredStaff { get;  private set; }

        private static readonly int NUMBER_OF_OPERATION_PHASES = 3;

        public List<Phase> Phases { get;  private set; } 

        public OperationType(){
            //for ORM
        }


        public OperationType(string name, int duration, bool status, List<RequiredStaffDto> staff, List<PhaseDto> phases)
        {
            this.Id = new OperationTypeId(name);
            this.EstimatedDuration = new EstimatedDuration(duration);
            this.Status = new OperationTypeStatus(status);
            this.Name = new OperationTypeName(name);

            foreach (RequiredStaffDto person in staff){
                RequiredStaff.Add(new RequiredStaff(person.StaffQuantity, person.Function, person.Specialization));
            }

            if(phases.Count != NUMBER_OF_OPERATION_PHASES){
                throw new BusinessRuleValidationException("An operation type must have" + NUMBER_OF_OPERATION_PHASES + "phases.");
            }

            foreach (PhaseDto stage in phases){
                Phases.Add(new Phase(stage.Description, stage.Duration));
            }
        }

        public void ChangeName(string name)
        {
            if (!this.Status.Active)
                throw new BusinessRuleValidationException("It is not possible to change the name to an inactive Operation Type.");
            this.Name = new OperationTypeName(name);
        }

        public void ChangeEstimatedDuration(int duration)
        {
            if (!this.Status.Active)
                throw new BusinessRuleValidationException("It is not possible to change the estimated duration to an inactive Operation Type.");
            this.EstimatedDuration = new EstimatedDuration(duration);
        }

        public void ChangeStatus(bool status)
        {
            this.Status = new OperationTypeStatus(status);
        }

        public void ChangeRequiredStaff(List<RequiredStaffDto> newStaff)
        {
            if (!this.Status.Active)
                throw new BusinessRuleValidationException("It is not possible to change the required staff for an inactive Operation Type.");

            foreach (RequiredStaffDto staffDto in newStaff)
            {
                var existingStaff = this.RequiredStaff.Find(rs => rs.SpecializationId.Value == staffDto.Specialization);

                if (existingStaff != null)
                {
                    existingStaff.ChangeStaffQuantity(staffDto.StaffQuantity);
                    existingStaff.ChangeFunction(staffDto.Function);
                    existingStaff.ChangeSpecialization(staffDto.Specialization);
                }
                else
                {
                    this.RequiredStaff.Add(new RequiredStaff(staffDto.StaffQuantity, staffDto.Function, staffDto.Specialization));
                }
            }
        }
        public void ChangePhases(List<PhaseDto> newPhases)
        {
            if (!this.Status.Active)
                throw new BusinessRuleValidationException("It is not possible to change the phases for an inactive Operation Type.");

            if (newPhases.Count != NUMBER_OF_OPERATION_PHASES)
            {
                throw new BusinessRuleValidationException("An operation type must have " + NUMBER_OF_OPERATION_PHASES + " phases.");
            }

            this.Phases = [];

            foreach (PhaseDto phaseDto in newPhases)
            {
                this.Phases.Add(new Phase(phaseDto.Description, phaseDto.Duration));
            }
        }


    }
}