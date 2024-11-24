using System;
using System.Collections.Generic;
using MDBackoffice.Domain.Shared;

namespace MDBackoffice.Domain.Rooms
{
    public class Room : Entity<RoomNumber>, IAggregateRoot
    {
        public RoomType Type { get; private set; }

        public Capacity Capacity { get; private set; }

        public List<Equipment> AvailableEquipment { get; private set; }

        public CurrentStatus CurrentStatus { get; private set; }

        public List<Slot> MaintenanceSlots { get; private set; }

        public Room()
        {
            AvailableEquipment = new List<Equipment>();
            MaintenanceSlots = new List<Slot>();
        }


        public Room(RoomNumber roomNumber, RoomType roomType, Capacity capacity, List<Equipment> equipment, CurrentStatus currentStatus, List<Slot> slots)
        {
            this.Id = roomNumber;
            Type = roomType;
            Capacity = capacity;
            AvailableEquipment = equipment;
            CurrentStatus = currentStatus;
            MaintenanceSlots = slots;
        }

        public Room(string roomNumber, string roomType, int capacity, string status)
        {
            this.Id = new RoomNumber(roomNumber);
            this.Type = new RoomType(roomType);
            this.Capacity = new Capacity(capacity);
            AvailableEquipment = new List<Equipment>();
            this.CurrentStatus = CurrentStatus.GetStatusByDescription(status);
            MaintenanceSlots = new List<Slot>();
        }

        public void AddEquipment(string equipment){
            AvailableEquipment.Add(new Equipment(equipment));
        }

        public void AddSlot(string startTime, string endTime, string startDate, string endDate = null)
        {
            var slot = new Slot(startTime, endTime, startDate, endDate);
            MaintenanceSlots.Add(slot);
        }

        public void ChangeSlots(List<SlotsDto> slots)
        {
             MaintenanceSlots.Clear();
            foreach (SlotsDto slotDto in slots)
            {
                this.MaintenanceSlots.Add(new Slot(slotDto.StartTime, slotDto.EndTime, slotDto.StartDate, slotDto.EndDate));
            }
        }

        public RoomDto ToDto()
        {
            return new RoomDto(
                roomNumber: this.Id.Value,
                type: this.Type.RoomTypeName,
                capacity: Capacity.CapcityNumber,
                availableEquipment: AvailableEquipment?.ConvertAll(equipment => equipment.EquipmentName),
                currentStatus: CurrentStatus.Description,
                maintenanceSlots: MaintenanceSlots?.ConvertAll(slot => new SlotsDto(
                    startTime: slot.TimeInterval.Start.ToString(),
                    endTime: slot.TimeInterval.End.ToString(),
                    startDate: slot.Date.Start.ToString(),
                    endDate: slot.Date.End.ToString()
                ))
            );
        }
    }
}