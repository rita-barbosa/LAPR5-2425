% DATA

:- dynamic agenda_operation_room/3.
:- dynamic room_in_scheduling/1.

surgery(so2,45,60,45).
surgery(so3,45,90,45).
surgery(so4,45,75,45).

surgery_id(so100001,so2).
surgery_id(so100002,so3).
surgery_id(so100003,so3).
surgery_id(so100004,so3).
surgery_id(so100005,so2).
surgery_id(so100006,so3).
surgery_id(so100007,so3).
surgery_id(so100008,so3).

room_id(r101).
room_id(r102).
room_id(r301).
room_id(r404).

%agenda_operation_room(r101,20241028,[(770, 950, so100003), (1080, 1110, so100004)]).
%agenda_operation_room(r102,20241028,[(720, 780, so100002), (1080, 1110, so100004)]).
%agenda_operation_room(r301,20241028,[(720, 780, so100002), (1080, 1110, so100004)]).
%agenda_operation_room(r404,20241028,[(720, 780, so100002), (1080, 1110, so100004)]).


% ------------------------------------------------------------------------
% ROUND-ROBIN ROOM DISTRIBUTION ALGORITHM
distribute_rooms(Day, ScheduledRoomsAgenda) :-
    findall(OpCode, surgery_id(OpCode, _), OperationsList),
    !,
    distribute_operations_round_robin(Day, OperationsList, _, [], ScheduledRoomsAgenda).

% Base case: no more operations to schedule
distribute_operations_round_robin(_, [], _, ScheduledRoomsAgenda, ScheduledRoomsAgenda) :-
    write('All operations have been successfully scheduled.'), nl.

% Restart from the beginning of the room list if exhausted
distribute_operations_round_robin(Day, Operations, [], CurrentSchedules, FinalSchedules) :-
    findall(Room, room_id(Room), RoomsList),
    distribute_operations_round_robin(Day, Operations, RoomsList, CurrentSchedules, FinalSchedules).


distribute_operations_round_robin(Day, [Operation | RemainingOps], [Room | RemainingRooms], CurrentSchedules, FinalSchedules) :-
    (   check_current_room_occupancy_ratio(Room, Day, Ratio),
        Ratio < 0.8,
        write('Room and Operation Combo: '), write(Room), write('-'), write(Operation), nl,
        retract(room_in_scheduling(_)),
        assert(room_in_scheduling(Room)),
        schedule_operation_in_room(Day, Operation, Room, CurrentSchedules, UpdatedSchedules)
    ->  % Successfully scheduled the operation
        RemainingScheduledOps = RemainingOps
    ;   % Skip this room if the ratio is too high or scheduling fails
        write('Room\'s Ratio is >= 0.8 or scheduling failed, skipping...\n'),
        RemainingScheduledOps = [Operation | RemainingOps]
    ),
    % Continue with the next room (or restart if list is empty)
    distribute_operations_round_robin(Day, RemainingScheduledOps, RemainingRooms, UpdatedSchedules, FinalSchedules).

% ROOM OCCUPANCY CHECK
check_current_room_occupancy_ratio(Room, Day, Ratio) :-
    (  retract(agenda_operation_room(Room, Day, Agenda)) -> true ; Agenda = [] ),
    get_total_time_sum_in_intervals(Agenda, 0, TotalTimeOccupied),
    free_agenda0(Agenda, FreeIntervals),
    get_total_time_sum_in_intervals(FreeIntervals, 0, TotalFreeTime),
    TotalFreeTime > 0, % Prevent division by zero
    Ratio is TotalTimeOccupied / TotalFreeTime,
    write('Room ID: '), write(Room), nl,
    write('TotalTimeOccupied: '), write(TotalTimeOccupied), nl,
    write('TotalFreeTime: '), write(TotalFreeTime), nl,
    write('Value of the Room\'s Ratio: '), format('~2f', [Ratio]), nl, nl,
    assert(agenda_operation_room(Room, Day, Agenda)).

check_current_room_occupancy_ratio(Room, _, 0) :-
    nl, write('Room ID: '), write(Room), nl,
    write('The room does not have a schedule for the day'), nl.

% TIME CALCULATIONS
get_total_time_sum_in_intervals([], Acc, Acc).

get_total_time_sum_in_intervals([(Start, End, _) | Agenda], Acc, TotalTimeOccupied) :-
    integer(Start), integer(End),
    SlotTime is End - Start,
    UpdatedAcc is Acc + SlotTime,
    get_total_time_sum_in_intervals(Agenda, UpdatedAcc, TotalTimeOccupied).

get_total_time_sum_in_intervals([(Start, End) | FreeIntervals], Acc, TotalTime) :-
    integer(Start), integer(End),
    SlotTime is End - Start,
    UpdatedAcc is Acc + SlotTime,
    get_total_time_sum_in_intervals(FreeIntervals, UpdatedAcc, TotalTime).

% OPERATION SCHEDULING WITH UPDATED SCHEDULES
schedule_operation_in_room(Day, Operation, Room, CurrentSchedules, UpdatedSchedules) :-
    surgery_id(Operation, OpType),
    surgery(OpType, PrepTime, Duration, CleanupTime),
    OpTotalTime is PrepTime + Duration + CleanupTime,
    (   retract(agenda_operation_room(Room, Day, CurrentAgenda)) -> true ; CurrentAgenda = [] ),
    free_agenda0(CurrentAgenda, FreeIntervals),
    filter_full_intervals1(FreeIntervals, OpTotalTime, SuitableIntervals),
    SuitableIntervals = [(StartTime, _) | _],
    NewStartTime is StartTime,
    NewEndTime is NewStartTime + OpTotalTime,
    NewAgenda = [(NewStartTime, NewEndTime, Operation) | CurrentAgenda],
    write('NewAgenda: '), write(NewAgenda), nl,
    % Update the schedules with the new agenda
    update_schedule(Room, Day, NewAgenda, CurrentSchedules, UpdatedSchedules),
    write('UpdatedSchedules: '), write(UpdatedSchedules), nl, nl.

% FREE INTERVALS
free_agenda0([], [(0, 1440)]).
free_agenda0([(0, Tfin, _) | LT], LT1) :- !, free_agenda1([(0, Tfin, _) | LT], LT1).
free_agenda0([(Tin, Tfin, _) | LT], [(0, T1) | LT1]) :-
    T1 is Tin - 1,
    free_agenda1([(Tin, Tfin, _) | LT], LT1).

free_agenda1([(_, Tfin, _)], [(T1, 1440)]) :- Tfin \== 1440, !, T1 is Tfin + 1.
free_agenda1([(_, _, _)], []).
free_agenda1([(_, T, _), (T1, Tfin2, _) | LT], LT1) :-
    Tx is T + 1, T1 == Tx, !,
    free_agenda1([(T1, Tfin2, _) | LT], LT1).
free_agenda1([(_, Tfin1, _), (Tin2, Tfin2, _) | LT], [(T1, T2) | LT1]) :-
    T1 is Tfin1 + 1, T2 is Tin2 - 1,
    free_agenda1([(Tin2, Tfin2, _) | LT], LT1).

% FILTER SUITABLE INTERVALS
filter_full_intervals1([], _, []).
filter_full_intervals1([(Start, End) | Rest], OpTotalTime, [(Start, End) | SuitableIntervals]) :-
    End - Start >= OpTotalTime,
    filter_full_intervals1(Rest, OpTotalTime, SuitableIntervals).
filter_full_intervals1([(Start, End) | Rest], OpTotalTime, SuitableIntervals) :-
    End - Start < OpTotalTime,
    filter_full_intervals1(Rest, OpTotalTime, SuitableIntervals).


% UPDATE ROOM SCHEDULE
update_schedule(Room, Day, NewAgenda, CurrentSchedules, UpdatedSchedules) :-
    nl, write('CurrentSchedules: '), write(CurrentSchedules), nl,
    ( select((Room, Day, ExistingAgenda), CurrentSchedules, TempSchedules) ->
        append(ExistingAgenda, NewAgenda, MergedAgenda),
        list_to_set(MergedAgenda, FinalAgenda),
        assert(agenda_operation_room(Room, Day, FinalAgenda)),
        UpdatedSchedules = [(Room, Day, FinalAgenda) | TempSchedules]
    ;   assert(agenda_operation_room(Room, Day, NewAgenda)),
        UpdatedSchedules = [(Room, Day, NewAgenda) | TempSchedules]
    ).
