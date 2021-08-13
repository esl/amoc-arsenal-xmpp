%% Minimalistic module for performing time-ordered actions in load tests without an event loop

-module(timetable).

-export([new/3, do/3, do/4, merge/1]).

-type time_diff() :: non_neg_integer(). %% milliseconds
-type timetable(Event) :: [{time_diff(), Event}].
-type executor(Event) :: fun((escalus:client(), Event) -> any()).
-type executor(Event, State) :: fun((escalus:client(), Event, State) -> State).

-export_type([timetable/1]).

%% @doc Creates a new timetable with Event scheduled every Interval (ms)
%%      Start time is a random Offset where 0 =< Offset =< Interval
-spec new(Event, non_neg_integer(), time_diff()) -> timetable(Event).
new(Event, Count, Interval) ->
    Offset = rand:uniform(Interval + 1) - 1,
    [{Interval * I + Offset, Event} || I <- lists:seq(0, Count - 1)].

%% @doc Executes F for each {Time, Event} from TimeTable as soon as Time (ms) passed
%%      AND previous executions finished.
-spec do(escalus:client(), executor(Event), timetable(Event)) -> ok.
do(Client, F, TimeTable) ->
    do(Client, fun(_, Event, no_state) ->
                       F(Client, Event),
                       no_state
               end, TimeTable, no_state),
    ok.

%% @doc Like do/3, but passes State through the executions of F.
-spec do(escalus:client(), executor(Event, State), timetable(Event), State) -> State.
do(Client, F, TimeTable, State) ->
    step(Client, F, TimeTable, State, current_time()).

-spec step(escalus:client(), executor(Event, State), timetable(Event), State, integer()) -> State.
step(Client, F, [{Time, Event} | TimeTable], State, StartTime) ->
    Elapsed = current_time() - StartTime,
    TimeDiff = max(0, Time - Elapsed),
    escalus_connection:wait(Client, TimeDiff),
    NewState = F(Client, Event, State),
    step(Client, F, TimeTable, NewState, StartTime);
step(_Client, _F, [], State, _StartTime) ->
    State.

%% @doc Merges Tables preserving the time order
-spec merge([timetable(Event)]) -> timetable(Event).
merge(Tables) ->
    lists:merge(Tables).

current_time() ->
    erlang:system_time(millisecond).
