%% Minimalistic module for performing time-ordered actions in load tests without an event loop

-module(timetable).

-export([new/3, do/3, do/4, merge/1]).

new(Event, Count, Interval) ->
    Offset = rand:uniform(Interval),
    [{Interval * I + Offset, Event} || I <- lists:seq(0, Count - 1)].

%% @doc Stateless execution
do(Client, F, TimeTable) ->
    do(Client, fun(_, Event, _) ->
                       F(Client, Event),
                       no_state
               end, TimeTable, no_state).

%% @doc Stateful execution
do(Client, F, TimeTable, State) ->
    step(Client, F, TimeTable, State, current_time()).

step(Client, F, [{Time, Event} | TimeTable], State, StartTime) ->
    Elapsed = current_time() - StartTime,
    TimeDiff = max(0, Time - Elapsed),
    escalus_connection:wait(Client, TimeDiff),
    NewState = F(Client, Event, State),
    step(Client, F, TimeTable, NewState, StartTime);
step(_Client, _F, [], _State, _StartTime) ->
    ok.

merge(Tables) ->
    lists:merge(Tables).

current_time() ->
    erlang:system_time(millisecond).
