-module(counter).
-behavior(gen_event).

-export([get_counters/1]).

-export([init/1]).
-export([handle_call/2]).
-export([handle_event/2]).
-export([handle_info/2]).
-export([terminate/2]).

get_counters(Pid) ->
    gen_event:call(Pid, ?MODULE, get_counters).

init(_InitArgs) ->
    TableID = ets:new(?MODULE, []),
    {ok, TableID}.

handle_call(get_counters, TableID) ->
    {ok, {counters, ets:tab2list(TableID)}, TableID}.

handle_event(Event, TableID) ->
    try ets:update_counter(TableID, Event, 1) of
        _Value -> {ok, TableID}    
    catch
        error:_Error ->
            ets:insert(TableID, {Event, 1}),
            {ok, TableID}
    end.

handle_info(_Event, TableID) ->
   {ok, TableID}.

terminate(_Reason, TableID) ->
    Counters = ets:tab2list(TableID),
    ets:delete(TableID),
    {counters, Counters}.