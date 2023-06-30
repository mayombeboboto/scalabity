-module(freq_overload).
%%%==============================================================%%%
-export([start_link/0]).
-export([no_frequency/0]).
-export([frequency_available/0]).
-export([frequency_denied/0]).

-export([add/2]).
-export([delete/2]).
%%%==============================================================%%%
%%%======================== API Functions =======================%%%
%%%==============================================================%%%
start_link() ->
    case gen_event:start_link({local, ?MODULE}) of
        {ok, Pid} ->
            add(counter, {}),
            add(loggers, {file, "logs/info.log"}),
            {ok, Pid};
        Error ->
            Error
    end.

no_frequency() ->
    gen_event:notify(?MODULE, {set_alarm, {no_frequency, self()}}).

frequency_available() ->
    gen_event:notify(?MODULE, {clear_alarm, no_frequency}).

frequency_denied() ->
    gen_event:notify(?MODULE, {event, {frequency_denied, self()}}).

add(Module, Args) ->
    gen_event:add_sup_handler(?MODULE, Module, Args).

delete(Module, Args) ->
    gen_event:delete_handler(?MODULE, Module, Args).