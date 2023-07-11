-module(top_app).
%%% ========================================================= %%%
-behavior(application).
%%% ========================================================= %%%
-export([start/2]).
-export([start_phase/3]).
-export([stop/1]).
%%% ========================================================= %%%
%%% =================== API Functions ======================= %%%
%%% ========================================================= %%%
-spec start(atom() | tuple(), list()) -> {ok, pid()}.
start(_StartType, _StartArgs) ->
    bsc_sup:start_link().

-spec start_phase(atom(), normal, []) -> no_return().
start_phase(StartPhase, StartType, Args) ->
    List = [StartPhase, StartType, Args],
    io:format("~p:~p(~p, ~p, ~p).~n", [?MODULE, ?FUNCTION_NAME|List]).

stop(_Data) ->
    ok.