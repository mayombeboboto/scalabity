-module(frequency_sup).
%%% ========================================================= %%%
-behavior(supervisor).
%%% ========================================================= %%%
-export([start_link/0]).
-export([stop/0]).

-export([init/1]).
%%% ========================================================= %%%
%%% =================== API Functions ======================= %%%
%%% ========================================================= %%%
-spec start_link() -> {ok, pid()}.
start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec stop() -> no_return().
stop() -> exit(whereis(?MODULE), shutdown).

init([]) ->
    ChildSpecList = [child(freq_overload), child(frequency)],
    {ok, {{rest_for_one, 2, 3600}, ChildSpecList}}.

child(Module) ->
    {Module, {Module, start_link, []},
     permanent, 2000, worker, [Module]}.