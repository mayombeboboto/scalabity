-module(bsc_sup).
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
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec stop() -> no_return().
stop() -> exit(whereis(?MODULE), shutdown).

%%% ========================================================= %%%
%%% ================= Callback Functions ==================== %%%
%%% ========================================================= %%%
init(_Args) ->
    ModTypes = [{freq_overload, worker},
                {frequency, worker},
                {simple_phone_sup, supervisor}],
    ChildSpecList = lists:map(fun child/1, ModTypes),
    SupFlag = #{ strategy => rest_for_one,
                 intensity => 2,
                 period => 3600 },
    {ok, {SupFlag, ChildSpecList}}.

%%% ========================================================= %%%
%%% ================= Internal Functions ==================== %%%
%%% ========================================================= %%%
child({Module, Type}) ->
    #{ id => Module,
       start => {Module, start_link, []},
       restart => permanent,
       shutdown => 2000,
       type => Type,
       modules => [Module] }.


