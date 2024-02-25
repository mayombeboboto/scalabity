%%%-------------------------------------------------------------------
%% @doc mutex top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(mutex_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

@spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{ strategy => one_for_one,
                  intensity => 5,
                  period => 3600},
    {ok, {SupFlags, []}}.

%% internal functions
