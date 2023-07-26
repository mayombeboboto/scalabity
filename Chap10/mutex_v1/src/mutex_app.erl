%%%-------------------------------------------------------------------
%% @doc mutex public API
%% @end
%%%-------------------------------------------------------------------

-module(mutex_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    mutex_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
