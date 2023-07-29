%%%-------------------------------------------------------------------
%% @doc tcp_wrapper public API
%% @end
%%%-------------------------------------------------------------------

-module(tcp_wrapper_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    tcp_wrapper_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
