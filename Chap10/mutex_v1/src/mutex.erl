-module(mutex).
-export([start_link/1]).
-export([start_link/2]).
-export([stop/1]).

-export([wait/1]).
-export([signal/1]).

-export([init/3]).

%%% APIs
start_link(Name) ->
    start_link(Name, []).

start_link(Name, DbgOpts) ->
    proc_lib:start_link(?MODULE, init, [self(), Name, DbgOpts]).

stop(Name) -> Name ! stop.

wait(Name) ->
    Name ! {wait, self()},
    Mutex = whereis(Name),
    receive
        {Mutex, ok} -> ok
    end.

signal(Name) ->
    Name ! {signal, self()},
    ok.

%%% Callback
init(Parent, Name, DbgOpts) ->
    register(Name, self()),
    process_flag(trap_exit, true),
    Debug = sys:debug_options(DbgOpts),
    proc_lib:init_ack({ok, self()}),
    free(Name, Parent, Debug).

%%% Internal Functions
free(Name, Parent, Debug) ->
    receive
        {wait, Pid} ->
            Pid ! {self(), ok},
            busy(Pid, Name, Parent, Debug);
        stop ->
            ok
    end.

busy(Pid, Name, Parent, Debug) ->
    receive
        {signal, Pid} ->
            free(Name, Parent, Debug)
    end.