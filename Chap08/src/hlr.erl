-module(hlr).
%%% ========================================================= %%%
-export([new/0]).
-export([attach/1]).
-export([detach/0]).
-export([lookup_id/1]).
-export([lookup_ms/1]).
%%% ========================================================= %%%
-type msisdn() :: integer().
%%% ========================================================= %%%
%%% =================== API Functions ======================= %%%
%%% ========================================================= %%%
-spec new() -> ok.
new() ->
    ets:new(msisdn2pid, [public, named_table]),
    ets:new(pid2msisdn, [public, named_table]),
    ok.

-spec attach(msisdn()) -> no_return().
attach(MSISDN) ->
    ets:insert(msisdn2pid, {MSISDN, self()}),
    ets:insert(pid2msisdn, {self(), MSISDN}).

-spec detach() -> no_return().
detach() ->
    case ets:lookup(pid2msisdn, self()) of
        [{Pid, MSISDN}] ->
            ets:delete(pid2msisdn, Pid),
            ets:delete(msisdn2pid, MSISDN);
        [] ->
            ok
    end.

-spec lookup_id(msisdn()) -> {error, invalid} | {ok, pid()}.
lookup_id(MSISDN) ->
    case ets:lookup(msisdn2pid, MSISDN) of
        [] -> {error, invalid};
        [{MSISDN, Pid}] -> {ok, Pid}
    end.

-spec lookup_ms(pid()) -> {error, invalid} | {ok, msisdn()}.
lookup_ms(Pid) ->
    case ets:lookup(pid2msisdn, Pid) of
        [] -> {error, invalid};
        [{Pid, MSISDN}] -> {ok, MSISDN}
    end.
