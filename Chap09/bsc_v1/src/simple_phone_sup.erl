-module(simple_phone_sup).
%%% ========================================================= %%%

%%% ========================================================= %%%
-export([start_link/0]).
-export([attach_phone/1]).
-export([detach_phone/1]).

-export([init/1]).

%%% ========================================================= %%%
-type msisdn() :: integer().
%%% ========================================================= %%%
%%% =================== API Functions ======================= %%%
%%% ========================================================= %%%
-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec attach_phone(msisdn()) -> {ok, pid()} | {error, term()}.
attach_phone(MSISDN) ->
    case hlr:lookup_id(MSISDN) of
        {ok, _Pid} ->
            {error, attached};
        _NotAttached ->
            supervisor:start_child(?MODULE, [MSISDN])
    end.

-spec detach_phone(msisdn()) -> {error, term()} | ok.
detach_phone(MSISDN) ->
    case hlr:lookup_id(MSISDN) of
        {ok, Pid} -> supervisor:terminate_child(?MODULE, Pid);
        _NotAttached -> {error, detached}
    end.

%%% ========================================================= %%%
%%% ================= Callback Functions ==================== %%%
%%% ========================================================= %%%
init([]) ->
    hlr:new(),
    SupFlag = #{ strategy => simple_one_for_one,
                 intensity => 10,
                 period => 3600 },
    ChildSpec = #{ id => msidsn,
                   start => {phone_fsm, start_link, []},
                   restart => transient,
                   shutdown => 2000,
                   type => worker,
                   modules => [phone_fsm] },
    {ok, {SupFlag, [ChildSpec]}}.

