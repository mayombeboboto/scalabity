-module(phone_sup).
%%% ========================================================= %%%
-behavior(supervisor).
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

-spec attach_phone(msisdn()) -> no_return().
attach_phone(MSISDN) ->
    case hlr:lookup_id(MSISDN) of
        {ok, _Pid} ->
            {error, attached};
        _Other ->
            ChildSpec = child_spec(MSISDN),
            supervisor:start_child(?MODULE, ChildSpec)
    end.

-spec detach_phone(msisdn()) -> no_return().
detach_phone(MSISDN) ->
    case hlr:lookup_id(MSISDN) of
        {ok, _Pid} ->
            supervisor:terminae_child(?MODULE, MSISDN),
            supervisor:delete_child(?MODULE, MSISDN);
        _Other ->
            {error, detached}
    end.

%%% ========================================================= %%%
init([]) ->
    SupFlags = #{ strategy => one_for_one,
                  intensity => 10,
                  period => 3600 },
    {ok, {SupFlags, []}}.

%%% ========================================================= %%%
child_spec(MSISDN) ->
    #{ id => MSISDN,
       start => {phone_fsm, start_link, [MSISDN]},
       restart => transient,
       shutdown => 2000,
       type => worker,
       modules => [phone_fsm] }.