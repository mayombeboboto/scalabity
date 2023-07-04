-module(phone_fsm).
%%% ========================================================= %%%
-behaviour(gen_statem).
%%% ========================================================= %%%
-export([start_link/1]).
-export([stop/1]).
-export([action/2]).
-export([busy/1]).
-export([reject/1]).
-export([accept/1]).
-export([hangup/1]).
-export([inbound/1]).

-export([init/1]).
-export([callback_mode/0]).
-export([terminate/3]).

-export([idle/3]).
-export([calling/3]).
-export([receiving/3]).
-export([connecting/3]).
%%% ========================================================= %%%
-type fsm_pid() :: pid().
-type other_fsm_pid() :: pid().
-type msisdn() :: integer().
-type action() :: accept | reject | hangup |
                  {outbound, msisdn()}.
%%% ========================================================= %%%
%%% =================== API Functions ======================= %%%
%%% ========================================================= %%%
-spec start_link(msisdn()) -> {ok, fsm_pid()}.
start_link(MSISDN) ->
    gen_statem:start_link(?MODULE, MSISDN, []).

-spec stop(fsm_pid()) -> no_return().
stop(PhoneFSMPid) ->
    gen_statem:stop(PhoneFSMPid).

-spec action(action(), fsm_pid()) -> no_return().
action({outbound, OtherMSISDN}, FSMPid) ->
    gen_statem:call(FSMPid, {outbound, OtherMSISDN});
action(Action, FSMPid) ->
    gen_statem:cast(FSMPid, {action, Action}).

-spec busy(other_fsm_pid()) -> no_return().
busy(OtherFSMPid) ->
    gen_statem:cast(OtherFSMPid, {busy, self()}).

-spec reject(other_fsm_pid()) -> no_return().
reject(OtherFSMPid) ->
    gen_statem:cast(OtherFSMPid, {reject, self()}).

-spec accept(other_fsm_pid()) -> no_return().
accept(OtherFSMPid) ->
    gen_statem:cast(OtherFSMPid, {accept, self()}).

-spec hangup(other_fsm_pid()) -> no_return().
hangup(OtherFSMPid) ->
    gen_statem:cast(OtherFSMPid, {hangup, self()}).

-spec inbound(other_fsm_pid()) -> no_return().
inbound(OtherFSMPid) ->
    gen_statem:cast(OtherFSMPid, {inbound, self()}).

%%% ========================================================= %%%
%%% ================= Callback Functions ==================== %%%
%%% ========================================================= %%%
init(MSISDN) ->
    process_flag(trap_exit, true),
    hlr:attach(MSISDN),
    {ok, idle, MSISDN}.

callback_mode() -> state_functions.

terminate(_Reason, idle, _Data) ->
    hlr:detach();
terminate(_Reason, calling, {_MSISDN, OtherPID}) ->
    phone_fsm:hangup(OtherPID),
    hlr:detach();
terminate(_Reason, connecting, {_MSISDN, OtherPID, _Freq}) ->
    phone_fsm:hangup(OtherPID),
    hlr:detach();
terminate(_Reason, receiving, {_MSISDN, FromPID}) ->
    phone_fsm:reject(FromPID),
    hlr:detach().

%%% ========================================================= %%%
%%% =================== State Functions ===================== %%%
%%% ========================================================= %%%
idle(cast, {inbound, FromPID}, MSISDN) ->
    phone:reply(inbound, FromPID, MSISDN),
    {next_state, receiving, {MSISDN, FromPID}};
idle({call, From}, {outbound, OtherMSISDN}, MSISDN) ->
    case hlr:lookup_id(OtherMSISDN) of
        {error, invalid} ->
            io:format("Error, invalid~n"),
            phone:reply(invalid, OtherMSISDN, MSISDN),
            gen_statem:reply(From, {error, invalid}),
            {next_state, idle, MSISDN};
        {ok, OtherFSMPid} when is_pid(OtherFSMPid) ->
            phone:reply(outbound, OtherMSISDN, MSISDN),
            phone_fsm:inbound(OtherFSMPid),
            gen_statem:reply(From, ok),
            {next_state, calling, {MSISDN, OtherFSMPid}}
    end;
idle(_EventType, EventContent, Data) ->
    Msg = "~p in idle, ignored. State: ~w, Event: ~w~n",
    io:format(Msg, [self(), Data, EventContent]),
    {next_state, idle, Data}.

%%% ========================================================= %%%
calling(cast, {action, hangup}, {MSISDN, OtherFSMPid}) ->
    phone_fsm:hangup(OtherFSMPid),
    {next_state, idle, MSISDN};
calling(cast, {busy, OtherFSMPid}, {MSISDN, OtherFSMPid}) ->
    phone:reply(busy, OtherFSMPid, MSISDN),
    {next_state, idle, MSISDN};
calling(cast, {reject, OtherFSMPid}, {MSISDN, OtherFSMPid}) ->
    phone:reply(rejected, OtherFSMPid, OtherFSMPid),
    {next_state, idle, MSISDN};
calling(cast, {accept, OtherFSMPid}, {MSISDN, OtherFSMPid}) ->
    case frequency:allocate() of
        {error, no_frequency} ->
            phone_fsm:reject(OtherFSMPid),
            phone:reply(no_frequency, OtherFSMPid, MSISDN),
            {next_state, idle, MSISDN};
        {ok, Freq} ->
            phone:reply(connected, OtherFSMPid, MSISDN),
            {next_state, connecting, {MSISDN, OtherFSMPid, Freq}}
    end;
calling(cast, {inbound, OtherFSMPid}, _Data) ->
    phone_fsm:busy(OtherFSMPid),
    keep_state_and_data;
calling({call, From}, {outbound, _OtherMSISDN}, _MSISDN) ->
    gen_statem:reply(From, {error, busy}),
    keep_state_and_data;
calling(_EventType, EventContent, Data) ->
    Msg = "In calling, ignored. State: ~w, Event: ~w~n",
    io:format(Msg, [Data, EventContent]),
    keep_state_and_data.

%%% ========================================================= %%%
receiving(cast, {action, accept}, {MSISDN, OtherFSMPid}) ->
    phone_fsm:accept(OtherFSMPid),
    {next_state, connecting, {MSISDN, OtherFSMPid}};
receiving(cast, {action, reject}, {MSISDN, OtherFSMPid}) ->
    phone_fsm:reject(OtherFSMPid),
    {next_state, idle, MSISDN};
receiving(cast, {hangup, OtherFSMPid}, {MSISDN, OtherFSMPid}) ->
    phone:reply(hangup, OtherFSMPid, MSISDN),
    {next_state, idle, MSISDN};
receiving(cast, {inbound, OtherFSMPid}, _Data) ->  % Others
    phone_fsm:busy(OtherFSMPid),
    keep_state_and_data;
receiving({call, From}, {outbound, _OtherMSISDN}, _MSISDN) ->
    gen_statem:reply(From, {error, busy}),
    keep_state_and_data;
receiving(_EventType, EventContent, Data) ->  % {action, hangup}
    Msg = "In receiving, ignored. State:~w, Event:~w~n",
    io:format(Msg,[Data, EventContent]),
    keep_state_and_data.

%%% ========================================================= %%%
connecting(cast, {inbound, OtherFSMPid}, Data) ->
    phone_fsm:busy(OtherFSMPid),
    {next_state, connecting, Data};
connecting(cast, {action, hangup}, {MSISDN, OtherFSMPid, Freq}) -> %% We hang up, We initated call
    phone_fsm:hangup(OtherFSMPid),
    frequency:deallocate(Freq),
    {next_state, idle, MSISDN};
connecting(cast, {action, hangup}, {MSISDN, OtherFSMPid}) -> %% We hang up, Other initated call
    phone_fsm:hangup(OtherFSMPid),
    {next_state, idle, MSISDN};
connecting(cast, {hangup, OtherFSMPid}, {MSISDN, OtherFSMPid}) -> %% they hang Up
    phone:reply(hangup, OtherFSMPid, MSISDN),
    {next_state, idle, MSISDN};
connecting(cast, {hangup, OtherFSMPid}, {MSISDN, OtherFSMPid, Freq}) -> %% they hang Up
    phone:reply(hangup, OtherFSMPid, MSISDN),
    frequency:deallocate(Freq),
    {next_state, idle, MSISDN};
connecting({call, From}, {outbound, _OtherMSISDN}, _MSISDN) ->
    gen_statem:reply(From, {error, busy}),
    keep_state_and_data;
connecting(_EventType, EventContent, Data) ->
    Msg = "In connected, ignored. State:~w, Event:~w~n",
    io:format(Msg,[Data, EventContent]),
    keep_state_and_data.
