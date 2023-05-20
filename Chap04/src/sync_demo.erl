-module(sync_demo).
%%%==============================================================%%%
-export([start_link/0]).

-export([first_request/0]).
-export([second_request/0]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).

%%%==============================================================%%%
%%%======================== API Functions =======================%%%
%%%==============================================================%%%
-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec first_request() -> no_return().
first_request() ->
  gen_server:call(?MODULE, {?FUNCTION_NAME, calendar:local_time()}).

-spec second_request() -> ok.
second_request() ->
  gen_server:call(?MODULE, ?FUNCTION_NAME).

%%%==============================================================%%%
%%%===================== Callback Functions =====================%%%
%%%==============================================================%%%
init([]) ->
  process_flag(trap_exit, true),
  {ok, []}.

handle_call({first_request, Datetime}, From, _State) ->
  {noreply, {From, Datetime}};
handle_call(second_request, _From, {From, Datetime}) ->
  gen_server:reply(From, Datetime),
  {reply, ok, []}.

handle_cast(_AsyncReq, State) ->
  {noreply, State}.

handle_info(_Msg, Frequencies) ->
  {noreply, Frequencies}.

%%%==============================================================%%%
%%%===================== Internal Functions =====================%%%
%%%==============================================================%%%
