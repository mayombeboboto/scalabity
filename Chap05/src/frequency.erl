-module(frequency).
%%%==============================================================%%%
-export([start_link/0]).
-export([stop/0]).

-export([allocate/0]).
-export([deallocate/1]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
%%%==============================================================%%%
-type freq() :: 10..15.

%%%==============================================================%%%
%%%======================== API Functions =======================%%%
%%%==============================================================%%%
-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec stop() -> no_return().
stop() -> gen_server:stop(?MODULE).

-spec allocate() -> {error, no_frequency} | {ok, freq()}.
allocate() ->
  gen_server:call(?MODULE, {allocate, self()}).

-spec deallocate(freq()) -> no_return().
deallocate(Frequency) ->
  gen_server:cast(?MODULE, {deallocate, Frequency}).

%%%==============================================================%%%
%%%===================== Callback Functions =====================%%%
%%%==============================================================%%%
init([]) ->
  process_flag(trap_exit, true),
  Frequencies = {lists:seq(10, 15), []},
  {ok, Frequencies}.

handle_call({allocate, Pid}, _From, Frequencies) ->
  {NewFrequencies, Reply} = allocate(Frequencies, Pid),
  {reply, Reply, NewFrequencies}.

handle_cast({deallocate, Frequency}, Frequencies) ->
  NewFrequencies = deallocate(Frequencies, Frequency),
  {noreply, NewFrequencies}.

handle_info({'EXIT', _Pid, normal}, Frequencies) ->
  {noreply, Frequencies};
handle_info({'EXIT', Pid, Reason}, Frequencies) ->
  io:format("Pid: ~p exited with reason: ~p~n", [Pid, Reason]),
  {noreply, Frequencies};
handle_info(_Msg, Frequencies) ->
  {noreply, Frequencies}.

terminate(_Reason, _Frequencies) ->
  ok.

%%%==============================================================%%%
%%%===================== Internal Functions =====================%%%
%%%==============================================================%%%
allocate({[], Allocated}, _Pid) ->
  {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
    {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, Freq) ->
    NewAllocated = lists:keydelete(Freq, 1, Allocated),
    {[Freq|Free], NewAllocated}.
