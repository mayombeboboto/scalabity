-module(coffee_gen_statem).
-behavior(gen_statem).

-export([start_link/0]).
-export([stop/0]).

-export([tea/0]).
-export([espresso/0]).
-export([americano/0]).
-export([cappuccino/0]).

-export([pay/1]).
-export([cancel/0]).
-export([cup_removed/0]).

-export([init/1]).
-export([callback_mode/0]).
-export([terminate/2]).

-export([selection/3]).
-export([payment/3]).
-export([remove/3]).

start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_statem:call(?MODULE, stop).

tea()        -> gen_statem:cast(?MODULE, {selection, ?FUNCTION_NAME, 100}).
espresso()   -> gen_statem:cast(?MODULE, {selection, ?FUNCTION_NAME, 100}).
americano()  -> gen_statem:cast(?MODULE, {selection, ?FUNCTION_NAME, 150}).
cappuccino() -> gen_statem:cast(?MODULE, {selection, ?FUNCTION_NAME, 150}).

pay(Coin)     -> gen_statem:cast(?MODULE, {?FUNCTION_NAME, Coin}).
cancel()      -> gen_statem:cast(?MODULE, ?FUNCTION_NAME).
cup_removed() -> gen_statem:cast(?MODULE, ?FUNCTION_NAME).

init([]) ->
    hw:reboot(),
    hw:display("Make Your Selection", []),
    process_flag(trap_exit, true),
    {ok, selection, []}.

callback_mode() ->
    state_functions.

terminate(_Reason, _Data) ->
    ok.

selection(cast, {selection, Type, Price}, _Data) ->
    hw:display("Please pay: ~w", [Price]),
    {next_state, payment, {Type, Price, 0}};
selection(cast, {pay, Coin}, _Data) ->
    hw:return_change(Coin),
    keep_state_and_data;
selection(cast, _EventContent, _Data) ->
    keep_state_and_data;
selection({call, From}, stop, Data) ->
    {stop_and_reply, normal, [{reply, From, ok}], Data}.

payment(cast, {pay, Coin}, {Type, Price, Paid}) when Coin+Paid < Price ->
    NewPaid = Coin+Paid,
    hw:display("Please pay: ~w", [Price - NewPaid]),
    {keep_state, {Type, Price, NewPaid}};
payment(cast, {pay, Coin}, {Type, Price, Paid}) when Coin+Paid >= Price ->
    NewPaid = Coin+Paid,
    hw:display("Preparing Drink.", []),
    hw:return_change(NewPaid - Price),
    hw:drop_cup(), hw:prepare(Type),
    hw:display("Remove Drink.", []),
    {next_state, remove, null};
payment(cast, cancel, {_Type, _Price, Paid}) ->
    hw:display("Make Your Selection", []),
    hw:return_change(Paid),
    {next_state, selection, null};
payment(cast, _EventContent, _Data) ->
    keep_state_and_data;
payment({call, From}, stop, Data) ->
    {stop_and_reply, normal, [{reply, From, ok}], Data}.

remove(cast, cup_removed, Data) ->
    hw:display("Make Your Selection", []),
    {next_state, selection, Data};
remove(cast, {pay, Coin}, _Data) ->
    hw:return_change(Coin),
    keep_state_and_data;
remove({call, From}, stop, Data) ->
    {stop_and_reply, normal, [{reply, From, ok}], Data}.


