-module(coffee).
-export([start_link/0]).

-export([tea/0]).
-export([espresso/0]).
-export([americano/0]).
-export([cappuccino/0]).

-export([cup_removed/0]).
-export([pay/1]).
-export([cancel/0]).

-export([init/0]).

%%% API Functions
start_link() ->
    {ok, spawn_link(?MODULE, init, [])}.

%% Drink Selections
tea()        -> ?MODULE ! {selection, ?FUNCTION_NAME, 100}.
espresso()   -> ?MODULE ! {selection, ?FUNCTION_NAME, 150}.
americano()  -> ?MODULE ! {selection, ?FUNCTION_NAME, 100}.
cappuccino() -> ?MODULE ! {selection, ?FUNCTION_NAME, 150}.

%% Client Actions
cup_removed() -> ?MODULE ! ?FUNCTION_NAME.
pay(Coin)     -> ?MODULE ! {pay, Coin}.
cancel()      -> ?MODULE ! cancel.

%%% Callback Functions
init() ->
    register(?MODULE, self()),
    hw:reboot(),
    hw:display("Make Your Selection", []),
    selection().

%%% States
selection() ->
    receive
        {selection, Type, Price} ->
            hw:display("Please pay: ~w", [Price]),
            payment(Type, Price, 0);
        {pay, Coin} ->
            hw:return_change(Coin),
            selection();
        _Other ->
            selection()
    end.

payment(Type, Price, Paid) ->
    receive
        {pay, Coin} ->
            if
                Coin + Paid >= Price ->
                    hw:display("Preparing Drink.", []),
                    hw:return_change(Coin+Paid - Price),
                    hw:drop_cup(), hw:prepare(Type),
                    hw:display("Remove Drink.", []),
                    remove();
                true ->
                    ToPay = Price - (Coin+Paid),
                    hw:display("Please pay: ~w", [ToPay]),
                    payment(Type, Price, Coin+Paid)
            end;
        cancel ->
            hw:display("Make Your Selection", []),
            hw:return_change(Paid),
            selection();
        _Other ->
            payment(Type, Price, Paid)
    end.

remove() ->
    receive
        cup_removed ->
            hw:display("Make Your Selection", []),
            selection();
        {pay, Coin} ->
            hw:return_change(Coin),
            remove();
        _Other ->
            remove()
    end.

