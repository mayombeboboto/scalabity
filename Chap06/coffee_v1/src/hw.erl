-module(hw).
-export([return_change/1]).
-export([display/2]).
-export([prepare/1]).
-export([drop_cup/0]).
-export([reboot/0]).

return_change(Payment) -> io:format("Machine: Returned: ~w in change~n", [Payment]).
display(String, Arg)   -> io:format("Display: " ++ String ++ "~n", Arg).
prepare(Type)          -> io:format("Machine: Preparing ~p.~n", [Type]).
drop_cup()             -> io:format("Machine: Dropped cup.~n").
reboot()               -> io:format("Machine: Rebooted Hardware~n").