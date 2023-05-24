-module(loggers).
-behavior(gen_event).
-export([init/1]).
-export([handle_call/2]).
-export([handle_event/2]).
-export([handle_info/2]).
-export([terminate/2]).

init(standard_io) ->
    {ok, {standard_io, 1}};
init({file, File}) ->
    {ok, FilePid} = file:open(File, write),
    {ok, {FilePid, 1}};
init(Args) ->
    {error, {args, Args}}.

handle_call(_Event, {IO, Count}) ->
    {ok, ok, {IO, Count+1}}.

handle_event(Event, {IO, Count}) ->
    print(IO, Count, Event, "Event"),
    {ok, {IO, Count+1}}.

handle_info(Event, {IO, Count}) ->
    print(IO, Count, Event, "Unknown"),
    {ok, {IO, Count+1}}.

terminate(_Reason, {standard_io, Count}) ->
    {count, Count};
terminate(_Reason, {FilePid, Count}) ->
    file:close(FilePid),
    {count, Count}.

print(IO, Count, Event, Tag) ->
    Format = "ID: ~w Time: ~w Date: ~w~n" ++ Tag ++ " :~w~n~n",
    io:format(IO, Format, [Count, time(), date(), Event]).