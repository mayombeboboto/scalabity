-module(phone).
%%% ========================================================= %%%
-export([start_test/2]).
-export([reply/3]).
%%% ========================================================= %%%
start_test(Num, Calls) ->
    frequency:start_link(),
    [phone_fsm:start_link(Value) || Value <- lists:seq(1, Num)],
    call(Calls, Num).

reply(outbound, _ToFSMPid, Ms) ->
    clear(),
    FromFSMPid = self(),
    Msg1 = "~p dialing ~p~n",
    io:format(Msg1,[FromFSMPid, _ToFSMPid]),
    F = fun() ->
		rand:seed(exsss),
		timer:sleep(rand:uniform(3000)),
        Msg2 = "~p hanging up ~p~n",
		io:format(Msg2, [FromFSMPid, Ms]),
		phone_fsm:action(hangup, FromFSMPid)
	end,
    put(pid, spawn(F));

reply(connected, OtherMsId, _Ms) ->
    clear(),
    FromFSMPid = self(),
    Msg1 = "~p connected to ~p~n",
    io:format(Msg1, [FromFSMPid, OtherMsId]),
    F = fun() ->
		rand:seed(exsss),
		timer:sleep(rand:uniform(3000)),
        Msg2 = "~p hanging up ~p~n",
		io:format(Msg2, [FromFSMPid, OtherMsId]),
		phone_fsm:action(hangup, FromFSMPid)
	end,
    put(pid, spawn(F));
reply(invalid, _ToMs, _Ms) ->
    Msg1 = "~p connecting to ~p failed:invalid number~n",
    io:format(Msg1, [_ToMs, _Ms]),
    clear();
reply(inbound, _FromFSMPid, _Ms) ->
    clear(),
    ToFSMPid = self(),
    F = fun() ->
		rand:seed(exsss),
		timer:sleep(rand:uniform(1500)),
		case rand:uniform(2) of
		    1 ->
                Msg2 = "accept(~p,~p)~n",
                io:format(Msg2,[ToFSMPid, _FromFSMPid]),
                phone_fsm:action(accept, ToFSMPid),
                timer:sleep(rand:uniform(3000)),
                phone_fsm:action(hangup, ToFSMPid);
		    2 ->
			    phone_fsm:action(reject, ToFSMPid)
		end
	end,
    put(pid, spawn(F));
reply(hangup, _FromFSMPid, _Ms) ->
    clear();
reply(_Reason, FromFSMPid, _Ms) ->
    Msg1 = "~p connecting to ~p failed:~w~n",
    io:format(Msg1, [element(2,hlr:lookup_ms(FromFSMPid)), _Ms, _Reason]),
    clear().

call(0, _Num) -> ok;
call(Calls, Num) ->
    %% timer:sleep(100),
    FromMs = rand:uniform(Num),
    ToMs = rand:uniform(Num),
    {ok, FromFSMPid} = hlr:lookup_id(FromMs),

    case phone_fsm:action({outbound, ToMs}, FromFSMPid) of
        ok -> call(Calls-1, Num);
        _Error -> call(Calls, Num)
    end.

clear() ->
    case get(pid) of
        undefined ->
            ok;
        Pid ->
            exit(Pid, kill), erase(pid),
            io:format("~p cleared~n",[self()])
    end.
