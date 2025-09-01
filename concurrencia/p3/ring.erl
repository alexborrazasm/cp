-module(ring).

-export([start/1, send/3, stop/1]).

-export([loop/2]).

% Waiting for messages
loop(Pid, Pos) ->
    receive
        % The first created process waits for the pid of the last one
        {update, NewPid} when Pid =:= none ->
            loop(NewPid, Pos);
        {message, Msg, N} ->
            io:format("~p received message ~p with ~p left~n", [Pos, Msg, N-1]),
            case N of
                1 -> % You are the last node N - 1 = 0
                    ok;
                _ ->
                    Pid ! {message, Msg, N-1}
            end,
            loop(Pid, Pos);
        stop -> 
            io:format("Process ~p stopping~n", [Pos]),
            Pid ! {stop, self()};
        {stop, First} ->
            io:format("Process ~p stopping~n", [Pos]),
            if % Avoid sending msg to nothing
                Pid =/= First ->
                    Pid ! {stop, First};
                true ->
                    ok
            end
    end.

% Create the ring recursively
start_ring(0, LastPid, FirstPid) ->
    FirstPid ! {update, LastPid},
    LastPid;
start_ring(N, LastPid, FirstPid) ->
    Pid = spawn(?MODULE, loop, [LastPid, N-1]),
    start_ring(N-1, Pid, FirstPid).

% Start a ring of N processes
start(N) when is_integer(N), N > 0 ->
    FirstPid = spawn(?MODULE, loop, [none, N-1]),
    start_ring(N-1, FirstPid, FirstPid);
start(_) ->
    {error, "N must be a positive integer"}.

% Send a message to the first process in the line
send(Pid, N, Msg) when is_integer(N), N > 0 ->
    Pid ! {message, Msg, N},
    ok;
send(_, 0, _) ->
    ok;
send(_, _, _) ->
    {error, "Invalid parameters"}.

% Stop all processes in the line
stop(Pid) ->
    Pid ! stop,
    ok.
