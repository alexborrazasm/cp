-module(line).

-export([start/1, send/2, stop/1]).

-export([loop/2]).

% Waiting for messages
loop(Pid, Pos) ->
    receive
        {message, Msg} ->
            io:format("~p received message ~p~n", [Pos, Msg]),
            case Pid of
                none ->
                    ok;
                _ ->
                    Pid ! {message, Msg}
            end,
            loop(Pid, Pos);
        stop -> 
            io:format("Process ~p stopping~n", [Pos]),
            case Pid of
                none ->
                    ok;
                _ ->
                    Pid ! stop
            end
    end.

% Create the line recursively
start_line(0, LastPid) ->
    LastPid;
start_line(N, LastPid) ->
    Pid = spawn(?MODULE, loop, [LastPid, N-1]),
    start_line(N-1, Pid).

% Start a line of N processes
start(N) when is_integer(N), N > 0 ->
    start_line(N, none);
start(_) ->
    {error, "N must be a positive integer"}.

% Send a message to the first process in the line
send(Pid, Msg) ->
    Pid ! {message, Msg},
    ok.

% Stop all processes in the line
stop(Pid) ->
    Pid ! stop,
    ok.
