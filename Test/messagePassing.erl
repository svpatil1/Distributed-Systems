-module (messagePassing).
-compile([export_all]).



serve() ->
    receive
        Request ->
            io:format("Handling: ~s~n", [Request]),
            serve()
    end.



math() ->
    receive
        {add, X, Y} ->
            io:format("~p + ~p = ~p~n", [X,Y,X+Y]),
            math();
        {sub, X, Y} ->
            io:format("~p - ~p = ~p~n", [X,Y,X-Y]),
            math();
        {variable, X} ->
            io:format("Variable is: ~p~n", [X]),
            math()
    end.


make_request(ServerId, Msg) ->
    ServerId ! Msg.


run() ->
    Pid = spawn(?MODULE, serve, []),  % spawn(Module, Exported_Function, List of Arguments).
    Pid2 = spawn(?MODULE, math, []),
    make_request(Pid, request1),
    make_request(Pid, request2),

    

    
    Pid2 ! {add, 1, 2},
    Pid2 ! {sub, 3, 2},
    Pid2 ! {variable, "Sanika"},
    ok.

