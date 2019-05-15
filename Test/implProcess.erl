- module(implProcess).
-export([start/0, processB/3, processA/0]).

processA() ->
	receive
		finished ->
            io:format("Process B finished~n", []);
		
		{oh,PidB,MapValue} ->
			io:format("OH ion~n", []),
			io:format("map is ~p~n", [MapValue]),
			PidB ! procA,
			processA();
		{c,PidB} ->
			io:format("c ion~n", []),
			PidB ! procA,
			processA()

	end.

processB(0,PidA,MapValue) ->
	PidA ! finished,
    io:format("process B finished~n", []),	
    io:format("process B is finished with map value ~p~n", [MapValue]);

processB(N,PidA,MapValue) ->
	I = maps:get("oh",MapValue),
	J = I +1,
	Map1 = maps:put("oh",J,MapValue),	
	PidA ! {oh , self(),Map1},
	receive
		procA ->
			io:format("Proc B received Proc A~n", [])
	end,
	processB(N - 1, PidA, Map1).





start() ->
 	M1 = #{"c"=>0,"h"=>0,"oh"=>0}, 
 	io:format("Initial map value ~p~n", [M1]),

    PidA = spawn(implProcess, processA, []),
    spawn(implProcess, processB, [10, PidA,M1]).
    
	



	