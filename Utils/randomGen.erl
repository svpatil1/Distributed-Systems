-module(randomGen).
-export([randomNumGen/1]).



randomNumGen(N) ->
	RandomInt = rand:uniform(N),
	io:format("Random number is: ~p ~n", [RandomInt]),
	RandomInt.

