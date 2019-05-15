
-module(main_test).
-import(main, [react/2, ch3oh/4, h3oh/2, h2oh/2, h2o/1, o2/2, co2/0, ch2oh/1, ch3/1, h3/1, h2/1, o/2, co/1, oh/0, h/2]).
-include_lib("eunit/include/eunit.hrl").


expect(Val) ->
	receive
		Val ->
			ok;
		Other ->
			{error, Other}
	end.

% In all test cases, value of the variable "Map" passesd to the message are dummy values
% test CH3OH process (choice 1: via H3OH)
ch3oh_test() ->
	
	Map = #{"ch3oh"=>2, "o2"=>2, "c"=>0, "h"=>0, "oh"=>0, "co2"=>0, "h2o"=>0, "o"=>0, "co"=>0},  % initial value of ions
	PID = spawn(main, ch3oh, [self(), 5, 10, 1]),
	PID ! {c, Map},
	MapUpdateCh3oh = #{"ch3oh"=>1, "o2"=>2, "c"=>1, "h"=>0, "oh"=>0, "co2"=>0, "h2o"=>0, "o"=>0, "co"=>0},	 % value of ions after message passing
	ok = expect({h, MapUpdateCh3oh, PID, 1}).

% test CH3OH process (choice 2: via CH2OH)
ch3oh1_test() ->
	
	Map = #{"ch3oh"=>2, "o2"=>2, "c"=>0, "h"=>0, "oh"=>0, "co2"=>0, "h2o"=>0, "o"=>0, "co"=>0},  % initial value of ions
	PID = spawn(main, ch3oh, [5, self(),10, 1]),
	PID ! {h, Map},
	MapUpdateCh3oh = #{"ch3oh"=>1, "o2"=>2, "c"=>0, "h"=>1, "oh"=>0, "co2"=>0, "h2o"=>0, "o"=>0, "co"=>0},	% value of ions after message passing
	ok = expect({c, MapUpdateCh3oh, PID, 1}).

% test CH3OH process (choice 3: via CH3)
ch3oh2_test() ->
	
	Map = #{"ch3oh"=>2, "o2"=>2, "c"=>0, "h"=>0, "oh"=>0, "co2"=>0, "h2o"=>0, "o"=>0, "co"=>0},  % initial value of ions
	PID = spawn(main, ch3oh, [5, 10, self(), 1]),
	PID ! {oh, Map},
	MapUpdateCh3oh = #{"ch3oh"=>1, "o2"=>2, "c"=>0, "h"=>0, "oh"=>1, "co2"=>0, "h2o"=>0, "o"=>0, "co"=>0},  % value of ions after message passing
	ok = expect({c, MapUpdateCh3oh, PID, 1}).

% test H3OH process (choice 1: via H2OH)
h3oh_test() ->
	
	Map = #{"ch3oh"=>2, "o2"=>2, "c"=>0, "h"=>0, "oh"=>0, "co2"=>0, "h2o"=>0, "o"=>0, "co"=>0},  % initial value of ions
	PID = spawn(main, h3oh, [self(), 1]),
	PID ! {h, Map, 5, 10},
	MapUpdateh3oh = #{"ch3oh"=>2, "o2"=>2, "c"=>0, "h"=>1, "oh"=>0, "co2"=>0, "h2o"=>0, "o"=>0, "co"=>0},  % value of ions after message passing
	ok = expect({h, MapUpdateh3oh, 5, 10}).

% test H3OH process (choice 2: via H3)
h3oh1_test() ->
	
	Map = #{"ch3oh"=>2, "o2"=>2, "c"=>0, "h"=>0, "oh"=>0, "co2"=>0, "h2o"=>0, "o"=>0, "co"=>0},  % initial value of ions
	PID = spawn(main, h3oh, [1, self()]),
	PID ! {h, Map, 5, 10},
	MapUpdateh3oh = #{"ch3oh"=>2, "o2"=>2, "c"=>0, "h"=>0, "oh"=>1, "co2"=>0, "h2o"=>0, "o"=>0, "co"=>0},  % value of ions after message passing	
	ok = expect({h, MapUpdateh3oh, 5, 10}).

% test CH2OH process
ch2oh_test() ->
	
	Map = #{"ch3oh"=>2, "o2"=>2, "c"=>0, "h"=>0, "oh"=>0, "co2"=>0, "h2o"=>0, "o"=>0, "co"=>0},  % initial value of ions
	PID = spawn(main, ch2oh, [self()]),
	PID ! {c, Map, 5, 10},
	MapUpdatech2oh = #{"ch3oh"=>2, "o2"=>2, "c"=>1, "h"=>0, "oh"=>0, "co2"=>0, "h2o"=>0, "o"=>0, "co"=>0},	% value of ions after message passing
	ok = expect({c, MapUpdatech2oh, 5, 10}).

% test CH3 process
ch3_test() ->
	
	Map = #{"ch3oh"=>2, "o2"=>2, "c"=>0, "h"=>0, "oh"=>0, "co2"=>0, "h2o"=>0, "o"=>0, "co"=>0},  % initial value of ions
	PID = spawn(main, ch3, [self()]),
	PID ! {c, Map, 5, 10},
	MapUpdatech3 = #{"ch3oh"=>2, "o2"=>2, "c"=>1, "h"=>0, "oh"=>0, "co2"=>0, "h2o"=>0, "o"=>0, "co"=>0},  % value of ions after message passing
	ok = expect({h, MapUpdatech3, 5, 10}).

% test H3 process
h3_test() ->
	
	Map = #{"ch3oh"=>2, "o2"=>2, "c"=>0, "h"=>0, "oh"=>0, "co2"=>0, "h2o"=>0, "o"=>0, "co"=>0},  % initial value of ions
	PID = spawn(main, h3, [self()]),
	PID ! {h, Map, 5, 10},
	MapUpdateh3 = #{"ch3oh"=>2, "o2"=>2, "c"=>0, "h"=>1, "oh"=>0, "co2"=>0, "h2o"=>0, "o"=>0, "co"=>0},  % value of ions after message passing
	ok = expect({h, MapUpdateh3, 5, 10}).

% test H2 process
h2_test() ->
	
	Map = #{"ch3oh"=>2, "o2"=>2, "c"=>0, "h"=>0, "oh"=>0, "co2"=>0, "h2o"=>0, "o"=>0, "co"=>0},  % initial value of ions
	PID = spawn(main, h2, [self()]),
	PID ! {h, Map, 5, 10},
	MapUpdateh3 = #{"ch3oh"=>2, "o2"=>2, "c"=>0, "h"=>1, "oh"=>0, "co2"=>0, "h2o"=>0, "o"=>0, "co"=>0},	 % value of ions after message passing
	ok = expect({h, MapUpdateh3, 5, 10}).

% test H2O process
h2o_test() ->
	
	Map = #{"ch3oh"=>2, "o2"=>2, "c"=>0, "h"=>0, "oh"=>0, "co2"=>0, "h2o"=>0, "o"=>0, "co"=>0},  % initial value of ions
	PID = spawn(main, h2o, [self()]),
	PID ! {procH2o, Map, 5, 10},
	MapUpdateh2o = #{"ch3oh"=>2, "o2"=>2, "c"=>0, "h"=>0, "oh"=>0, "co2"=>0, "h2o"=>1, "o"=>0, "co"=>0},  % value of ions after message passing	
	ok = expect({c, MapUpdateh2o, 5, 10}).

% test CO process
co_test() ->
	
	Map = #{"ch3oh"=>2, "o2"=>2, "c"=>0, "h"=>0, "oh"=>0, "co2"=>0, "h2o"=>0, "o"=>1, "co"=>0},  % initial value of ions
	PID = spawn(main, co, [self()]),
	PID ! {procCo, Map, 5, 10},
	MapUpdateCo = #{"ch3oh"=>2, "o2"=>2, "c"=>0, "h"=>0, "oh"=>0, "co2"=>0, "h2o"=>0, "o"=>0, "co"=>0},  % value of ions after message passing	
	ok = expect({procCo2_B, MapUpdateCo, 5, 10}).



