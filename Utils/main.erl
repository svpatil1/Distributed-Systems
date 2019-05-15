-module(main).
-export([react/2, ch3oh/4, h3oh/2, h2oh/2, h2o/1, o2/2, co2/0, ch2oh/1, ch3/1, h3/1, h2/1, o/2, co/1, oh/0, h/2]).


% The system starts with "react" process. It takes two argumets as input, CH3OH and O2 molecules
react(CH3OH, O2) ->
	M1 = #{"ch3oh"=>CH3OH, "o2"=>O2, "c"=>0, "h"=>0, "oh"=>0, "co2"=>0, "h2o"=>0, "o"=>0, "co"=>0},  % Initial state of molecules and ions
	io:format("Initial State: ~p~n~n", [M1]),

	% Spawning different processes
	Pid_oh = spawn(?MODULE, oh, []),

	Pid_co2 = spawn(?MODULE, co2, []),

	Pid_co = spawn(?MODULE, co, [Pid_co2]),

	Pid_o = spawn(?MODULE, o, [Pid_co, Pid_oh]),

	Pid_o2 = spawn(?MODULE, o2, [Pid_co2, Pid_o]),

	Pid_h2o = spawn(?MODULE, h2o, [Pid_o2]),
	
	Pid_h  = spawn(?MODULE, h, [Pid_o2, Pid_h2o]),

	Pid_h2  = spawn(?MODULE, h2, [Pid_h]),

	Pid_h3 = spawn(?MODULE, h3, [Pid_h2]),

	Pid_ch3 = spawn(?MODULE, ch3, [Pid_h3]),

	Pid_h2oh = spawn(?MODULE, h2oh, [Pid_h2o, Pid_h2]),

	Pid_ch2oh = spawn(?MODULE, ch2oh, [Pid_h2oh]),

	Pid_h3oh = spawn(?MODULE, h3oh, [Pid_h2oh, Pid_h3]),

	Pid_ch3oh = spawn(?MODULE, ch3oh, [Pid_h3oh, Pid_ch2oh, Pid_ch3, Pid_h2o]),

    
    % Create random number between 1 to 3 and make choices randomly
	RandomInt = rand:uniform(3),
   
    % Passing the message to different processes according to the path selected. 
    if
    	((CH3OH > 0) and (O2 > 0)) ->  % This condition is to make sure that atleast 1 molecule of oxygen and methanol is available for reaction
    		if 
    			(RandomInt == 1) ->  % Choice: H3OH
    				io:fwrite("CH3OH Process Choice: ~p~n~n",[RandomInt]),
    				Pid_ch3oh ! {c, M1};  % releases C ion from CH3OH molecule

    			(RandomInt == 2) -> % Choice: CH2OH
    				io:fwrite("CH3OH Process Choice: ~p~n~n",[RandomInt]),
    				Pid_ch3oh ! {h, M1};  % releases H ion from CH3OH molecule

    			(RandomInt == 3) ->  % Choice: CH3
    				io:fwrite("CH3OH Process Choice: ~p~n~n",[RandomInt]),
    				Pid_ch3oh ! {oh, M1};  % releases OH ion from CH3OH molecule
    				
    		true ->
    			io:fwrite("Error: incorrect path selection for CH3OH ~n~n")
    		end;
    	true ->
    		io:fwrite("Error: Start reaction with atleast one molecule of methane and oxygen each. ~n~n")
    end.

% Process CH3Oh
ch3oh(Pid_h3oh, Pid_ch2oh, Pid_ch3, Pid_h2o) ->
	receive
		{c, Map} ->
			I = maps:get("c", Map),
			J = I + 1,  % incrementing the C ion count
			MapUpdateC = maps:put("c", J, Map),
			K = maps:get("ch3oh", MapUpdateC),
			L = K - 1,  % decrementing the ch3oh molecule after its use

			% Check to make sure that CH3OH ion does not go negative
			if 
      			L < 0 -> 
      			MapUpdateCh3oh =  maps:put("ch3oh", 0, MapUpdateC),
         		io:fwrite("Error: Methane molecules less than 0 ~n~n");

         		true ->
         		MapUpdateCh3oh =  maps:put("ch3oh", L, MapUpdateC) 
      
   			end,

   			io:format("Process CH3OH: ~p~n~n", [MapUpdateCh3oh]),
   			Pid_h3oh ! {h, MapUpdateCh3oh, self(), Pid_h2o},  % message passing to H3OH process	
			ch3oh(Pid_h3oh, Pid_ch2oh, Pid_ch3, Pid_h2o);

		{h, Map} ->
			I = maps:get("h", Map),
			J = I + 1,  % incrementing the H ion count
			MapUpdateH = maps:put("h", J, Map),
			K = maps:get("ch3oh", MapUpdateH),
			L = K - 1 ,  % decrementing the ch3oh molecule after its use

			% Check to make sure that CH3OH ion does not go negative
			if 
      			L < 0 -> 
      			MapUpdateCh3oh =  maps:put("ch3oh", 0, MapUpdateH),
         		io:fwrite("Error: Methane molecules less than 0 ~n~n");

         		true ->
         		MapUpdateCh3oh =  maps:put("ch3oh", L, MapUpdateH) 
      
   			end,

   			io:format("Process CH3OH: ~p~n~n", [MapUpdateCh3oh]),
			Pid_ch2oh ! {c, MapUpdateCh3oh, self(), Pid_h2o},  % message passing to CH2OH process
			ch3oh(Pid_h3oh, Pid_ch2oh, Pid_ch3, Pid_h2o);


		{oh, Map} ->
			I = maps:get("oh", Map),
			J = I + 1,  % incrementing the OH ion count
			MapUpdateOH = maps:put("oh", J, Map),
			K = maps:get("ch3oh", MapUpdateOH),
			L = K - 1 ,  % decrementing the ch3oh molecule after its use

			% Check to make sure that CH3OH molecule does not go negative
			if 
      			L < 0 -> 
      			MapUpdateCh3oh =  maps:put("ch3oh", 0, MapUpdateOH),
         		io:fwrite("Error: Methane molecules less than 0 ~n~n");

         		true ->
         		MapUpdateCh3oh =  maps:put("ch3oh", L, MapUpdateOH) 
         		
   			end,

   			io:format("Process CH3OH: ~p~n~n", [MapUpdateCh3oh]),
			Pid_ch3 ! {c, MapUpdateCh3oh, self(), Pid_h2o},  % message passing to CH3 process
			ch3oh(Pid_h3oh, Pid_ch2oh, Pid_ch3, Pid_h2o);

		% Message is passed to this process when there are more than 1 CH3OH molecules. 
		% Message is passed to this process for all methanol molecules left after the consumption of 1st methanol molecule.
		{procCh3oh, Map} -> 
			CH3OH = maps:get("ch3oh", Map),
			O2 = maps:get("o2", Map),
			RandomInt = rand:uniform(3),  % make choices randomly for each CH3OH molecule

			% Passing the message to different processes according to the path selected.
			if	
				((CH3OH > 0) and (O2 > 0)) -> % This condition is to make sure that atleast 1 molecule of oxygen and methanol is available for reaction
    				if 
    					(RandomInt == 1) ->  % Choice: H3OH
    							io:fwrite("CH3OH Process Choice: ~p~n~n",[RandomInt]),
    							self() ! {c, Map},
    							ch3oh(Pid_h3oh,Pid_ch2oh, Pid_ch3, Pid_h2o);

    					(RandomInt == 2) ->  % Choice: CH2OH
    							io:fwrite("CH3OH Process Choice: ~p~n~n",[RandomInt]),
    							self() ! {h, Map},
    							ch3oh(Pid_h3oh,Pid_ch2oh, Pid_ch3, Pid_h2o);

    					(RandomInt == 3) ->  % Choice: CH3
    							io:fwrite("CH3OH Process Choice: ~p~n~n",[RandomInt]),
    							self() ! {oh, Map},
    							ch3oh(Pid_h3oh,Pid_ch2oh, Pid_ch3, Pid_h2o);
    				true ->
    					io:fwrite("Error: incorrect path selection for CH3OH ~n~n")
    				end;

	    		true ->
	    			io:fwrite("Insufficent CH3OH and O2 molecules to perform further reaction. ~n~n"),
	    			io:format("Ion reaction to form stable molecule if possible............ ~n~n"),
	    			io:format("State of the reaction ~p~n", [Map]),
	    			
	    			H = maps:get("h", Map),
	    			O2 = maps:get("o2", Map),
	    			C = maps:get("c", Map),
	    			CO2 = maps:get("co2", Map),
	    			O = maps:get("o", Map),
	    			O2 = maps:get("o2", Map),
	    			H2O = maps:get("h2o", Map),
	    			DoubleO = O2 * 4, 
					OH = maps:get("oh", Map),
				
					% Ion reactions to form stable molecules like CO2 and H2O
					% Checks if stable molecule can be formed dependiong on the amount of ions
	    			if
	    				((H > 0) and (DoubleO > 0 ) and (H == DoubleO)) -> % to check whether sufficient O ions are available to form H2O molecule
	    					MapUpdateO = maps:put("o", O2*2, Map),  % split o2 into O ions and add to map 
	    					H2O = maps:get("h2o", MapUpdateO),
	    					HalfH = H div 2,
	    					UpdateH2o = H2O + HalfH,
	    					MapUpdateH2o = maps:put("h2o", UpdateH2o, MapUpdateO),  % updating H2O molecule count
	    					MapUpdateOion = maps:put("o", 0, MapUpdateH2o),  % updating O ion count
	    					MapUpdateO2 = maps:put("o2", 0, MapUpdateOion),  % updating O2 molecule count
	    					MapUpdateH = maps:put("h", 0, MapUpdateO2),  % updating H ion count
	    					io:format("State of the reaction: ~p~n~n", [MapUpdateH]),
	    					self() ! {procCh3oh,MapUpdateH},
	    					ch3oh(Pid_h3oh, Pid_ch2oh, Pid_ch3, Pid_h2o);

	    				(C > 0) -> % check for C ion and O2 molecule to form stable CO2 molecule
	    					if
	    						(O2 > 0) ->
	    							MapUpdateC = maps:put("c", C - 1 , Map),  % updating C ion count
	    							MapUpdateO2 = maps:put("o2", O2 - 1 , MapUpdateC),  % updating O2 molecule count
	    							MapUpdateCO2 = maps:put("co2", CO2 + 1 , MapUpdateO2),  % updating CO2 molecule count
	    							io:format("State of reaction: ~p~n~n", [MapUpdateCO2]),
	    							self() ! {procCh3oh,MapUpdateCO2},  % pass the message to same process with updated state of ions  
		    						ch3oh(Pid_h3oh, Pid_ch2oh, Pid_ch3, Pid_h2o);
		    					true ->
	    								io:fwrite("C and O2 not sufficient to form CO2 molecule. ~n~n")
	    					end;

	    				(H > 0) -> % check for H and O ion to form stable H2O molecule
	    					if
	    						((O >= 1) and (H > 2)) ->
	    							MapUpdateH = maps:put("h", H - 2 , Map),  % updating H ion count
	    							MapUpdateO = maps:put("o", O - 1 , MapUpdateH),  % updating O ion count
	    							MapUpdateH2O = maps:put("h2o", H2O + 1 , MapUpdateO),  % updating H2O molecule count
	    							io:format("State of reaction: ~p~n~n", [MapUpdateH2O]),
	    							self() ! {procCh3oh, MapUpdateH2O},  % pass the message to same process with updated state of ions 
		    						ch3oh(Pid_h3oh, Pid_ch2oh, Pid_ch3, Pid_h2o);

	    						((O2 >= 1) and (H > 4)) ->
	    							MapUpdateH = maps:put("h", H - 4 , Map),  % updating H ion count
	    							MapUpdateO2 = maps:put("o2", O2 - 1 , MapUpdateH),  % updating O ion count
	    							MapUpdateH2O = maps:put("h2o", H2O + 2 , MapUpdateO2),  % updating H2O molecule count
	    							io:format("State of reaction: ~p~n~n", [MapUpdateH2O]),
	    							self() ! {procCh3oh, MapUpdateH2O},  % pass the message to same process with updated state of ions 
		    						ch3oh(Pid_h3oh, Pid_ch2oh, Pid_ch3, Pid_h2o);

		    					((OH > 0) and (H > 0)) ->
		    						OH = maps:get("oh", Map),
		    							H2O = maps:get("h2o", Map),
		    							MapUpdateOH = maps:put("oh", OH - 1, Map),  % updating OH ion count
		    							MapUpdateH = maps:put("h",H  - 1, MapUpdateOH),  % updating H ion count
		    							MapUpdateH2O = maps:put("h2o", H2O + 1, MapUpdateH),  % updating H2O molecule count
		    							io:format("State of OH, H and H2O: ~p~n~n", [MapUpdateH2O]),
		    							self() ! {procCh3oh,MapUpdateH2O},  % pass the message to same process with updated state of ions 
		    							ch3oh(Pid_h3oh, Pid_ch2oh, Pid_ch3, Pid_h2o);

		    					true ->
	    								io:fwrite("No sufficient ions to form H2O molecule. ~n~n")
	    					end;

	    				true ->
	    					io:fwrite("Reaction completed or ions not sufficient/available for further reaction. ~n~n")
	    			end,
	    			ch3oh(Pid_h3oh, Pid_ch2oh, Pid_ch3, Pid_h2o)	
	    		end,
	    	ch3oh(Pid_h3oh, Pid_ch2oh, Pid_ch3, Pid_h2o)		
	end.

% Process H3OH
h3oh(Pid_h2oh, Pid_h3) ->
	receive
		{h, Map, Pid_ch3oh, Pid_h2o} ->
			H = maps:get("h", Map), 
			OH = maps:get("oh", Map),	

			RandomInt = rand:uniform(2), % make choices randomly

	   			if
	   				(RandomInt == 1) ->   % Choice: H2OH					
						MapUpdate = maps:put("h", H + 1, Map),  % H ion count is incremented as it is released from this process
						io:fwrite("H3OH Process Choice: ~p~n~n",[RandomInt]),
						io:format("Process H3OH: ~p~n~n", [MapUpdate]),
						Pid_h2oh ! {h, MapUpdate, Pid_ch3oh, Pid_h2o},  % message passing to H2OH process		
						h3oh(Pid_h2oh, Pid_h3);

					(RandomInt == 2) ->  % Choice: H3
						MapUpdateOH = maps:put("oh", OH + 1 , Map), 
						io:fwrite("H3OH Process Choice: ~p~n~n",[RandomInt]),
						io:format("Process H3OH: ~p~n~n", [MapUpdateOH]),
						Pid_h3 ! {h, MapUpdateOH, Pid_ch3oh, Pid_h2o},
						h3oh(Pid_h2oh, Pid_h3)
	   			end,	
	   		h3oh(Pid_h2oh, Pid_h3)			
	end.

% Process CH2OH
ch2oh(Pid_h2oh) ->
	receive
		{c, Map, Pid_ch3oh, Pid_h2o} ->
			I = maps:get("c", Map),
			J = I + 1,  % C ion count is incremented as it is released from this process
			MapUpdate = maps:put("c", J, Map),
			io:format("Process CH2OH: ~p~n~n", [MapUpdate]),
			Pid_h2oh ! {c, MapUpdate, Pid_ch3oh, Pid_h2o},  % message passing to H2OH process	
			ch2oh(Pid_h2oh)
	end.

% Process CH3
ch3(Pid_h3) ->
	receive
		{c, Map, Pid_ch3oh, Pid_h2o} ->
			I = maps:get("c", Map),
			J = I + 1,  % C ion count is incremented as it is released from this process
			MapUpdate = maps:put("c", J, Map),
			io:format("Process CH3: ~p~n~n", [MapUpdate]),
			Pid_h3 ! {h, MapUpdate, Pid_ch3oh, Pid_h2o},  % message passing to H3 process	
			ch3(Pid_h3)
	end.

% Process H2OH
h2oh(Pid_h2o, Pid_h2) ->
	receive
		{h, Map, Pid_ch3oh, Pid_h2o} ->
			I = maps:get("h", Map),
			OH = maps:get("oh", Map),	

			RandomInt = rand:uniform(2), % make choices randomly

	   			if
	   				(RandomInt == 1) ->  % Choice: H2O
	   					J = I + 1,   % H ion count is incremented as it is released from this process
						MapUpdate = maps:put("h", J, Map),
						io:fwrite("H2OH Process Choice: ~p~n~n",[RandomInt]),
						io:format("Process H2OH: ~p~n~n", [MapUpdate]),
						Pid_h2o ! {procH2o, MapUpdate, Pid_ch3oh, Pid_h2o},  % message passing to H2O process	
						h2oh(Pid_h2o, Pid_h2);

					(RandomInt == 2) -> % Choice: H2
						MapUpdateOH = maps:put("oh", OH + 1 , Map),
						io:fwrite("H2OH Process Choice: ~p~n~n",[RandomInt]),
						io:format("Process H2OH: ~p~n~n", [MapUpdateOH]), 
						Pid_h2 ! {h, MapUpdateOH, Pid_ch3oh, Pid_h2o},
						h2oh(Pid_h2o, Pid_h2)
	   			end;	

		{c, Map, Pid_ch3oh, Pid_h2o} ->
			I = maps:get("h", Map),
			J = I + 1,  % H ion count is incremented as it is released from this process
			MapUpdate = maps:put("h", J, Map),
			io:format("Process H2OH: ~p~n~n", [MapUpdate]),
			Pid_h2o ! {procH2o, MapUpdate, Pid_ch3oh, Pid_h2o},  % message passing to H2O process	
			h2oh(Pid_h2o, Pid_h2)
	end.

% Process H3
h3(Pid_h2) ->
	receive
		{h, Map, Pid_ch3oh, Pid_h2o} ->
			I = maps:get("h", Map),
			J = I + 1,  % H ion count is incremented as it is released from this process
			MapUpdate = maps:put("h", J, Map),
			io:format("Process H3: ~p~n~n", [MapUpdate]),
			Pid_h2 ! {h, MapUpdate, Pid_ch3oh, Pid_h2o},  % message passing to H2 process
			h3(Pid_h2)
	end.

% Process H2
h2(Pid_h) ->
	receive
		{h, Map, Pid_ch3oh, Pid_h2o} ->
			I = maps:get("h", Map),  % get H ions
			MapUpdateH = maps:put("h", I + 1, Map), % update the count for H ion
			io:format("Process H2: ~p~n~n", [MapUpdateH]),
			Pid_h ! {h, MapUpdateH, Pid_ch3oh, Pid_h2o},  % message passing to H process
			h2(Pid_h)
	end.

% Process H
h(Pid_o2, Pid_h2o) ->
	receive
		{h, Map, Pid_ch3oh, Pid_h2o} ->
			OH = maps:get("oh", Map),  % get OH ions

			RandomInt = rand:uniform(2),  % make choices randomly

			if
				(RandomInt == 1) ->  % Choice: nil process
					self() ! {h_nil, Map, Pid_ch3oh, Pid_h2o},  % message passing to H process
					h(Pid_o2, Pid_h2o);

				(RandomInt == 2) -> % Choice: H2O
					if
						(OH > 0) ->
							self() ! {oh, Map, Pid_ch3oh, Pid_h2o},  % message passing to H process and form H2O
							h(Pid_o2, Pid_h2o);
						(OH == 0) ->
							self() ! {h_nil, Map, Pid_ch3oh, Pid_h2o},  % if no OH ion present to form water
							h(Pid_o2, Pid_h2o)
					end,
				h(Pid_o2, Pid_h2o)
			end;

		{h_nil, Map, Pid_ch3oh, Pid_h2o} ->
			I = maps:get("h", Map),  % get H ions
			MapUpdateH = maps:put("h", I + 1, Map), % increment the count for H ion andpass message to O2 process for further reaction
			io:format("H Process Choice 1........... ~n~n"),
			io:format("Process H: ~p~n~n", [MapUpdateH]),
			Pid_ch3oh ! {procCh3oh, MapUpdateH},  % message passed to CH3OH process to carry out decomposition of methanol molecule if any	
			h(Pid_o2, Pid_h2o);

		{oh, Map, Pid_ch3oh, Pid_h2o} ->
			OH = maps:get("oh", Map),  % get OH ions
			MapUpdateOH = maps:put("oh", OH - 1, Map), % update the count for H ion
			io:format("H Process Choice 2........... ~n~n"), 
			io:format("Process H: ~p~n~n", [MapUpdateOH]),
			Pid_h2o ! {procH2o, MapUpdateOH, Pid_ch3oh, Pid_h2o},	
			h(Pid_o2, Pid_h2o)

	end.

% Process H2O
h2o(Pid_o2) ->
	receive
		{procH2o, Map, Pid_ch3oh, Pid_h2o} ->
			I = maps:get("h2o", Map),
			J = I +1,  % H2O molecule count is incremented as it is the stable molecule obtained
			MapUpdateH2O = maps:put("h2o", J, Map),
			io:format("Process_A H2O: ~p~n~n", [MapUpdateH2O]),
			Pid_o2 ! {c, MapUpdateH2O, Pid_ch3oh, Pid_h2o},	
			h2o(Pid_o2);

		{procH2o_B, Map, Pid_ch3oh, Pid_h2o} ->
			I = maps:get("h2o", Map),
			J = I +1,  % H2O molecule count is incremented as it is the stable molecule obtained
			MapUpdateH2O = maps:put("h2o", J, Map),
			io:format("Process_B H2O: ~p~n~n", [MapUpdateH2O]),
			Pid_ch3oh ! {procCh3oh, MapUpdateH2O},
			h2o(Pid_o2)

	end.

% Process O2
o2(Pid_co2, Pid_o) ->
	receive
		{c, Map, Pid_ch3oh, Pid_h2o} ->
			I = maps:get("co2", Map),
			J = I +1,  % CO2 molecule count is incremented as it is the stable molecule obtained
			MapUpdateCo2 = maps:put("co2", J, Map),
			K = maps:get("o2", MapUpdateCo2),
			L = K - 1 ,% decrementing the o2 molecule after use

			% Check to make sure that O2 molecule does not go negative
			if 
      			L < 0 -> 
	      			MapUpdateO2 =  maps:put("o2", 0, MapUpdateCo2),
	         		io:fwrite("Error: Oxygen molecules less than 0 ~n~n");

         		true ->
	         		MapUpdateO2 =  maps:put("o2", L, MapUpdateCo2),
	         		io:fwrite("Check: Oxygen molecule greater than 0 ~n~n")  
      
   			end,

   			%  Make choices randomly for the process to further perform reaction.
   			RandomInt = rand:uniform(2), 
   			
   			if
				(RandomInt == 1)  -> % Choice: CO2
					io:fwrite("O2 Process Choice: ~p~n~n",[RandomInt]),
					io:format("Process O2: ~p~n~n", [MapUpdateO2]),
					Pid_co2 ! {procCo2, MapUpdateO2, Pid_ch3oh, Pid_h2o},  % message passing to CO2 process which releases CO2 molecule	
					o2(Pid_co2, Pid_o);

				(RandomInt == 2)  -> % Choice: O
					io:fwrite("O2 Process Choice: ~p~n~n",[RandomInt]),
					io:format("Process O2: ~p~n~n", [MapUpdateO2]),
					Pid_o ! {procO, MapUpdateO2, Pid_ch3oh, Pid_h2o},  % message passing to O process
					o2(Pid_co2, Pid_o);

				true ->
					io:format("Error in selecting random paths for O2")
			end,
			
			io:format("Process O2: ~p~n~n", [MapUpdateO2]),
			o2(Pid_co2, Pid_o)
	end.

% Process CO2
co2() ->
	receive
		{procCo2, Map, Pid_ch3oh, Pid_h2o} ->
			I = maps:get("c", Map),
			J = I - 1,  % decrementing c ion as it is consumed and formed stable CO2 molecule

			% Check to make sure that C molecule does not go negative
			if
				J < 0  ->
					MapUpdateC = maps:put("c", 0, Map);
				true ->
					MapUpdateC = maps:put("c", J, Map)
			end,
			
			io:format("Process_A CO2: ~p~n~n", [MapUpdateC]),
			Pid_ch3oh ! {procCh3oh, MapUpdateC},  % pass the message to CH3Oh process for further reaction between remaining methanol and oxygen molecules 
			co2();

		{procCo2_B, Map, Pid_ch3oh, Pid_h2o} ->
			CO2 = maps:get("co2", Map),
			MapUpdateCO2 = maps:put("co2", CO2 + 1, Map),
			io:format("Process_B CO2: ~p~n~n", [MapUpdateCO2]),
			Pid_ch3oh ! {procCh3oh, MapUpdateCO2},  % pass the message to CH3Oh process for further reaction between remaining methanol and oxygen molecules 	
			co2()

	end.

% Process O
o(Pid_co, Pid_oh) ->
	receive
		{procO, Map, Pid_ch3oh, Pid_h2o} ->
			I = maps:get("o", Map),
			J = I + 1,  % incrementing O ion count
			MapUpdateO = maps:put("o", J, Map),
			C = maps:get("c", MapUpdateO),
			H = maps:get("h", MapUpdateO),
			CO = maps:get("co", MapUpdateO),	
   			
   			RandomInt = rand:uniform(2),  % make choices randomly between CO and OH process, for O ion to undergo reaction
   			
   			if
   				(RandomInt == 1) ->  % Choice: CO
   				
					if 
						(C > 0)  ->
							MapUpdateC = maps:put("c", C - 1, MapUpdateO),  % decremented C, as it is consumed to form CO
							MapUpdateCO = maps:put("co", CO + 1, MapUpdateC),  % increment CO ion
							io:fwrite("O Process Choice: ~p~n~n",[RandomInt]),
							io:format("Process O: ~p~n~n", [MapUpdateCO]),  
							Pid_co ! {procCo, MapUpdateCO, Pid_ch3oh, Pid_h2o},  % message passing to CO process		 
							o(Pid_co, Pid_oh);

						(C == 0) ->
							io:format("No enough C ion to form CO. Thus status of reaction is: ~p~n~n", [MapUpdateO]),
							Pid_ch3oh ! {procCh3oh, MapUpdateO},
							o(Pid_co, Pid_oh);

						true ->
							io:format("Error: C ions negative")
					end;

				(RandomInt == 2) -> % Choice: H2O
					if
						(H > 0)  ->
							MapUpdateH = maps:put("h", H - 1, MapUpdateO),  % decremented H, as it is consumed to form OH to form H2O
							io:fwrite("O Process Choice: ~p~n~n",[RandomInt]),
							io:format("Process O: ~p~n~n", [MapUpdateH]),  % message passing to O process
							Pid_h2o ! {procH2o_B, MapUpdateH, Pid_ch3oh, Pid_h2o},  % message passing to H2O process .	
							o(Pid_co, Pid_oh);

						(H == 0) ->
							io:format("No enough H ion to form OH. Thus status of reaction is: ~p~n~n", [MapUpdateO]),
							Pid_ch3oh ! {procCh3oh, MapUpdateO},
							o(Pid_co, Pid_oh);

						true ->
							io:format("Error: C ions negative")

					end;
				
				true ->
					Pid_ch3oh ! {procCh3oh, MapUpdateO},
					o(Pid_co, Pid_oh)
			end,
			o(Pid_co, Pid_oh)
	end.

% Process CO
co(Pid_co2) ->
	receive
		{procCo, Map, Pid_ch3oh, Pid_h2o} -> 
			O = maps:get("o", Map),
			
			if
				(O > 0)  ->  % If O ion is present, then form CO2
					MapUpdateO = maps:put("o", O - 1, Map),  % decremented O, as it is consumed to form CO2
					io:format("Process CO: ~p~n~n", [MapUpdateO]),
					Pid_co2 ! {procCo2_B, MapUpdateO, Pid_ch3oh, Pid_h2o},  % message passing to CO2 process which releases CO2 molecule
					co(Pid_co2);

				(O =< 0)  ->  % If O ion is not available for reaction, pass message to CH3OH process for further reaction
					io:format("Process CO: ~p~n~n", [Map]),
					Pid_ch3oh ! {procCh3oh, Map},  % message passing to CH3OH process
					co(Pid_co2);
				true ->
					io:format("Something went wrong. O ion count cannot be negative ~n~n")
			end,
			co(Pid_co2)
	end.

% Process OH
oh() ->
	receive
		{procOh, Map, Pid_ch3oh, Pid_h2o} -> 
			H = maps:get("h", Map),
			
			if
				(H > 0)  ->
					MapUpdateH = maps:put("h", H - 1, Map),  % decremented H, as it is consumed to form H2O
					io:format("Process OH: ~p~n~n", [MapUpdateH]),
					Pid_h2o ! {procH2o_B, MapUpdateH, Pid_ch3oh, Pid_h2o},  % pass the message to CH3OH process for further reaction between remaining methanol and oxygen molecules 
					oh();

				(H =< 0)  ->
					io:format("Process OH: ~p~n~n", [Map]),
					Pid_ch3oh ! {procCh3oh, Map},  % message passing to Ch3OH process
					oh();
				true ->
					io:format("Something went wrong. O ion count cannot be negative ~n~n")
			end,
			oh()
			
	end.
