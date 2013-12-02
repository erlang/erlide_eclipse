-module(fun_names).

%% requires R17 to compile

h() ->
	fun F(0) -> 
			1;
		F(N)-> 
			N * F(N-1) 
	end.

