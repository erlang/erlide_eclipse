-module(erlide_edoc).

-export([files/2]).

files(Files, Options) ->
	try
		erlide_log:logp(Files),
		erlide_log:logp(Options),
		[begin erlide_log:logp(F), edoc:files([F], Options) end || F<-Files]
	catch
		_:Err ->
			{error, Err}
	end.
