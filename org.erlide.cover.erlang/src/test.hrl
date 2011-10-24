%% Author: Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
%% Created: 17-02-2011
%% Description: definitions and constants for testing helper 
%%-------------------------------------------------------------

-define(TEVENT, eunit_event).


%state
-record(test_state, {type,
					 output = ""}).

-record(result, {pass,
				 fail,
				 skip,
				 cancel}).



