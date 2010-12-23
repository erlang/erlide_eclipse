%% Author: Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
%% Created: 22-12-2010
%% Description: definitions and constants 
%%----------------------------------------------

-define(EVENT, cover_event).
-define(OK, cover_ok).
-define(ERROR, cover_error).
-define(COVER_DIR, "cover_report").
-define(NO_FILE, nofile).


%state
-record(state, {cover_type}).

%results per module
-record(module_res, {name, 
					 name_html, 
					 line_num,
					 covered_num,
					 percentage,
					 lines = [], 
					 functions = []}).

%results per unit (module, function)
-record(unit_res, {name,
				   arity,
				   total_l,
				   covered_l,
				   percentage}).

%results: lines
-record(line_res, {num,
				   calls}).



