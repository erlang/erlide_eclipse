%% Author: Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
%% Created: 22-12-2010
%% Description: definitions and constants 
%%----------------------------------------------

-define(EVENT, cover_event).
-define(FINISHED, cover_fin).
-define(COVER_DIR, "cover_report").
-define(NO_FILE, nofile).
-define(INDEX, index).

%state
-record(state, {cover_type,
				includes = [],
				report_dir}).

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

%error record
-record(cover_error, {place,
					  type,
					  info = ""}).



