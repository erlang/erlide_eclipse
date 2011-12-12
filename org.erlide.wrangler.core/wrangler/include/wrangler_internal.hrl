%%% This is an -*- Erlang -*- file.
%%%-------------------------------------------------------------------
%%% File    : wrangler.hrl
%%%-------------------------------------------------------------------

-record(options, {search_paths=[],
		  include_dirs=[],
		  plt_libs= [kernel,stdlib]
		  }).

-record(attr, {pos = {0,0}, ann = [], com = none}).

% type defined by typer:
-record(typer_analysis,
	{mode					,
	 macros      = []			, % {macro_name, value}
	 includes    = []			,
	 
	 %% Esp for Dialyzer
	 %% ----------------------
	 code_server = dialyzer_codeserver:new(),
	 callgraph   = dialyzer_callgraph:new(),
	 ana_files   = [],
	 plt         = none,
	 
	 %% Esp for TypEr
	 %% ----------------------
	 t_files     = [], 
	 
	 %% For choosing between contracts or comments
	 contracts   = true,
	 
	 %% Any file in 'final_files' is compilable.
	 %% And we need to keep it as {FileName,ModuleName}
	 %% in case filename does NOT match with moduleName
	 final_files = [],  
	 
	 ex_func     = dict:new(),
	 record      = dict:new(),
	 
	 %% Functions: the line number of the function 
	 %%            should be kept as well
	 func        = dict:new(),
	 inc_func    = dict:new(),
	 trust_plt   = dialyzer_plt:new()}).


-define(DEFAULT_LOC, 
        {0, 0}).  %% default defining location.
-define(DEFAULT_MODULE,
	unknown).  %% default module name.

-define(ModuleGraphTab, wrangler_modulegraph_tab).

-define(DEFAULT_TABWIDTH, 8).  
-define(DEFAULT_EUNIT_TEST_SUFFIX, "_test").
-define(DEFAULT_EUNIT_GENERATOR_SUFFIX, "_test_").
-define(DEFAULT_EUNIT_TESTMODULE_SUFFIX, "_tests").
-define(DEFAULT_EQC_PROP_PREFIX, "prop_").
-define(DEFAULT_TS_MODULE_SUFFIX, "_SUITE").
-define(EMACS, true).
-define(MONITOR, true).

-define(WRANGLER_DIR, filename:dirname(lists:filter(fun(X)-> lists:suffix("wrangler/ebin", X) end, code:get_path())) ).

-ifdef(EMACS).
-define(wrangler_io(__String, __Args),wrangler_io:format(__String, __Args)).
-else.
-define(wrangler_io(__String, __Args), ok).
-endif.
     
-define(log_warning(String), wrangler_error_logger:add_to_logger({warning, String})).
     
%%-define(DEBUG, true)
 
-ifdef(DEBUG).
-define(debug(__String, __Args), ?wrangler_io(__String, __Args)).
-else.
-define(debug(__String, __Args), ok).
-endif.

    
-type(filename()::string()).
-type(modulename()::atom()).
-type(functionname()::atom()).
-type(functionarity()::integer()).
-type(dir()::string()).
-type (syntaxTree() :: {tree, any(), any(), any()}| {wrapper, any(), any(), any()}).
-type(pos()::{integer(), integer()}).
-type(line()::integer()).
-type(col()::integer()).
-type(key():: attributes | errors | exports | functions | imports | module_imports | module | records | rules | warnings).
-type(module_info()::[{key(), any()}]).  
-type(anyterm()::any()).
-type(editor()::emacs|eclipse).
-type(whitespace() :: '\t' | '\n' | ' ').
-type(token() :: {'var', pos(), atom()} | {'integer', pos(), integer()} | {'string', pos(), string()}
	       | {'float', pos(), float()} | {'char', pos(), char()}
	       | {'atom', pos(), atom()} | {atom(), pos()}
	       | {'whitespace', pos(), whitespace()} | {'comment', pos(), string()}).

-type(scc_order()::[[{{atom(), atom(), integer()}, syntaxTree()}]]).
-type(callercallee()::[{{modulename(), functionname(), functionarity()}, [{modulename(), functionname(), functionarity()}]}]).
-type(external_calls()::[{atom(), atom(), integer()}]).
-record(callgraph, {'callercallee', 'scc_order', 'external_calls'}).
      
