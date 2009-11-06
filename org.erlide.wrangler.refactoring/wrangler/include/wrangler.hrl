%%% This is an -*- Erlang -*- file.
%%%-------------------------------------------------------------------
%%% File    : wrangler.hrl
%%%-------------------------------------------------------------------

-record(options, {search_paths=[],
		  include_dirs=[],
		  plt_libs= [kernel,stdlib]
		  }).

-record(attr, {pos = {0,0}, ann = [], com = none}).

%% Will be edited by Makefile 

-define(WRANGLER_DIR, filename:dirname(lists:filter(fun(X)-> lists:suffix("wrangler/ebin", X) end, code:get_path()))  ).

-define(DEFAULT_LOC, 
        {0, 0}).  %% default defining location.
-define(DEFAULT_MODULE,
	unknown).  %% default module name.

-define(ModuleGraphTab, wrangler_modulegraph_tab).

-define(DEFAULT_TABWIDTH, 8).  %% default number of characters represented by a Tab key.
-define(DEFAULT_EUNIT_TEST_SUFFIX, "_test").
-define(DEFAULT_EUNIT_GENERATOR_SUFFIX, "_test_").
-define(DEFAULT_EUNIT_TESTMODULE_SUFFIX, "_tests").
-define(DEFAULT_EQC_PROP_PREFIX, "prop_").
-define(DEFAULT_TS_MODULE_SUFFIX, "_SUITE").



-ifdef(EMACS).
-define(wrangler_io(__String, __Args), refac_io:format(__String, __Args)).
-else.
-define(wrangler_io(__String, __Args), ok).
-endif.
      

-type(filename()::string()).
-type(modulename()::atom()).
-type(functionname()::atom()).
-type(functionarity()::integer()).
-type(dir()::string()).
-type(syntaxTree()::any()).    %% any() should be refined.
-type(pos()::{integer(), integer()}).
-type(line()::integer()).
-type(col()::integer()).
-type(key():: attributes | errors | exports | functions | imports | module | records | rules | warnings).
-type(moduleInfo()::[{key(), any()}]).  %% any() should be refined.
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
      
