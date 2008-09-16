%%% This is an -*- Erlang -*- file.
%%%-------------------------------------------------------------------
%%% File    : wrangler.hrl
%%%-------------------------------------------------------------------

-record(options, {search_paths=[],
		  include_dirs=[],
		  plt_libs= [kernel,stdlib]
		  }).

-record(callgraph, {scc_order, external_calls}).

-record(attr, {pos = {0,0}, ann = [], com = none}).

%% Edited by Gyorgy Orosz
-define(WRANGLER_DIR, filename:dirname(lists:filter(fun(X)-> lists:suffix("wrangler/ebin", X) end, code:get_path()))  ).


-define(DEFAULT_LOC,
        {0, 0}).  %% default defining location.
-define(DEFAULT_MODULE,
	unknown).  %% default module name.

