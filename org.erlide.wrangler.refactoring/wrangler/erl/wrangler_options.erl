
%% This module is not used at the moment.

-module(wrangler_options).

-export([build/1]).

-include("../hrl/wrangler.hrl").

build(Opts) ->
  build_options(Opts, #options{}).

build_options([Term={OptionName,Value}|Rest], Options) ->
  case OptionName of
    search_paths ->
      build_options(Rest, Options#options{search_paths=Value});
    include_dirs ->
      build_options(Rest, Options#options{include_dirs=Value});
    plt_libs ->
      build_options(Rest, Options#options{plt_libs=Value});
     _ ->
      io:format("Bad Options:~p:\n", [Term])
  end;
build_options([Term|_Rest], _Options) ->
     io:format("Bad Options:~p:\n", [Term]);
build_options([], Options) ->
  Options.
