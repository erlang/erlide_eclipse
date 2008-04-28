%% Author: jakob
%% Created: 19 sep 2007
%% Description: TODO: Add description to erlide_pair_match
-module(erlide_pair_match).

%%
%% Include files
%%
-include("erlide_scanner.hrl").
-include("erlide.hrl").

%%
%% Exported Functions
%%
-export([match/2]).

%%
%% API Functions
%%

match(Offset, Module) ->
    T = erlide_scanner:getTokenAt(Module, Offset),
    Kind = T#token.kind,
    case erlide_text:is_paren(Kind) of
        false ->
            false;
        OC ->
            case find_match(Kind, OC, T, Module) of
               	false ->
                    false;
                TMatch ->
            		TMatch#token.offset
            end
    end.

%%
%% Local Functions
%%

next_token(opening, X, Module) -> erlide_scanner:getNextToken(Module, X);
next_token(closing, X, Module) -> erlide_scanner:getPrevToken(Module, X).

find_match(Paren, OC, T, Module) ->
    TF = next_token(OC, T, Module),
    case TF of
        T ->
            false;
        _ ->
            Kind = TF#token.kind,
            case erlide_text:is_paren(Kind) of
                false -> % not a paren, move on
                    find_match(Paren, OC, TF, Module);
                OC -> % recurse if new paren of same sort
                    case find_match(Kind, OC, TF, Module) of
                        false ->
                            false;
                        TN ->
	                    	find_match(Paren, OC, TN, Module)
                    end;
                _ -> % matching paren, ok we're done
                    TF
            end
    end.

