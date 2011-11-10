%% Copyright (c) 2010, Huiqing Li, Simon Thompson  
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%     %% Redistributions of source code must retain the above copyright
%%       notice, this list of conditions and the following disclaimer.
%%     %% Redistributions in binary form must reproduce the above copyright
%%       notice, this list of conditions and the following disclaimer in the
%%       documentation and/or other materials provided with the distribution.
%%     %% Neither the name of the copyright holders nor the
%%       names of its contributors may be used to endorse or promote products
%%       derived from this software without specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ''AS IS''
%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS AND CONTRIBUTORS 
%% BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR 
%% BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR 
%% OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF 
%% ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%% =====================================================================
%% Refactoring: Rename a variable name.
%%
%% Author contact: hl@kent.ac.uk, sjt@kent.ac.uk
%%
%% =====================================================================
%%
%% @doc Rename a variable with a user-supplied new variable name.
%% <p> To apply this refactoring, point the cursor to the beginning of any occurrence of this variable, then select
%% <em> Rename Variable Name </em> from the <em> Refactor </em> menu, after that the refactorer will prompt to enter
%% the new parameter name in the mini-buffer.
%% </p>
%% <p> This refactoring has a local effect, i.e., it only affects the function in which the refactoring is initialised.
%% </p>
%% <p> The following <em> side-conditions </em> apply to this refactoring:
%% <li> The new variable name should not conflict with any of the declared variable names in the same scope;</li>
%% <li> The new name should not shadow any of the existing variables in the outer scopes, or be shadowed by any of
%% of existing variables in the inner scopes, i.e., renaming to the new name should not change the semantics of the
%% program.</li>
%% </p>
%% @end

%% @private
-module(wrangler_add_new_refac).

-export([add/5]).

-include("../include/wrangler_internal.hrl").

add(File, Type, ExistingMenuItems, IDE, WranglerInstDir) ->
    case filename:extension(File) of 
        ".erl" ->
            case is_behaviour_file(File, Type) of 
                true ->
                    ModName = filename:basename(File, ".erl"),
                    case lists:member(list_to_atom(ModName),[Name||[_, Name]<-ExistingMenuItems]) of 
                        false ->
                            Dir = get_wrangler_dir(IDE, WranglerInstDir),
                            OutDir = Dir ++ "/ebin",
                            case compile:file(File, [{i, Dir++"/include"}, {outdir, OutDir}]) of
                                {ok, _} -> 
                                    NewCode = gen_new_code(ModName, Type),
                                    ElFile = Dir ++ "/elisp/wrangler.el",
                                    case file:write_file(ElFile, list_to_binary(NewCode), [append]) of
                                        ok ->
                                            {ok, ElFile};
                                        {error, Reason} ->
                                            {error, Reason}
                                    end;                
                                _ -> throw({error, "the current file does not compile."})
                            end;
                        _ -> throw({error, "menu item already exists."})
                    end;
                _ ->
                    Msg = format_msg("the current file does not implement a ~p behaviour.\n",
                                     [Type]),
                    throw({error, Msg})
            end;
        _ ->
            throw({error, "the current file is not an Erlang source file."})
    end. 


is_behaviour_file(File, Behaviour) ->
    {ok, {AnnAST, _Info}} = wrangler_ast_server:parse_annotate_file(File, true, []),
    Forms = wrangler_syntax:form_list_elements(AnnAST),
    lists:any(fun (F) ->
		      case wrangler_syntax:type(F) of
                          attribute ->
                              case wrangler_syntax:revert(F) of 
                                  {attribute, _Pos, behaviour, Behaviour} ->
                                      true;
                                  _ -> false
                              end;
			  _ -> false
		      end
	      end, Forms).

gen_new_code(ModName, Type) ->
    M = list_to_atom(ModName),
    {AddMenuItem, NewFun} =
        case Type of 
            gen_refac ->
                {format_msg("\n\n(setq my_gen_refac_menu_items (append my_gen_refac_m"
                            "enu_items `((\"~s\" ~p))))",
                            [ModName, list_to_atom(ModName)]),
                 format_msg("\n\n(defun ~p() \n   (interactive)\n   (apply-adhoc-refa"
                            "c '~p))",
                            [M, M])};
            gen_composite_refac ->
                {format_msg("\n\n(setq my_gen_composite_refac_menu_items (append "
                            "my_gen_composite_refac_menu_items `((\"~s\" ~p))))",
                            [ModName, list_to_atom(ModName)]),
                 format_msg("\n\n(defun ~p() \n   (interactive)\n   (apply-composite-"
                            "refac '~p))",
                            [M, M])}
        end,
    AddMenuItem ++ NewFun.

get_wrangler_dir(emacs, _WranglerInstDir) ->                      
    code:lib_dir(wrangler);
get_wrangler_dir(windows, WranglerInstDir) -> 
    code:lib_dir(WranglerInstDir).


format_msg(Temp, Args) ->
    lists:flatten(io_lib:format(Temp, Args)).
