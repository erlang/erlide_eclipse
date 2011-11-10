%% @hidden
%% @private
-module(wrangler_write_file).

-export([write_refactored_files/5,write_refactored_files/4,write_refactored_files_for_preview/3]).

-include("../include/wrangler_internal.hrl").

-include_lib("kernel/include/file.hrl"). 
%% =====================================================================
%% @doc Pretty-print the abstract syntax trees to a files, and add the previous 
%% version to history for undo purpose. <code>Files</code> is a list of three element 
%% tuples. The first element in the tuple is the original file name, the second element 
%% is the new file name if the filename has been changed by the refactoring, otherwise it 
%% should be the same as the first element, and the third element in the tuple is the 
%% AST represention of the file.

write_refactored_files(Results, HasWarningMsg, Editor, TabWidth, Cmd) ->
    case Editor of
	emacs ->
	    write_refactored_files_emacs(Results, HasWarningMsg, TabWidth, Cmd);
	eclipse ->
	    write_refactored_files_eclipse(Results, TabWidth);
	command ->
	    write_refactored_files_command_line(Results, TabWidth);
        composite_emacs ->
            write_refactored_files_composite_emacs(Results, TabWidth)
    end.

write_refactored_files(Results, Editor, TabWidth, Cmd) ->
    case Editor of
	emacs ->
	    write_refactored_files_emacs(Results, TabWidth, Cmd);
	eclipse ->
	    write_refactored_files_eclipse(Results, TabWidth);
	command ->
	    write_refactored_files_command_line(Results, TabWidth);
        composite_emacs ->
            write_refactored_files_composite_emacs(Results, TabWidth)
    end.

write_refactored_files_emacs(Results, TabWidth, Cmd) ->
    write_refactored_files_for_preview(Results,TabWidth,Cmd),
    ChangedFiles = lists:map(fun ({FileInfo,_AST}) -> 
				     element(1, FileInfo)
			     end,Results),
    output_msg(ChangedFiles),
    {ok,ChangedFiles}.

write_refactored_files_emacs(Results, HasWarningMsg, TabWidth, Cmd) ->
    write_refactored_files_for_preview(Results,TabWidth,Cmd),
    ChangedFiles = lists:map(fun ({FileInfo,_AST}) -> 
				     element(1, FileInfo)
			     end,Results),
    output_msg(ChangedFiles),
    {ok,ChangedFiles, HasWarningMsg}.

output_msg(ChangedFiles) ->
    case ChangedFiles of 
        [] ->
            ?wrangler_io("No file has been changed by this refactoring.\n",[]);
        _ ->
            ?wrangler_io("The following files are to be changed by this refactoring:\n~p\n",
                         [ChangedFiles])
    end.

write_refactored_files_for_preview(Files, TabWidth, LogMsg) ->
    write_refactored_files_for_preview(Files, TabWidth, LogMsg, []).
write_refactored_files_for_preview(Files, TabWidth, LogMsg, _SearchPaths) ->
    F = fun (FileAST) ->
		case FileAST of
		    {{FileName,NewFileName}, AST} ->
			FileFormat = wrangler_misc:file_format(FileName),
                        NewExt = filename:extension(FileName)++".swp",
			SwpFileName = filename:rootname(FileName) ++ NewExt,
                        {Content, Changes} = wrangler_prettypr:print_ast_and_get_changes(FileFormat, AST, TabWidth),
			case file:write_file(SwpFileName, list_to_binary(Content)) of
			    ok ->
                                %% wrangler_ast_server:update_ast({FileName, true, SearchPaths, TabWidth, FileFormat}, SwpFileName),
                                {{{filename:join([FileName]),
                                   filename:join([NewFileName]), false},
                                  filename:join([SwpFileName])}, Changes};
			    {error,Reason} ->
				Msg = io_lib:format("Wrangler could not write to file ~s: ~w \n",
						    [FileName, Reason]),
				throw({error, lists:flatten(Msg)})
			end;
		    {{FileName, NewFileName, IsNew}, AST} ->
			FileFormat = wrangler_misc:file_format(FileName),
                        NewExt = filename:extension(FileName)++".swp",
			SwpFileName = filename:rootname(FileName) ++ NewExt,
                        {Content, Changes} = wrangler_prettypr:print_ast_and_get_changes(FileFormat, AST, TabWidth),
			case file:write_file(SwpFileName, list_to_binary(Content)) of
			    ok ->
                                {{{filename:join([FileName]), filename:join([NewFileName]), IsNew},
				  filename:join([SwpFileName])}, Changes};
			    {error, Reason} ->
				Msg = io_lib:format("Wrangler could not write to directory ~s: ~w \n",
						    [filename:dirname(FileName), Reason]),
				throw({error, lists:flatten(Msg)})
			end 
		end
	end,
    {FilePairs, ListOfNoOfChangedFunsToks} = lists:unzip(lists:map(F, Files)),
    {FinalChangedFuns, FinalToksRemoved, FinalToksAdded} = lists:unzip3(ListOfNoOfChangedFunsToks),
    case lists:any(fun (R) -> R == error end, FilePairs) of
	true -> lists:foreach(fun (P) ->
				      case P of
					  error -> ok;
					  {_,SwpF} -> file:delete(SwpF)
				      end
			      end, FilePairs),
		throw({error, "Wrangler failed to output the refactoring result."});
	_ ->
	    LogMsgAboutChanges = io_lib:format(" Num of files affected: ~p, "
					       "Num of functions/attributes affected: ~p, "
					       "Num of tokens removed: ~p, "
					       "Num of tokens added: ~p.\n",
					       [length(FilePairs), lists:sum(FinalChangedFuns),
						lists:sum(FinalToksRemoved), lists:sum(FinalToksAdded)]),
	    NewLogMsg = LogMsg++ lists:flatten(LogMsgAboutChanges),
	    wrangler_preview_server:add_files({FilePairs, NewLogMsg})
    end.

write_refactored_files_eclipse(Results, TabWidth) ->
    Res = lists:map(fun ({{OldFName,NewFName},AST}) ->
			    FileFormat = wrangler_misc:file_format(OldFName),
			    {OldFName, NewFName,
			     wrangler_prettypr:print_ast(FileFormat,AST,TabWidth)}
		    end,Results),
    {ok,Res}.

write_refactored_files_command_line(Results, TabWidth) ->
    FilesToWrite = [FileTuple || {FileTuple, _} <- Results],
    lists:foreach(fun (FileTuple) ->
			  check_access(FileTuple)
		  end, FilesToWrite),
    backup_files(Results),
    Res =[write_a_file({FileInfo, AST}, TabWidth) || {FileInfo, AST}<-Results],
    {ok, Res}.

write_refactored_files_composite_emacs(Results, TabWidth) ->
    FilesToWrite = [FileTuple || {FileTuple, _} <- Results],
    lists:foreach(fun (FileTuple) ->
			  check_access(FileTuple)
		  end, FilesToWrite),
    wrangler_backup_server:add_to_backups(FilesToWrite),
    Res =[write_a_file({FileInfo, AST}, TabWidth) || {FileInfo, AST}<-Results],
    {ok, Res}.
write_a_file({FileInfo, AST}, TabWidth) ->
    OldFileName = element(1, FileInfo),
    NewFileName = element(2, FileInfo),
    FileFormat = wrangler_misc:file_format(OldFileName),
    Bin = list_to_binary(wrangler_prettypr:print_ast(FileFormat, AST, TabWidth)),
    case file:write_file(OldFileName, Bin) of
        ok when OldFileName == NewFileName ->
            OldFileName;
        ok ->
            case file:rename(OldFileName, NewFileName) of
                ok -> OldFileName;
                {error, Reason} ->
                    Msg = io_lib:format("Wrangler could not rename file ~s: ~w \n",
                                        [OldFileName, Reason]),
                    throw({error, lists:flatten(Msg)})
            end;
        {error, Reason} ->
            Msg = io_lib:format("Wrangler could not write to file ~s: ~w \n",
                                [NewFileName, Reason]),
            throw({error, lists:flatten(Msg)})
    end.
  
backup_files(Results) ->
    F0 = fun ({FileInfo,_AST}) ->
		 case FileInfo of
		     {FileName,NewFileName} ->
			 {ok,Bin} = file:read_file(FileName),
			 {{filename:join([FileName]),
			   filename:join([NewFileName]),false},Bin};
		     {FileName,NewFileName,IsNew} ->
			 {ok,Bin} = file:read_file(FileName),
			 {{filename:join([FileName]),
			   filename:join([NewFileName]),IsNew},Bin}
		 end
	 end,
    FilesToBackup = lists:map(F0,Results),
    wrangler_undo_server:add_to_history({FilesToBackup,"",
					 element(1,hd(FilesToBackup))}).

check_access({OldFileName, NewFileName}) ->
    case OldFileName==NewFileName of 
	true ->
	    case writable(OldFileName) of
		true -> ok;
		false ->
		    Msg = io_lib:format("File ~s is not writeable\n", [OldFileName]),
		    throw({error,lists:flatten(Msg)})
	    end;
	false ->
	    Dir=filename:dirname(NewFileName),
	    case writable(Dir) of
		true ->
		    ok;
		false ->
		    Msg = lists:flatten(io_lib:format("Dirctory ~s is not writeable\n", [Dir])),
		    throw({error, Msg})
	    end
    end;

check_access({OldFileName, NewFileName, IsNew}) ->
    case IsNew of 
	true->
	    check_access({'_', NewFileName});
	false ->
	    check_access({OldFileName, NewFileName})
    end.

writable(FileName) ->
    case file:read_file_info(FileName) of
	{ok, #file_info{access=write}} ->
	    true;
	{ok, #file_info{access=read_write}} ->
	    true;
	_ ->
	    false
    end.
