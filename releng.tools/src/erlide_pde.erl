-module(erlide_pde).

-export([
         get_plugin_version/1
        ]).

get_plugin_version(Filepath) when is_list(Filepath) ->
    {ok, Binary} = file:read_file(Filepath),
    get_plugin_version(Binary);
get_plugin_version(Binary) when is_binary(Binary) ->
    String = binary_to_list(Binary),
    case re:run(String, "Bundle-Version: ([0-9]+\\.[0-9]+\\.[0-9]+)") of
        {match, [_, {From, To}]} ->
            string:substr(String, From+1, To);
        _ ->
            "0.0.0"
    end.
