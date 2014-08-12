-module(erlide_pde_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

get_plugin_version_test_() ->
    [
     ?_assertEqual("0.0.0", erlide_pde:get_plugin_version(<<"foo\nBundlxe-Version: 1.2.3\nbar\n">>)),
     ?_assertEqual("0.0.0", erlide_pde:get_plugin_version(<<"foo\nBundle-Version: 1.2q.3\nbar\n">>)),
     ?_assertEqual("1.2.3", erlide_pde:get_plugin_version(<<"foo\nBundle-Version: 1.2.3\nbar\n">>)),
     ?_assertEqual("32.12.11", erlide_pde:get_plugin_version(<<"foo\nBundle-Version: 32.12.11.qualifier\nbar\n">>))
     ].
