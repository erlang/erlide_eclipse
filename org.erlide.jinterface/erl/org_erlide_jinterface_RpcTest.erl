-module(org_erlide_jinterface_RpcTest).
-compile(export_all).

  'new'(P0) when is_list(P0) ->
    jrpc:call(<<"org.erlide.jinterface.RpcTest">>, {<<"org.erlide.jinterface.RpcTest">>, [<<"java.lang.String">>]}, [P0]).

  'new'()  ->
    jrpc:call(<<"org.erlide.jinterface.RpcTest">>, {<<"org.erlide.jinterface.RpcTest">>, []}, []).

  'square'(Obj, P0) when is_integer(P0) ->
    %%  returns int
    jrpc:call(Obj, {<<"square">>, [<<"int">>]}, [P0]).

  'test_str'(P0) when is_list(P0) ->
    %%  returns java.lang.String
    jrpc:call(<<"org.erlide.jinterface.RpcTest">>, {<<"test_str">>, [<<"java.lang.String">>]}, [P0]).

  'notifyAll'(Obj)  ->
    jrpc:cast(Obj, {<<"notifyAll">>, []}, []).

  'wait'(Obj)  ->
    jrpc:cast(Obj, {<<"wait">>, []}, []).

  'toString'(Obj)  ->
    %%  returns java.lang.String
    jrpc:call(Obj, {<<"toString">>, []}, []).

  'getClass'(Obj)  ->
    %%  returns java.lang.Class
    jrpc:call(Obj, {<<"getClass">>, []}, []).

  'equals'(Obj, P0)  ->
    %%  returns boolean
    jrpc:call(Obj, {<<"equals">>, [<<"java.lang.Object">>]}, [P0]).

  'test_str_arr'(P0)  ->
    %%  returns java.lang.String
    jrpc:call(<<"org.erlide.jinterface.RpcTest">>, {<<"test_str_arr">>, [<<"[Ljava.lang.String;">>]}, [P0]).

  'test_int'(P0) when is_integer(P0) ->
    %%  returns int
    jrpc:call(<<"org.erlide.jinterface.RpcTest">>, {<<"test_int">>, [<<"int">>]}, [P0]).

  'wait'(Obj, P0, P1) when is_integer(P0), is_integer(P1) ->
    jrpc:cast(Obj, {<<"wait">>, [<<"long">>, <<"int">>]}, [P0, P1]).

  'hashCode'(Obj)  ->
    %%  returns int
    jrpc:call(Obj, {<<"hashCode">>, []}, []).

  'test_int_arr'(P0)  ->
    {error, not_supported}.

  'notify'(Obj)  ->
    jrpc:cast(Obj, {<<"notify">>, []}, []).

  'wait'(Obj, P0) when is_integer(P0) ->
    jrpc:cast(Obj, {<<"wait">>, [<<"long">>]}, [P0]).

