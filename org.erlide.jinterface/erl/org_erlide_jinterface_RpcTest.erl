-module(org_erlide_jinterface_RpcTest).
-compile(export_all).

  'new'(P0)  ->
    jrpc:call(<<"org.erlide.jinterface.RpcTest">>, {<<"org.erlide.jinterface.RpcTest">>, [<<"java.lang.String">>]}, [P0]).

  'new'()  ->
    jrpc:call(<<"org.erlide.jinterface.RpcTest">>, {<<"org.erlide.jinterface.RpcTest">>, []}, []).

  'square'(Obj, P0) when is_reference(Obj) ->
    %%  returns int
    jrpc:call(Obj, {<<"square">>, [<<"int">>]}, [P0]).

  'test_str'(P0) ->
    %%  returns java.lang.String
    jrpc:call(<<"org.erlide.jinterface.RpcTest">>, {<<"test_str">>, [<<"java.lang.String">>]}, [P0]).

  'notifyAll'(Obj) when is_reference(Obj) ->
    jrpc:cast(Obj, {<<"notifyAll">>, []}, []).

  'wait'(Obj) when is_reference(Obj) ->
    jrpc:cast(Obj, {<<"wait">>, []}, []).

  'toString'(Obj) when is_reference(Obj) ->
    %%  returns java.lang.String
    jrpc:call(Obj, {<<"toString">>, []}, []).

  'getClass'(Obj) when is_reference(Obj) ->
    %%  returns java.lang.Class
    jrpc:call(Obj, {<<"getClass">>, []}, []).

  'equals'(Obj, P0) when is_reference(Obj) ->
    %%  returns boolean
    jrpc:call(Obj, {<<"equals">>, [<<"java.lang.Object">>]}, [P0]).

  'test_str_arr'(P0) ->
    %%  returns java.lang.String
    jrpc:call(<<"org.erlide.jinterface.RpcTest">>, {<<"test_str_arr">>, [<<"[Ljava.lang.String;">>]}, [P0]).

  'test_int'(P0) ->
    %%  returns int
    jrpc:call(<<"org.erlide.jinterface.RpcTest">>, {<<"test_int">>, [<<"int">>]}, [P0]).

  'wait'(Obj, P0, P1) when is_reference(Obj) ->
    jrpc:cast(Obj, {<<"wait">>, [<<"long">>, <<"int">>]}, [P0, P1]).

  'hashCode'(Obj) when is_reference(Obj) ->
    %%  returns int
    jrpc:call(Obj, {<<"hashCode">>, []}, []).

  'test_int_arr'(P0) ->
    {error, not_supported}.

  'notify'(Obj) when is_reference(Obj) ->
    jrpc:cast(Obj, {<<"notify">>, []}, []).

  'wait'(Obj, P0) when is_reference(Obj) ->
    jrpc:cast(Obj, {<<"wait">>, [<<"long">>]}, [P0]).

