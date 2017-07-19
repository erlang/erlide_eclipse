%% coding: utf-8
-module(test_unicode).
-compile(export_all).

%%
%% 人不知而不慍，不亦君子乎？」
%%
%% 生むぎ　生ごめ　生たまご
%%
%% советских военных судов и самолетов была
%%
%% Աեցեհի իմ լավ ?ւղիե լավարար,
%%
%% Yukarda mavi gök, asağıda yağız yer yaratıldıkta
%%
start("謀", $為) ->
    S = "為人謀而不忠乎",
    C = $為,
    [S, C].

stop()->
    ok.

