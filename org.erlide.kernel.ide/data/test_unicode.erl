%% coding: utf-8
-module(test_unicode).
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
    "為人謀而不忠乎",
    $為,
    ok.

stop()->
    ok.

