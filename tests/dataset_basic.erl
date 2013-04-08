-module(dataset_basic).
-include_lib("eunit/include/eunit.hrl").

-behavior(riak_test).
-export([confirm/0,
         full_test/1]).

-define(UUID1, <<"TEST1">>).
-define(UUID2, <<"TEST2">>).

confirm() ->
    lager:info("Deploy node to test command line"),
    [Node] = rt:deploy_nodes(1),
    ?assertEqual(ok, rt:wait_until_nodes_ready([Node])),
    full_test(Node),
    pass.


%%
%% This test runs through creating, modyfing and deleting a VM
%%
full_test(Node) ->
    ?assertEqual({ok, []}, rt_sniffle:dataset_list(Node)),
    create_test(Node, ?UUID1),
    list_test(Node, [?UUID1]),
    ?assertEqual({ok,[{<<"name">>,?UUID1},
                      {<<"version">>,<<"0.1.0">>}]},
                 rt_sniffle:dataset_get(Node, ?UUID1)),
    ?assertEqual(ok, rt_sniffle:dataset_set(Node, ?UUID1, <<"key">>, 1)),
    ?assertEqual({ok, [{<<"key">>, 1},
                       {<<"name">>,?UUID1},
                       {<<"version">>,<<"0.1.0">>}]},
                 rt_sniffle:dataset_get(Node, ?UUID1)),
    ?assertEqual(ok, rt_sniffle:dataset_set(Node, ?UUID1, [{<<"key">>, 2}])),
    ?assertEqual(ok, rt_sniffle:dataset_set(Node, ?UUID1, [{<<"key1">>, 1}, {<<"key2">>, 2}])),
    ?assertEqual({ok, [{<<"key">>, 2},
                       {<<"key1">>, 1},
                       {<<"key2">>, 2},
                       {<<"name">>,?UUID1},
                       {<<"version">>,<<"0.1.0">>}]},
                 rt_sniffle:dataset_get(Node, ?UUID1)),
    create_test(Node, ?UUID2),
    list_test(Node, [?UUID1, ?UUID2]),
    ?assertEqual(ok, rt_sniffle:dataset_delete(Node, ?UUID1)),
    list_test(Node, [?UUID1]).

create_test(Node, UUID) ->
    ?assertEqual(ok, rt_sniffle:dataset_create(Node, UUID)),
    ok.

list_test(Node, List) ->
    {ok, R} = rt_sniffle:dataset_list(Node),
    ?assertEqual(lists:sort(List), lists:sort(R)),
    ok.
