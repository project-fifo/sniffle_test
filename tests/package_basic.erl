-module(package_basic).
-include_lib("eunit/include/eunit.hrl").

-behavior(riak_test).
-export([confirm/0]).

-define(Name1, <<"TEST1">>).
-define(Name2, <<"TEST2">>).

confirm() ->
    lager:info("Deploy node to test command line"),
    [Node] = rt:deploy_nodes(1),
    ?assertEqual(ok, rt:wait_until_nodes_ready([Node])),
    ?assertEqual({ok, []}, rt_sniffle:package_list(Node)),
    {ok, UUID1} = rt_sniffle:package_create(Node, ?Name1),

    ?assertEqual(duplicate, rt_sniffle:package_create(Node, ?Name1)),

    list_test(Node, [UUID1]),
    ?assertEqual({ok,[{<<"name">>,?Name1},
                      {<<"uuid">>, UUID1},
                      {<<"version">>,<<"0.1.0">>}]},
                 rt_sniffle:package_get(Node, UUID1)),
    ?assertEqual(ok, rt_sniffle:package_set(Node, UUID1, <<"key">>, 1)),
    ?assertEqual({ok, [{<<"key">>, 1},
                       {<<"name">>, ?Name1},
                       {<<"uuid">>, UUID1},
                       {<<"version">>,<<"0.1.0">>}]},
                 rt_sniffle:package_get(Node, UUID1)),
    ?assertEqual(ok, rt_sniffle:package_set(Node, UUID1, [{<<"key">>, 2}])),
    ?assertEqual(ok, rt_sniffle:package_set(Node, UUID1, [{<<"key1">>, 1}, {<<"key2">>, 2}])),
    ?assertEqual({ok, [{<<"key">>, 2},
                       {<<"key1">>, 1},
                       {<<"key2">>, 2},
                       {<<"name">>,?Name1},
                       {<<"uuid">>, UUID1},
                       {<<"version">>,<<"0.1.0">>}]},
                 rt_sniffle:package_get(Node, UUID1)),
    {ok, UUID2} = rt_sniffle:package_create(Node, ?Name2),
    list_test(Node, [UUID1, UUID2]),
    ?assertEqual(ok, rt_sniffle:package_delete(Node, UUID2)),
    list_test(Node, [UUID1]),
    ?assertEqual(not_found, rt_sniffle:package_get(Node, UUID2)),
    pass.


list_test(Node, List) ->
    {ok, R} = rt_sniffle:package_list(Node),
    ?assertEqual(lists:sort(List), lists:sort(R)),
    ok.
