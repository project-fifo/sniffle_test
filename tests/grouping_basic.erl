-module(grouping_basic).
-include_lib("eunit/include/eunit.hrl").

-behavior(riak_test).
-export([confirm/0]).

-define(CLUSTER1, <<"C1">>).
-define(CLUSTER2, <<"C2">>).
-define(STACK1, <<"S1">>).

confirm() ->
    lager:info("Deploy node to test command line"),
    [Node] = rt:deploy_nodes(1),
    ?assertEqual(ok, rt:wait_until_nodes_ready([Node])),

    %% We should have no hypervisors registered
    list_test(Node, []),

    %% We create two custers and one stack, all should be empty after this
    {ok, UUID1} = rt_sniffle:grouping_add(Node, ?CLUSTER1, cluster),
    {ok, UUID2} = rt_sniffle:grouping_add(Node, ?CLUSTER2, cluster),
    {ok, UUID3} = rt_sniffle:grouping_add(Node, ?STACK1, stack),
    list_test(Node, [UUID1, UUID2, UUID3]),
    ?assertEqual({ok, cl(UUID1, ?CLUSTER1)},
                 rt_sniffle:grouping_get(Node, UUID1)),
    ?assertEqual({ok, cl(UUID2, ?CLUSTER2)},
                 rt_sniffle:grouping_get(Node, UUID2)),
    ?assertEqual({ok, st(UUID3, ?STACK1)},
                 rt_sniffle:grouping_get(Node, UUID3)),

    %% We add a cluster to the stack now stack and clsuter both should know
    %% eachtoer.
    ?assertEqual(ok,
                 rt_sniffle:grouping_add_element(Node, UUID3, UUID1)),
    ?assertEqual({ok, st(UUID3, ?STACK1, [], [UUID1])},
                 rt_sniffle:grouping_get(Node, UUID3)),
    ?assertEqual({ok, cl(UUID1, ?CLUSTER1, [UUID3], [])},
                 rt_sniffle:grouping_get(Node, UUID1)),

    %% We the stack to the second clsuter. Now both clsuters should know
    %% thes tack.
    ?assertEqual(ok,
                 rt_sniffle:grouping_add_grouping(Node, UUID2, UUID3)),
    ?assertEqual({ok, st(UUID3, ?STACK1, [], [UUID1, UUID2])},
                 rt_sniffle:grouping_get(Node, UUID3)),
    ?assertEqual({ok, cl(UUID1, ?CLUSTER1, [UUID3], [])},
                 rt_sniffle:grouping_get(Node, UUID1)),
    ?assertEqual({ok, cl(UUID2, ?CLUSTER2, [UUID3], [])},
                 rt_sniffle:grouping_get(Node, UUID2)),

    %% We delete one of the clusters and the stack should have have it removed.
    ?assertEqual(ok,
                 rt_sniffle:grouping_delete(Node, UUID1)),
    ?assertEqual({ok, st(UUID3, ?STACK1, [], [UUID2])},
                 rt_sniffle:grouping_get(Node, UUID3)),
    ?assertEqual(not_found,
                 rt_sniffle:grouping_get(Node, UUID1)),
    ?assertEqual({ok, cl(UUID2, ?CLUSTER2, [UUID3], [])},
                 rt_sniffle:grouping_get(Node, UUID2)),
    list_test(Node, [UUID2, UUID3]),


    %% Now we remove the second cluster form the stack and both
    %% should know it
    ?assertEqual(ok,
                 rt_sniffle:grouping_remove_element(Node, UUID3, UUID2)),
    ?assertEqual({ok, cl(UUID2, ?CLUSTER2)},
                 rt_sniffle:grouping_get(Node, UUID2)),
    ?assertEqual({ok, st(UUID3, ?STACK1)},
                 rt_sniffle:grouping_get(Node, UUID3)),

    %% Add the existinc cluster back and make sure deleting the stack will
    %% remove the cluster
    ?assertEqual(ok,
                 rt_sniffle:grouping_add_grouping(Node, UUID2, UUID3)),
    ?assertEqual({ok, st(UUID3, ?STACK1, [], [UUID2])},
                 rt_sniffle:grouping_get(Node, UUID3)),
    ?assertEqual({ok, cl(UUID2, ?CLUSTER2, [UUID3], [])},
                 rt_sniffle:grouping_get(Node, UUID2)),
    ?assertEqual(ok,
                 rt_sniffle:grouping_delete(Node, UUID3)),
    ?assertEqual(not_found,
                 rt_sniffle:grouping_get(Node, UUID3)),
    ?assertEqual(not_found,
                 rt_sniffle:grouping_get(Node, UUID1)),
    ?assertEqual({ok, cl(UUID2, ?CLUSTER2)},
                 rt_sniffle:grouping_get(Node, UUID2)),
    list_test(Node, [UUID2]),


    pass.

list_test(Node, List) ->
    {ok, R} = rt_sniffle:grouping_list(Node),
    ?assertEqual(lists:sort(List), lists:sort(R)),
    ok.

cl(UUID, Name) ->
    cl(UUID, Name, [], []).

cl(UUID, Name, Gs, Es) ->
    g(UUID, Name, <<"cluster">>, Gs, Es).

st(UUID, Name) ->
    st(UUID, Name, [], []).

st(UUID, Name, Gs, Es) ->
    g(UUID, Name, <<"stack">>, Gs, Es).

g(UUID, Name, Type, Gs, Es) ->
    [{<<"elements">>, lists:sort(Es)},
     {<<"groupings">>,lists:sort(Gs)},
     {<<"metadata">>,[]},
     {<<"name">>, Name},
     {<<"type">>, Type},
     {<<"uuid">>, UUID}].
