-module(hypervisors_basic).
-include_lib("eunit/include/eunit.hrl").

-behavior(riak_test).
-export([confirm/0]).

-define(HV1, <<"H1">>).
-define(HOST1, <<"127.0.0.1">>).
-define(PORT1, 4200).

-define(HV2, <<"H2">>).
-define(HOST2, <<"127.0.0.2">>).
-define(PORT2, 4201).



confirm() ->
    lager:info("Deploy node to test command line"),
    [Node] = rt:deploy_nodes(1),
    ?assertEqual(ok, rt:wait_until_nodes_ready([Node])),

    %% We should have no hypervisors registered
    list_test(Node, []),

    %% Register the first hypervisor and see if it's listed
    %% and readable
    ?assertEqual(ok, rt_sniffle:hypervisor_register(Node, ?HV1, ?HOST1, ?PORT1)),
    list_test(Node, [?HV1]),
    ?assertEqual({ok,[{<<"host">>,?HOST1},
                      {<<"name">>,?HV1},
                      {<<"port">>,?PORT1},
                      {<<"version">>,<<"0.1.0">>}]}
                 ,rt_sniffle:hypervisor_get(Node, ?HV1)),

    %% Set some values and test of they stick
    ?assertEqual(ok, rt_sniffle:hypervisor_set(Node, ?HV1, <<"key">>, 1)),
    ?assertEqual({ok,[{<<"host">>,?HOST1},
                      {<<"key">>, 1},
                      {<<"name">>,?HV1},
                      {<<"port">>,?PORT1},
                      {<<"version">>,<<"0.1.0">>}]}
                 ,rt_sniffle:hypervisor_get(Node, ?HV1)),

    %% Now check if updates and multipe key writes work
    ?assertEqual(ok, rt_sniffle:hypervisor_set(Node, ?HV1, [{<<"key">>, 2}])),
    ?assertEqual(ok, rt_sniffle:hypervisor_set(Node, ?HV1, [{<<"key1">>, 1}, {<<"key2">>, 2}])),

    ?assertEqual({ok,[{<<"host">>,<<"127.0.0.1">>},
                      {<<"key">>,2},
                      {<<"key1">>,1},
                      {<<"key2">>,2},
                      {<<"name">>,<<"H1">>},
                      {<<"port">>,4200},
                      {<<"version">>,<<"0.1.0">>}]}
                 ,rt_sniffle:hypervisor_get(Node, ?HV1)),

    %% Register a second node and check if it's listable then
    %% delete it again and check if it's done.
    ?assertEqual(ok, rt_sniffle:hypervisor_register(Node, ?HV2, ?HOST2, ?PORT2)),

    list_test(Node, [?HV1, ?HV2]),

    ?assertEqual(ok, rt_sniffle:hypervisor_unregister(Node, ?HV2)),
    list_test(Node, [?HV1]),

    ?assertEqual(not_found, rt_sniffle:hypervisor_get(Node, ?HV2)),

    pass.

list_test(Node, List) ->
    {ok, R} = rt_sniffle:hypervisor_list(Node),
    ?assertEqual(lists:sort(List), lists:sort(R)),
    ok.
