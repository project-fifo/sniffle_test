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
    ?assertEqual(ok, rt_sniffle:hypervisor_set(Node, ?HV1, <<"alias">>, ?HV1)),
    list_test(Node, [?HV1]),
    ?assertEqual({ok,hv([])}
                ,rt_sniffle:hypervisor_get(Node, ?HV1)),

    %% Set some values and test of they stick
    ?assertEqual(ok, rt_sniffle:hypervisor_set(Node, ?HV1, [<<"metadata">>, <<"key">>], 1)),
    ?assertEqual({ok,hv([{<<"key">>, 1}])}
                ,rt_sniffle:hypervisor_get(Node, ?HV1)),

    %% Now check if updates and multipe key writes work
    ?assertEqual(ok, rt_sniffle:hypervisor_set(Node, ?HV1, [{[<<"metadata">>, <<"key">>], 2}])),
    ?assertEqual(ok, rt_sniffle:hypervisor_set(
                       Node, ?HV1,
                       [
                        {[<<"metadata">>, <<"key1">>], 1},
                        {[<<"metadata">>, <<"key2">>], 2},
                        {[<<"metadata">>, <<"key3">>], 3},
                        {[<<"metadata">>, <<"key4">>], 4},
                        {[<<"metadata">>, <<"key5">>], 5},
                        {[<<"metadata">>, <<"key6">>], 6},
                        {[<<"metadata">>, <<"key7">>], 7},
                        {[<<"metadata">>, <<"key8">>], 8}
                       ])),

    ?assertEqual({ok,hv([{<<"key">>,  2},
                         {<<"key1">>, 1},
                         {<<"key2">>, 2},
                         {<<"key3">>, 3},
                         {<<"key4">>, 4},
                         {<<"key5">>, 5},
                         {<<"key6">>, 6},
                         {<<"key7">>, 7},
                         {<<"key8">>, 8}])}
                ,rt_sniffle:hypervisor_get(Node, ?HV1)),


    %% Cehck if updaring mgets works

    ?assertEqual(ok, rt_sniffle:hypervisor_set(
                       Node, ?HV1,
                       [
                        {[<<"metadata">>, <<"key1">>], 11},
                        {[<<"metadata">>, <<"key2">>], 12},
                        {[<<"metadata">>, <<"key3">>], 13},
                        {[<<"metadata">>, <<"key4">>], 14},
                        {[<<"metadata">>, <<"key5">>], 15},
                        {[<<"metadata">>, <<"key6">>], 16},
                        {[<<"metadata">>, <<"key7">>], 17},
                        {[<<"metadata">>, <<"key8">>], 18}
                       ])),

    ?assertEqual({ok,hv([{<<"key">>,  2},
                         {<<"key1">>, 11},
                         {<<"key2">>, 12},
                         {<<"key3">>, 13},
                         {<<"key4">>, 14},
                         {<<"key5">>, 15},
                         {<<"key6">>, 16},
                         {<<"key7">>, 17},
                         {<<"key8">>, 18}])}
                ,rt_sniffle:hypervisor_get(Node, ?HV1)),

    %% Register a second node and check if it's listable then
    %% delete it again and check if it's done.
    ?assertEqual(ok, rt_sniffle:hypervisor_register(Node, ?HV2, ?HOST2, ?PORT2)),
    ?assertEqual(ok, rt_sniffle:hypervisor_set(Node, ?HV2, <<"alias">>, ?HV2)),

    list_test(Node, [?HV1, ?HV2]),

    ?assertEqual(ok, rt_sniffle:hypervisor_unregister(Node, ?HV2)),
    list_test(Node, [?HV1]),

    ?assertEqual(not_found, rt_sniffle:hypervisor_get(Node, ?HV2)),

    pass.

list_test(Node, List) ->
    {ok, R} = rt_sniffle:hypervisor_list(Node),
    ?assertEqual(lists:sort(List), lists:sort(R)),
    ok.

hv(M) ->
    [{<<"alias">>,<<"H1">>},
     {<<"characteristics">>,[]},
     {<<"etherstubs">>,<<>>},
     {<<"host">>,<<"127.0.0.1">>},
     {<<"metadata">>,M},
     {<<"networks">>,<<>>},
     {<<"path">>,[[{<<"cost">>,1},{<<"name">>,<<"H1">>}]]},
     {<<"pools">>,[]},
     {<<"port">>,4200},
     {<<"resources">>,[]},
     {<<"services">>,[]},
     {<<"sysinfo">>,<<>>},
     {<<"uuid">>,<<"H1">>},
     {<<"version">>,<<>>},
     {<<"virtualisation">>,<<>>}].
