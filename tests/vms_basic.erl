-module(vms_basic).
-include_lib("eunit/include/eunit.hrl").

-behavior(riak_test).
-export([confirm/0,
         full_test/1,
         register_test/2]).

-define(UUID1, <<"TEST1">>).
-define(UUID2, <<"TEST2">>).

-define(HV, <<"HYPERVISOR">>).

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
    ?assertEqual({ok, []}, rt_sniffle:vm_list(Node)),
    register_test(Node, ?UUID1),
    list_test(Node, [?UUID1]),
    ?assertEqual({ok, [{<<"hypervisor">>,?HV},{<<"uuid">>,?UUID1}]},rt_sniffle:vm_get(Node, ?UUID1)),
    ?assertEqual(ok, rt_sniffle:vm_set(Node, ?UUID1, <<"key">>, 1)),
    ?assertEqual({ok, [{<<"hypervisor">>,?HV},
                       {<<"key">>, 1},
                       {<<"uuid">>,?UUID1}]},
                 rt_sniffle:vm_get(Node, ?UUID1)),
    ?assertEqual(ok, rt_sniffle:vm_set(Node, ?UUID1, [{<<"key">>, 2}])),
    ?assertEqual(ok, rt_sniffle:vm_set(Node, ?UUID1, [{<<"key1">>, 1}, {<<"key2">>, 2}])),
    ?assertEqual({ok, [{<<"hypervisor">>,?HV},
                       {<<"key">>, 2},
                       {<<"key1">>, 1},
                       {<<"key2">>, 2},
                       {<<"uuid">>,?UUID1}]},
                 rt_sniffle:vm_get(Node, ?UUID1)),
    register_test(Node, ?UUID2),
    list_test(Node, [?UUID1, ?UUID2]),
    unregister_test(Node, ?UUID2),
    list_test(Node, [?UUID1]).

register_test(Node, UUID) ->
    ?assertEqual(ok, rt_sniffle:vm_register(Node, UUID, ?HV)),
    ok.

unregister_test(Node, UUID) ->
    ?assertEqual(ok, rt_sniffle:vm_unregister(Node, UUID)),
    ok.

list_test(Node, List) ->
    {ok, R} = rt_sniffle:vm_list(Node),
    ?assertEqual(lists:sort(List), lists:sort(R)),
    ok.
