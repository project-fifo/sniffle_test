-module(verify_cloud_status).
-include_lib("eunit/include/eunit.hrl").

-behavior(riak_test).
-export([confirm/0]).

-define(UUID1, <<"TEST1">>).
-define(UUID2, <<"TEST2">>).

-define(HV1, <<"HYPERVISOR1">>).
-define(HOST1, <<"127.0.0.1">>).
-define(PORT1, 4200).
-define(HV2, <<"HYPERVISOR2">>).
-define(HOST2, <<"127.0.0.2">>).
-define(PORT2, 4200).

-define(assertOk(A), ?assertEqual(ok, A)).

confirm() ->
    [Node] = rt:deploy_nodes(1),
    ?assertOk(rt:wait_until_nodes_ready([Node])),
    ?assertOk(rt:wait_until_no_pending_changes([Node])),
    ?assertOk(rt:wait_until_transfers_complete([Node])),
    ?assertOk(rt_sniffle:hypervisor_register(Node, ?HV1, ?HOST1, ?PORT1)),
    ?assertOk(rt_sniffle:hypervisor_register(Node, ?HV2, ?HOST2, ?PORT2)),
    {ok, L} = rt_sniffle:hypervisor_list(Node),
    ?assertEqual([?HV1, ?HV2], lists:sort(L)),
    rt_sniffle:hypervisor_set(Node, ?HV1, [<<"resources">>, <<"r">>], 19),
    rt_sniffle:hypervisor_set(Node, ?HV2, [<<"resources">>, <<"r">>], 23),

    rt_intercept:add(Node, {libchunter, [{{ping,2}, ping_ok}]}),
    ?assertEqual({ok,{[{<<"hypervisors">>, [?HV1, ?HV2]},
                       {<<"r">>, 42},
                       {<<"storage">>,<<"internal">>}],
                      []}}, rt_sniffle:cloud_status(Node)),

    rt_intercept:add(Node, {libchunter, [{{ping,2}, ping_fail}]}),
    ?assertEqual({ok,{[{<<"hypervisors">>,[?HV1, ?HV2]},
                       {<<"r">>, 42}],
                      [[{<<"category">>,<<"chunter">>},
                        {<<"element">>,?HV1},
                        {<<"message">>,
                         <<"Chunter server ", ?HV1/binary, " down.">>},
                        {<<"type">>,<<"critical">>}],
                       [{<<"category">>,<<"chunter">>},
                        {<<"element">>,?HV2},
                        {<<"message">>,
                         <<"Chunter server ", ?HV2/binary, " down.">>},
                        {<<"type">>,<<"critical">>}]]}}, rt_sniffle:cloud_status(Node)),
    pass.
