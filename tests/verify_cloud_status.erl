-module(verify_cloud_status).
-include_lib("eunit/include/eunit.hrl").

-behavior(riak_test).
-export([confirm/0]).

-define(UUID1, <<"TEST1">>).
-define(UUID2, <<"TEST2">>).

-define(HV, <<"HYPERVISOR">>).
-define(HOST, <<"127.0.0.1">>).
-define(PORT, 4200).

confirm() ->
    [Node] = rt:deploy_nodes(1),
    ?assertEqual(ok, rt:wait_until_nodes_ready([Node])),
    ?assertEqual(ok, rt_sniffle:hypervisor_register(Node, ?HV, ?HOST, ?PORT)),
    ?assertEqual({ok,[?HV]}, rt_sniffle:hypervisor_list(Node)),
    rt_intercept:add(Node, {libchunter, [{{ping,2}, ping_ok}]}),
    ?assertEqual({ok,{[{<<"hypervisors">>,[?HV]}],
                      []}}, rt_sniffle:cloud_status(Node)),

    pass.
