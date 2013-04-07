-module(vms_cluster).
-include_lib("eunit/include/eunit.hrl").

-behavior(riak_test).
-export([confirm/0]).


confirm() ->
    lager:info("Deploy node to test command line"),
    [Node1, Node2, Node3] = rt:deploy_nodes(3),
    ?assertEqual(ok, rt:wait_until_nodes_ready([Node1, Node2, Node3])),
    vms_basic:full_test(Node1),
    pass.
