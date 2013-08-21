-module(verify_coverage_with_down_nodes).
-include_lib("eunit/include/eunit.hrl").

-behavior(riak_test).
-export([confirm/0]).

-define(NET1, <<"network1">>).

confirm() ->
    lager:info("Deploying 4 nodes"),
    %% handoff_concurrency needs to be raised to make the leave operation faster.
    %% most clusters go up to 10, but this one is one louder, isn't it?
    [Node1, Node2, Node3, Node4] = Nodes = rt:deploy_nodes(4),

    %% Ensure each node owns 100% of it's own ring
    lager:info("Ensure each nodes 100% of it's own ring"),

    [rt:wait_until_owners_according_to(Node, [Node]) || Node <- Nodes],

    lager:info("Loading some data up in this cluster."),
    {ok, UUID1} = rt_sniffle:network_create(Node1, ?NET1),

    ?assertEqual({ok, [{<<"name">>, ?NET1},
                       {<<"uuid">>, UUID1},
                       {<<"version">>,<<"0.1.0">>}]},
                 rt_sniffle:network_get(Node1, UUID1)),

    lager:info("joining Node 2 to the cluster... It takes two to make a thing go right"),
    rt:join(Node2, Node1),
    wait_and_validate([Node1, Node2], UUID1),

    lager:info("joining Node 3 to the cluster"),
    rt:join(Node3, Node1),
    wait_and_validate([Node1, Node2, Node3], UUID1),

    lager:info("joining Node 4 to the cluster"),
    rt:join(Node4, Node1),
    wait_and_validate(Nodes, UUID1),

    lager:info("Creating 99 more users."),
    [ rt_sniffle:network_create(Node1, <<"network", Nb/binary>>)
      || Nb <- [list_to_binary(integer_to_list(N))
                || N <- lists:seq(2, 100)]],
    {ok, Users1} = rt_sniffle:network_list(Node1),
    ?assertEqual(100, length(Users1)),


    lager:info("Simulate node failure and check if everything is still working."),
    rt:stop(Node3),
    ?assertEqual(ok, rt:wait_until_unpingable(Node3)),
    {ok, Users2} = rt_sniffle:network_list(Node1),
    ?assertEqual(100, length(Users2)),

    pass.

wait_and_validate(Nodes, UUID1) ->
    wait_and_validate(Nodes, Nodes, UUID1).

wait_and_validate(RingNodes, UpNodes, UUID1) ->
    lager:info("Wait until all nodes are ready and there are no pending changes"),
    ?assertEqual(ok, rt:wait_until_nodes_ready(UpNodes)),
    ?assertEqual(ok, rt:wait_until_all_members(UpNodes)),
    ?assertEqual(ok, rt:wait_until_no_pending_changes(UpNodes)),
    lager:info("Ensure each node owns a portion of the ring"),
    [rt:wait_until_owners_according_to(Node, RingNodes) || Node <- UpNodes],
    [rt:wait_for_service(Node, rt:config(rc_services, [riak_kv])) || Node <- UpNodes],
    lager:info("Verify that you got much data... (this is how we do it)"),
    [?assertEqual({ok, [{<<"name">>, ?NET1},
                        {<<"uuid">>, UUID1},
                        {<<"version">>,<<"0.1.0">>}]},
                  rt_sniffle:network_get(Node, UUID1)) || Node <- UpNodes],
    done.


