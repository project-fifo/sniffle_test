-module(dataset_failed_get).
-include_lib("eunit/include/eunit.hrl").

-behavior(riak_test).
-export([confirm/0]).

confirm() ->
    lager:info("Deploy node to test command line"),
    [Node] = rt:deploy_nodes(1),
    ?assertEqual(ok, rt:wait_until_nodes_ready([Node])),
    rt_intercept:add(Node, {hackney, [{{request, 5}, request_404}]}),
    ?assertEqual({error,404}, rt_sniffle:dataset_import(Node, <<"http://127.0.0.1/">>)),
    ?assertEqual({ok, []}, rt_sniffle:dataset_list(Node)),
    pass.
