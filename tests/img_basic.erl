-module(img_basic).
-include_lib("eunit/include/eunit.hrl").

-behavior(riak_test).
-export([confirm/0,
         full_test/1]).

-define(UUID1, <<"TEST1">>).
-define(UUID2, <<"TEST2">>).

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
    ?assertEqual({ok, []}, rt_sniffle:img_list(Node)),
    ?assertEqual(ok, rt_sniffle:img_create(Node, ?UUID1, 0, <<"bla">>)),
    list_test(Node, [?UUID1]),
    list_test(Node, ?UUID1, [0]),
    ?assertEqual(ok, rt_sniffle:img_create(Node, ?UUID1, 1, <<"blubb">>)),
    list_test(Node, ?UUID1, [0, 1]),
    ?assertEqual({ok, <<"blubb">>}, rt_sniffle:img_get(Node, ?UUID1, 1)).

list_test(Node, List) ->
    {ok, R} = rt_sniffle:img_list(Node),
    ?assertEqual(lists:sort(List), lists:sort(R)),
    ok.

list_test(Node, ID, List) ->
    {ok, R} = rt_sniffle:img_list(Node, ID),
    ?assertEqual(lists:sort(List), lists:sort(R)),
    ok.
