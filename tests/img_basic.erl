-module(img_basic).
-include_lib("eunit/include/eunit.hrl").

-behavior(riak_test).
-export([confirm/0]).

-define(UUID1, <<"TEST1">>).
-define(UUID2, <<"TEST2">>).

%%
%% This test runs through creating, modyfing and deleting a VM
%%

confirm() ->
    lager:info("Deploy node to test command line"),
    [Node] = rt:deploy_nodes(1),
    ?assertEqual(ok, rt:wait_until_nodes_ready([Node])),
    ?assertEqual({ok, []}, rt_sniffle:img_list(Node)),
    ?assertEqual({ok, undefined}, rt_sniffle:img_create(Node, ?UUID1, 0, <<"bla">>)),
    list_test(Node, [?UUID1]),
    list_test(Node, ?UUID1, [0]),
    ?assertEqual({ok, undefined}, rt_sniffle:img_create(Node, ?UUID1, 1, <<"blubb">>)),
    list_test(Node, ?UUID1, [0, 1]),
    ?assertEqual({ok, <<"blubb">>}, rt_sniffle:img_get(Node, ?UUID1, 1)),
    ?assertEqual({ok, undefined}, rt_sniffle:img_create(Node, ?UUID2, 0, <<"bla">>)),
    list_test(Node, [?UUID1, ?UUID2]),
    ?assertEqual(ok, rt_sniffle:img_delete(Node, ?UUID2, 0)),
    list_test(Node, [?UUID1]),
    ?assertEqual(not_found, rt_sniffle:img_get(Node, ?UUID2, 0)),
    pass.

list_test(Node, List) ->
    {ok, R} = rt_sniffle:img_list(Node),
    ?assertEqual(lists:sort(List), lists:sort(R)),
    ok.

list_test(Node, ID, List) ->
    {ok, R} = rt_sniffle:img_list(Node, ID),
    ?assertEqual(lists:sort(List), lists:sort(R)),
    ok.
