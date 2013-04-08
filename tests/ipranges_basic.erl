-module(ipranges_basic).
-include_lib("eunit/include/eunit.hrl").

-behavior(riak_test).
-export([confirm/0,
         full_test/1]).

-define(NAME, <<"TEST1">>).
-define(NET, rt_sniffle:ip_to_int(<<"10.0.0.0">>)).
-define(GW, rt_sniffle:ip_to_int(<<"10.0.0.1">>)).
-define(MASK, rt_sniffle:ip_to_int(<<"255.255.255.0">>)).
-define(FIRST, rt_sniffle:ip_to_int(<<"10.0.0.100">>)).
-define(LAST, rt_sniffle:ip_to_int(<<"10.0.0.200">>)).
-define(TAG, <<"admni">>).
-define(VLAN, 0).

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
    list_test(Node, []),
    {ok, UUID1} = rt_sniffle:iprange_create(
                    Node, ?NAME,
                    ?NET, ?GW, ?MASK,
                    ?FIRST, ?LAST,
                    ?TAG, ?VLAN),
    list_test(Node, [UUID1]),
    get_test(Node, UUID1, ?FIRST),
    ?assertEqual({ok,{<<"admni">>,
                      ?FIRST,
                      ?MASK,
                      ?GW}},
                 rt_sniffle:iprange_claim(Node, UUID1)),
    get_test(Node, UUID1, ?FIRST + 1),
    ?assertEqual({ok,{<<"admni">>,
                      ?FIRST + 1,
                      ?MASK,
                      ?GW}},
                 rt_sniffle:iprange_claim(Node, UUID1)),
    get_test(Node, UUID1, ?FIRST + 2),
    ?assertEqual(ok,
                 rt_sniffle:iprange_release(Node, UUID1, ?FIRST)),
    get_test(Node, UUID1, ?FIRST + 2, [?FIRST]),
    ?assertEqual({ok,{<<"admni">>,
                      ?FIRST,
                      ?MASK,
                      ?GW}},
                 rt_sniffle:iprange_claim(Node, UUID1)),
    get_test(Node, UUID1, ?FIRST + 2, []),
    ?assertEqual(ok,
                 rt_sniffle:iprange_release(Node, UUID1, ?FIRST + 1)),
    get_test(Node, UUID1, ?FIRST + 1, []),
    ok.

list_test(Node, List) ->
    {ok, R} = rt_sniffle:iprange_list(Node),
    ?assertEqual(lists:sort(List), lists:sort(R)),
    ok.


get_test(Node, UUID, Current) ->
    ?assertEqual({ok,[{<<"current">>, Current},
                      {<<"first">>, ?FIRST},
                      {<<"gateway">>, ?GW},
                      {<<"last">>, ?LAST},
                      {<<"name">>, ?NAME},
                      {<<"netmask">>, ?MASK},
                      {<<"network">>, ?NET},
                      {<<"tag">>, ?TAG},
                      {<<"uuid">>, UUID},
                      {<<"version">>,<<"0.1.0">>},
                      {<<"vlan">>, ?VLAN}]},
                 rt_sniffle:iprange_get(Node, UUID)).

get_test(Node, UUID, Current, Free) ->
    ?assertEqual({ok,[{<<"current">>, Current},
                      {<<"first">>, ?FIRST},
                      {<<"free">>, Free},
                      {<<"gateway">>, ?GW},
                      {<<"last">>, ?LAST},
                      {<<"name">>, ?NAME},
                      {<<"netmask">>, ?MASK},
                      {<<"network">>, ?NET},
                      {<<"tag">>, ?TAG},
                      {<<"uuid">>, UUID},
                      {<<"version">>,<<"0.1.0">>},
                      {<<"vlan">>, ?VLAN}]},
                 rt_sniffle:iprange_get(Node, UUID)).
