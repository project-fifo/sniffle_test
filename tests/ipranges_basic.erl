-module(ipranges_basic).
-include_lib("eunit/include/eunit.hrl").

-behavior(riak_test).
-export([confirm/0]).

-define(NAME1, <<"TEST1">>).
-define(NET1, rt_sniffle:ip_to_int(<<"10.0.0.0">>)).
-define(GW1, rt_sniffle:ip_to_int(<<"10.0.0.1">>)).
-define(MASK1, rt_sniffle:ip_to_int(<<"255.255.255.0">>)).
-define(FIRST1, rt_sniffle:ip_to_int(<<"10.0.0.100">>)).
-define(LAST1, rt_sniffle:ip_to_int(<<"10.0.0.200">>)).
-define(TAG1, <<"admni">>).
-define(VLAN1, 0).

-define(NAME2, <<"TEST1">>).
-define(NET2, rt_sniffle:ip_to_int(<<"10.0.0.0">>)).
-define(GW2, rt_sniffle:ip_to_int(<<"10.0.0.1">>)).
-define(MASK2, rt_sniffle:ip_to_int(<<"255.255.255.0">>)).
-define(FIRST2, rt_sniffle:ip_to_int(<<"10.0.0.100">>)).
-define(LAST2, rt_sniffle:ip_to_int(<<"10.0.0.200">>)).
-define(TAG2, <<"admni">>).
-define(VLAN2, 0).

confirm() ->
    lager:info("Deploy node to test command line"),
    [Node] = rt:deploy_nodes(1),
    ?assertEqual(ok, rt:wait_until_nodes_ready([Node])),
    list_test(Node, []),
    {ok, UUID1} = rt_sniffle:iprange_create(
                    Node, ?NAME1,
                    ?NET1, ?GW1, ?MASK1,
                    ?FIRST1, ?LAST1,
                    ?TAG1, ?VLAN1),
    list_test(Node, [UUID1]),
    get_test(Node, UUID1, ?FIRST1),
    ?assertEqual({ok,{?TAG1,
                      ?FIRST1,
                      ?MASK1,
                      ?GW1}},
                 rt_sniffle:iprange_claim(Node, UUID1)),
    get_test(Node, UUID1, ?FIRST1 + 1),
    ?assertEqual({ok,{?TAG1,
                      ?FIRST1 + 1,
                      ?MASK1,
                      ?GW1}},
                 rt_sniffle:iprange_claim(Node, UUID1)),
    get_test(Node, UUID1, ?FIRST1 + 2),
    ?assertEqual(ok,
                 rt_sniffle:iprange_release(Node, UUID1, ?FIRST1)),
    get_test(Node, UUID1, ?FIRST1 + 2, [?FIRST1]),
    ?assertEqual({ok,{?TAG1,
                      ?FIRST1,
                      ?MASK1,
                      ?GW1}},
                 rt_sniffle:iprange_claim(Node, UUID1)),
    get_test(Node, UUID1, ?FIRST1 + 2, []),
    ?assertEqual(ok,
                 rt_sniffle:iprange_release(Node, UUID1, ?FIRST1 + 1)),
    get_test(Node, UUID1, ?FIRST1 + 1, []),
    ?assertEqual(not_found, rt_sniffle:iprange_get(Node, <<"bla">>)),

    pass.



list_test(Node, List) ->
    {ok, R} = rt_sniffle:iprange_list(Node),
    ?assertEqual(lists:sort(List), lists:sort(R)),
    ok.


get_test(Node, UUID, Current) ->
    ?assertEqual({ok,[{<<"current">>, Current},
                      {<<"first">>, ?FIRST1},
                      {<<"gateway">>, ?GW1},
                      {<<"last">>, ?LAST1},
                      {<<"name">>, ?NAME1},
                      {<<"netmask">>, ?MASK1},
                      {<<"network">>, ?NET1},
                      {<<"tag">>, ?TAG1},
                      {<<"uuid">>, UUID},
                      {<<"version">>,<<"0.1.0">>},
                      {<<"vlan">>, ?VLAN1}]},
                 rt_sniffle:iprange_get(Node, UUID)).

get_test(Node, UUID, Current, Free) ->
    ?assertEqual({ok,[{<<"current">>, Current},
                      {<<"first">>, ?FIRST1},
                      {<<"free">>, Free},
                      {<<"gateway">>, ?GW1},
                      {<<"last">>, ?LAST1},
                      {<<"name">>, ?NAME1},
                      {<<"netmask">>, ?MASK1},
                      {<<"network">>, ?NET1},
                      {<<"tag">>, ?TAG1},
                      {<<"uuid">>, UUID},
                      {<<"version">>,<<"0.1.0">>},
                      {<<"vlan">>, ?VLAN1}]},
                 rt_sniffle:iprange_get(Node, UUID)).
