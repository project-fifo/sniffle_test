-module(ipranges_ensure_unique).

-behavior(riak_test).
-export([confirm/0]).

-include_lib("eunit/include/eunit.hrl").

-define(NAME, <<"TEST1">>).
-define(NET, rt_sniffle:ip_to_int(<<"10.0.0.0">>)).
-define(GW, rt_sniffle:ip_to_int(<<"10.0.0.1">>)).
-define(MASK, rt_sniffle:ip_to_int(<<"255.255.255.0">>)).
-define(FIRST, rt_sniffle:ip_to_int(<<"10.0.0.100">>)).
-define(LAST, rt_sniffle:ip_to_int(<<"10.0.0.200">>)).
-define(TAG, <<"admni">>).
-define(VLAN, 0).

confirm() ->
    [Node1|_] = Nodes = rt:build_cluster(4),
    lager:info("Ensure all nodes believe ~p is the claimant", [Node1]),
    [?assertEqual(Node1, rt:claimant_according_to(Node)) || Node <- Nodes],
    {ok, UUID1} = rt_sniffle:iprange_create(
                    Node1, ?NAME,
                    ?NET, ?GW, ?MASK,
                    ?FIRST, ?LAST,
                    ?TAG, ?VLAN),
    lager:info("Ensure all nodes know the network.", []),
    [?assertEqual({ok, [UUID1]}, rt_sniffle:iprange_list(Node)) || Node <- Nodes],
    lager:info("Try to get a many of IP's at once.", []),
    IPs = rt:pmap(fun(Node) ->
                          case rt_sniffle:iprange_claim(Node, UUID1) of
                              {ok, {_, IP, _, _}} ->
                                  IP;
                              {error, _} ->
                                  Node
                          end
                  end, Nodes ++ Nodes ++ Nodes),
    IPsSorted = lists:sort(IPs),
    IPsUnique = lists:usort(IPs),
    lager:info("Ensure there was no ip handed twice.", []),
    ?assertEqual(IPsSorted, IPsUnique),
    pass.
