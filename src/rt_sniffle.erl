-module(rt_sniffle).


-export([node_endpoing/1,
         call/2,
         ip_to_int/1]).


-export([
         vm_register/3,
         vm_unregister/2,
         vm_update/4,
         vm_set/3,
         vm_set/4,
         vm_log/3,
         vm_snapshot/3,
         vm_delete_snapshot/3,
         vm_rollback_snapshot/3,
         vm_list/1,
         vm_list/2,
         vm_get/2,
         vm_start/2,
         vm_stop/2,
         vm_reboot/2,
         vm_stop/3,
         vm_reboot/3,
         vm_delete/2
        ]).

-export([
         grouping_add/3,
         grouping_delete/2,
         grouping_get/2,
         grouping_metadata_set/3,
         grouping_metadata_set/4,
         grouping_list/3,
         grouping_list/2,
         grouping_list/1,
         grouping_add_element/3,
         grouping_remove_element/3,
         grouping_add_grouping/3,
         grouping_remove_grouping/3
        ]).

-export([
         hypervisor_register/4,
         hypervisor_unregister/2,
         hypervisor_get/2,
         hypervisor_set/3,
         hypervisor_set/4,
         hypervisor_list/1,
         hypervisor_list/2
        ]).

-export([cloud_status/1]).

-export([
         dataset_create/2,
         dataset_import/2,
         dataset_delete/2,
         dataset_get/2,
         dataset_set/3,
         dataset_set/4,
         dataset_list/1,
         dataset_list/2
        ]).

-export([
         img_create/4,
         img_delete/3,
         img_get/3,
         img_list/1,
         img_list/2
         ]).

-export([
         package_create/2,
         package_delete/2,
         package_get/2,
         package_set/3,
         package_set/4,
         package_list/1,
         package_list/2
        ]).

-export([
         network_create/2,
         network_delete/2,
         network_get/2,
         network_add_iprange/3,
         network_remove_iprange/3,
         network_list/1,
         network_list/2
         ]).

-export([
         iprange_create/9,
         iprange_delete/2,
         iprange_get/2,
         iprange_release/3,
         iprange_claim/2,
         iprange_list/1,
         iprange_list/2,
         iprange_set/3,
         iprange_set/4
        ]).


node_endpoing(Node) ->
    {ok, R} = rpc:call(Node, application, get_env, [mdns_server_lib, listener]),
    R.

call(Node, Msg) ->
    {IP, Port} = node_endpoing(Node),
    lager:debug("~s:~p <- ~p", [IP, Port, Msg]),
    {ok, Socket} = gen_tcp:connect(IP, Port, [binary, {active,false}, {packet,4}], 100),
    ok = gen_tcp:send(Socket, term_to_binary(Msg)),
    {ok, Repl} = gen_tcp:recv(Socket, 0),
    {reply, Res} = binary_to_term(Repl),
    lager:debug("~s:~p -> ~p", [IP, Port, Res]),
    gen_tcp:close(Socket),
    Res.

%%%===================================================================
%%% Grouping Functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Adds a new grouping to sniffle, the name is a plain
%%   binary, the type is the atom cluster or stack
%%   encapsulated in $ signs.
%%   A UUID is returned.
%% @end
%%--------------------------------------------------------------------
grouping_add(Node, Name, cluster) when
      is_binary(Name)->
    call(Node, {grouping, add, Name, cluster});
grouping_add(Node, Name, none) when
      is_binary(Name)->
    call(Node, {grouping, add, Name, none});
grouping_add(Node, Name, stack) when
      is_binary(Name)->
    call(Node, {grouping, add, Name, stack}).

%%--------------------------------------------------------------------
%% @doc Deletes a grouping script from the library
%% @end
%%--------------------------------------------------------------------
grouping_delete(Node, ID) when
      is_binary(ID)->
    call(Node, {grouping, delete, ID}).

%%--------------------------------------------------------------------
%% @doc Reads a grouping script and returns the jsx object for it.
%% @end
%%--------------------------------------------------------------------
grouping_get(Node, ID) when
      is_binary(ID)->
    call(Node, {grouping, get, ID}).

%%--------------------------------------------------------------------
%% @doc Lists the ID's of all scripts in the database.
%% @end
%%--------------------------------------------------------------------
grouping_list(Node)->
    call(Node, {grouping, list}).

%%--------------------------------------------------------------------
%% @doc Lists the ID's of all scripts in the database filtered by
%%   the passed requirements.
%% @end
%%--------------------------------------------------------------------
grouping_list(Node, Requirements)->
    call(Node, {grouping, list, Requirements}).

%%--------------------------------------------------------------------
%% @doc Lists the ID's of all scripts in the database filtered by
%%   the passed requirements.
%% @end
%%--------------------------------------------------------------------
grouping_list(Node, Requirements, Full)->
    call(Node, {grouping, list, Requirements, Full}).

%%--------------------------------------------------------------------
%% @doc Lists the ID's of all scripts in the database filtered by
%%   the passed requirements.
%% @end
%%--------------------------------------------------------------------
grouping_metadata_set(Node, Grouping, Attribute, Value) when
      is_binary(Grouping) ->
    call(Node, {grouping, metadata, set, Grouping, Attribute, Value}).

%%--------------------------------------------------------------------
%% @doc Sets options on a dtace script. The root key 'config' has a
%%   special meaning here since it holds replacement variables.
%% @end
%%--------------------------------------------------------------------
grouping_metadata_set(Node, Grouping, Attributes) when
      is_binary(Grouping) ->
    call(Node, {grouping, metadata, set, Grouping, Attributes}).

grouping_add_element(Node, Grouping, Element)
  when is_binary(Grouping), is_binary(Element) ->
    call(Node, {grouping, element, add, Grouping, Element}).

grouping_remove_element(Node, Grouping, Element)
  when is_binary(Grouping), is_binary(Element) ->
    call(Node, {grouping, element, remove, Grouping, Element}).

grouping_add_grouping(Node, Grouping, Element)
  when is_binary(Grouping), is_binary(Element) ->
    call(Node, {grouping, grouping, add, Grouping, Element}).

grouping_remove_grouping(Node, Grouping, Element)
  when is_binary(Grouping), is_binary(Element) ->
    call(Node, {grouping, grouping, remove, Grouping, Element}).

%%%===================================================================
%%% VM Functions
%%%===================================================================

vm_register(Node, VM, Hypervisor) when
      is_binary(VM),
      is_binary(Hypervisor) ->
    call(Node, {vm, register, VM, Hypervisor}).

vm_unregister(Node, VM) when
      is_binary(VM) ->
    call(Node, {vm, unregister, VM}).


vm_start(Node, VM) when
      is_binary(VM) ->
    call(Node, {vm, start, VM}).

vm_stop(Node, VM) when is_binary(VM) ->
    vm_stop(Node, VM, []).

vm_stop(Node, VM, []) when
      is_binary(VM)->
    call(Node, {vm, stop, VM});

vm_stop(Node, VM, [force]) when
      is_binary(VM)->
    call(Node, {vm, stop, force, VM}).

vm_get(Node, VM) when
      is_binary(VM) ->
    call(Node, {vm, get, VM}).

vm_reboot(Node, VM) when
      is_binary(VM) ->
    vm_reboot(Node, VM, []).

vm_reboot(Node, VM, []) when
      is_binary(VM) ->
    call(Node, {vm, reboot, VM});

vm_reboot(Node, VM, [force]) when
      is_binary(VM) ->
    call(Node, {vm, reboot, force, VM}).

vm_delete(Node, VM) when
      is_binary(VM) ->
    call(Node, {vm, delete, VM}).

vm_update(Node, VM, Package, Config) when
      is_binary(VM),
      is_list(Config) ->
    call(Node, {vm, update, VM, Package, Config}).

vm_set(Node, VM, Attribute, Value) when
      is_binary(VM) ->
    call(Node, {vm, set, VM, Attribute, Value}).

vm_set(Node, VM, Attributes) when
      is_binary(VM) ->
    call(Node, {vm, set, VM, [{K, V} || {K, V} <- Attributes]}).

vm_log(Node, VM, Log) ->
    call(Node, {vm, log, VM, Log}).

vm_snapshot(Node, VM, Comment) ->
    call(Node, {vm, snapshot, VM, Comment}).

vm_delete_snapshot(Node, VM, UUID) ->
    call(Node, {vm, snapshot, delete, VM, UUID}).


vm_rollback_snapshot(Node, VM, UUID) ->
    call(Node, {vm, snapshot, rollback, VM, UUID}).

vm_list(Node) ->
    call(Node, {vm, list}).

vm_list(Node, Reqs) ->
    call(Node, {vm, list, Reqs}).



%%%===================================================================
%%% Hypervisor Functions
%%%===================================================================

hypervisor_register(Node, Hypervisor, Host, Port) when
      is_binary(Hypervisor),
      is_integer(Port),
      Port > 0 ->
    call(Node, {hypervisor, register, Hypervisor, Host, Port}).

hypervisor_unregister(Node, Hypervisor) ->
    call(Node, {hypervisor, unregister, Hypervisor}).

hypervisor_get(Node, Hypervisor) ->
    call(Node, {hypervisor, get, Hypervisor}).

hypervisor_set(Node, Hypervisor, Resource, Value) ->
    call(Node, {hypervisor, set, Hypervisor, Resource, Value}).

hypervisor_set(Node, Hypervisor, Resources) ->
    call(Node, {hypervisor, set, Hypervisor, Resources}).

hypervisor_list(Node) ->
    call(Node, {hypervisor, list}).

hypervisor_list(Node, Requirements) ->
    call(Node, {hypervisor, list, Requirements}).

%%%===================================================================
%%% Cloud Functions
%%%===================================================================

cloud_status(Node) ->
    call(Node, {cloud, status}).

%%%===================================================================
%%%  DATASET Functions
%%%===================================================================

dataset_create(Node, Dataset) ->
    call(Node, {dataset, create, Dataset}).

dataset_import(Node, URL) ->
    call(Node, {dataset, import, URL}).

dataset_delete(Node, Dataset) ->
    call(Node, {dataset, delete, Dataset}).

dataset_get(Node, Dataset) ->
    call(Node, {dataset, get, Dataset}).

dataset_set(Node, Dataset, Attributes) ->
    call(Node, {dataset, set, Dataset, Attributes}).

dataset_set(Node, Dataset, Attribute, Value) ->
    call(Node, {dataset, set, Dataset, Attribute, Value}).

dataset_list(Node) ->
    call(Node, {dataset, list}).

dataset_list(Node, Reqs) ->
    call(Node, {dataset, list, Reqs}).


%%%===================================================================
%%%  IMG Functions
%%%===================================================================

img_create(Node, Img, Idx, Data) ->
    call(Node, {img, create, Img, Idx, Data}).

img_delete(Node, Img, Idx) ->
    call(Node, {img, delete, Img, Idx}).

img_get(Node, Img, Idx) ->
    call(Node, {img, get, Img, Idx}).

img_list(Node) ->
    call(Node, {img, list}).

img_list(Node, Img) ->
    call(Node, {img, list, Img}).


%%%===================================================================
%%%  PACKAGE Functions
%%%===================================================================

package_create(Node, Package) when
      is_binary(Package) ->
    call(Node, {package, create, Package}).

package_delete(Node, Package) when
      is_binary(Package) ->
    call(Node, {package, delete, Package}).

package_get(Node, Package) when
      is_binary(Package) ->
    call(Node, {package, get, Package}).

package_set(Node, Package, Attributes) when
      is_binary(Package),
      is_list(Attributes) ->
    call(Node, {package, set, Package, Attributes}).

package_set(Node, Package, Attribute, Value)  when
      is_binary(Package) ->
    call(Node, {package, set, Package, Attribute, Value}).

package_list(Node) ->
    call(Node, {package, list}).

package_list(Node, Reqs) ->
    call(Node, {package, list, Reqs}).

network_create(Node, Name) ->
    call(Node, {network, create, Name}).

network_delete(Node, Network) ->
    call(Node, {network, delete, Network}).

network_get(Node, Network) ->
    call(Node, {network, get, Network}).

network_add_iprange(Node, Network, IPRange) ->
    call(Node, {network, add_iprange, Network, IPRange}).

network_remove_iprange(Node, Network, IPRange) ->
    call(Node, {network, remove_iprange, Network, IPRange}).

network_list(Node) ->
    call(Node, {network, list}).

network_list(Node, Requirements) ->
    call(Node, {network, list, Requirements}).


%%%===================================================================
%%%  IPrange Functions
%%%===================================================================

iprange_create(Node, Iprange, Network, Gateway, Netmask, First, Last, Tag, Vlan) when
      is_binary(Iprange),
      is_binary(Tag),
      is_integer(Network),
      is_integer(Gateway), Network =:= (Gateway band Netmask),
      is_integer(Netmask),
      is_integer(First), Network =:= (First band Netmask),
      is_integer(Last), Network =:= (Last band Netmask),
      is_integer(Vlan), Vlan >= 0 ->
    call(Node, {iprange, create, Iprange,
          Network, Gateway, Netmask,
          First, Last,
          Tag, Vlan});

iprange_create(Node, Iprange, Network, Gateway, Netmask, First, Last, Tag, Vlan) when
      is_binary(Iprange),
      is_binary(Tag),
      is_integer(Vlan), Vlan >= 0->
    iprange_create(Node, Iprange,
                   ip_to_int(Network),
                   ip_to_int(Gateway),
                   ip_to_int(Netmask),
                   ip_to_int(First),
                   ip_to_int(Last),
                   Tag,
                   Vlan).

iprange_delete(Node, Iprange) ->
    call(Node, {iprange, delete, Iprange}).

iprange_get(Node, Iprange) ->
    call(Node, {iprange, get, Iprange}).

iprange_release(Node, Iprange, Ip) when
      is_binary(Iprange),
      is_integer(Ip) ->
    call(Node, {iprange, release, Iprange, ip_to_int(Ip)});

iprange_release(Node, Iprange, Ip) when
      is_binary(Iprange) ->
    iprange_release(Node, Iprange, ip_to_int(Ip)).

iprange_claim(Node, Iprange) ->
    call(Node, {iprange, claim, Iprange}).

iprange_list(Node) ->
    call(Node, {iprange, list}).

iprange_list(Node, Reqs) ->
    call(Node, {iprange, list, Reqs}).


iprange_set(Node, Iprange, Attributes) when
      is_binary(Iprange),
      is_list(Attributes) ->
    call(Node, {iprange, set, Iprange, Attributes}).

iprange_set(Node, Iprange, Attribute, Value)  when
      is_binary(Iprange) ->
    call(Node, {iprange, set, Iprange, Attribute, Value}).

ip_to_int(IP) when is_integer(IP) ->
    IP;

ip_to_int(IP) ->
    [As, Bs, Cs, Ds] = re:split(IP, "\\.", [{return, list}]),
    {A, _} = string:to_integer(As),
    {B, _} = string:to_integer(Bs),
    {C, _} = string:to_integer(Cs),
    {D, _} = string:to_integer(Ds),
    <<I:32>> = <<A:8, B:8, C:8, D:8>>,
    I.
