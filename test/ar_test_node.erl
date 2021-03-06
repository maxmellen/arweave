-module(ar_test_node).

-export([start/2, connect_to_slave/0, slave_call/3, slave_call/4]).
-export([slave_gossip/2, slave_add_tx/2, slave_mine/1]).
-export([wait_until_height/2, slave_wait_until_height/2]).
-export([wait_until_block_hash_list/2]).

-include_lib("eunit/include/eunit.hrl").

start(no_block, Peer) ->
	[B0] = ar_weave:init([]),
	start(B0, Peer);
start(B0, Peer) ->
	ar_storage:clear(),
	Node = ar_node:start([], [B0]),
	ar_http_iface_server:reregister(http_entrypoint_node, Node),
	ar_meta_db:reset_peer(Peer),
	Bridge = ar_bridge:start([], Node, ar_meta_db:get(port)),
	ar_http_iface_server:reregister(http_bridge_node, Bridge),
	ar_node:add_peers(Node, Bridge),
	{Node, B0}.

connect_to_slave() ->
	%% Connect the nodes by making an HTTP call.
	SlavePort = slave_call(ar_meta_db, get, [port]),
	{ok, {{<<"200">>, <<"OK">>}, _, _, _, _}} =
		ar_httpc:request(
			<<"GET">>,
			{127, 0, 0, 1, SlavePort},
			"/info",
			[{<<"X-P2p-Port">>, integer_to_binary(ar_meta_db:get(port))}]
		).

slave_call(Module, Function, Args) ->
	slave_call(Module, Function, Args, 5000).

slave_call(Module, Function, Args, Timeout) ->
	ar_rpc:call(slave, Module, Function, Args, Timeout).

slave_gossip(on, Node) ->
	slave_call(ar_node, set_loss_probability, [Node, 0]);
slave_gossip(off, Node) ->
	slave_call(ar_node, set_loss_probability, [Node, 1]).

slave_add_tx(Node, TX) ->
	slave_call(ar_node, add_tx, [Node, TX]).

slave_mine(Node) ->
	slave_call(ar_node, mine, [Node]).

wait_until_height(Node, TargetHeight) ->
	{ok, BHL} = ar_util:do_until(
		fun() ->
			case ar_node:get_blocks(Node) of
				BHL when length(BHL) - 1 == TargetHeight ->
					{ok, BHL};
				_ ->
					false
			end
		end,
		100,
		60 * 1000
	),
	BHL.

slave_wait_until_height(Node, TargetHeight) ->
	slave_call(?MODULE, wait_until_height, [Node, TargetHeight]).

wait_until_block_hash_list(Node, BHL) ->
	?assertEqual(ok, ar_util:do_until(
		fun() ->
			case ar_node:get_blocks(Node) of
				BHL ->
					ok;
				_ ->
					false
			end
		end,
		100,
		60 * 1000
	)).
