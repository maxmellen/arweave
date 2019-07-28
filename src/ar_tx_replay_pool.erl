-module(ar_tx_replay_pool).

-export([verify_tx/6, verify_block_txs/5, pick_txs_to_mine/5]).
-export([pick_txs_to_keep_in_mempool/5]).

-include("ar.hrl").

%%% This module contains functions for transaction verification. It relies on
%%% some verification helpers from the ar_tx and ar_node_utils modules.
%%% The module should be used to verify transactions on-edge, validate
%%% new blocks' transactions, pick transactions to include into a block, and
%%% remove no longer valid transactions from the mempool after accepting a new block.

-record(state, {
	%% Maps block hash to the set of TX IDs included in it.
	weave_map = maps:new(),
	%% Block hash list containing last ?MAX_TX_ANCHOR_DEPTH blocks.
	bhl = []
}).

-record(mempool, {
	tx_id_set = sets:new()
}).

%% @doc Verify that a transaction against the given mempool, wallet list, recent
%% weave txs, current block height, and current difficulty. The mempool is used
%% to look for the same transaction there and to make sure the transaction
%% does not reference another transaction from the mempool. The mempool is NOT
%% used to verify shared resources like funds, wallet list references, and
%% data size. Therefore, the function is suitable for on-edge verification
%% where we want to accept potentially conflicting transactions to avoid
%% consensus issues later.
verify_tx(TX, Diff, Height, RecentTXs, MempoolTXs, WalletList) ->
	WeaveState = create_state(RecentTXs),
	TXIDSet = sets:from_list(
		[MempoolTX#tx.id || MempoolTX <- MempoolTXs]
	),
	Mempool = #mempool {
		tx_id_set = TXIDSet
	},
	verify_tx(
		general_verification,
		TX,
		Diff,
		Height,
		WalletList,
		WeaveState,
		Mempool
	).

%% @doc Verify the transactions are valid for the block taken into account
%% the given current difficulty and height, the previous blocks' wallet list,
%% and recent weave transactions.
verify_block_txs(TXs, Diff, Height, WalletList, RecentTXs) ->
	WeaveState = create_state(RecentTXs),
	{VerifiedTXs, _, _} = apply_txs(
		TXs,
		Diff,
		Height,
		WalletList,
		WeaveState,
		#mempool {}
	),
	case length(VerifiedTXs) of
		L when L == length(TXs) ->
			valid;
		_ ->
			invalid
	end.

%% @doc Pick a list of transactions from the mempool to mine on.
%% Transactions have to be valid when applied on top of each other taken
%% into account the current height and diff, recent weave transactions, and
%% the wallet list. The total data size of chosen transactions does not
%% exceed the block size limit. Before a valid subset of transactions is chosen,
%% transactions are sorted from biggest to smallest and then from oldest
%% block anchors to newest.
pick_txs_to_mine(RecentTXs, Height, Diff, WalletList, TXs) ->
	case ar_fork:height_1_8() of
		H when Height >= H ->
			pick_txs_to_mine_post_1_8(RecentTXs, Height, Diff, WalletList, TXs);
		_ ->
			pick_txs_to_mine_pre_1_8(Height, Diff, WalletList, TXs)
	end.

%% @doc Choose transactions to keep in the mempool after a new block is
%% accepted. Transactions are verified independently from each other
%% taking into account the given difficulty and height of the new block,
%% the new recent weave transactions, and the new wallet list.
pick_txs_to_keep_in_mempool(RecentTXs, TXs, Diff, Height, WalletList) ->
	WeaveState = create_state(RecentTXs),
	Mempool = #mempool{},
	lists:filter(
		fun(TX) ->
			case verify_tx(
				general_verification,
				TX,
				Diff,
				Height,
				WalletList,
				WeaveState,
				Mempool
			) of
				{valid, _, _} ->
					true;
				{invalid, _} ->
					false
			end
		end,
		TXs
	).

%% PRIVATE

create_state(RecentTXs) ->
	MaxDepthTXs = lists:sublist(RecentTXs, ?MAX_TX_ANCHOR_DEPTH),
	{BHL, Map} = lists:foldr(
		fun({BH, TXIDs}, {BHL, Map}) ->
			{[BH | BHL], maps:put(BH, sets:from_list(TXIDs), Map)}
		end,
		{[], maps:new()},
		MaxDepthTXs
	),
	#state {
		weave_map = Map,
		bhl = BHL
	}.

verify_tx(general_verification, TX, Diff, Height, FloatingWalletList, WeaveState, Mempool) ->
	case ar_tx:verify(TX, Diff, Height, FloatingWalletList) of
		true ->
			verify_tx(last_tx_in_mempool, TX, Diff, Height, FloatingWalletList, WeaveState, Mempool);
		false ->
			{invalid, tx_verification_failed}
	end;
verify_tx(last_tx_in_mempool, TX, Diff, Height, FloatingWalletList, WeaveState, Mempool) ->
	case ar_fork:height_1_8() of
		H when Height >= H ->
			%% Only verify after fork 1.8 otherwise it causes a soft fork
			%% since current nodes can accept blocks with a chain of last_tx
			%% references. The check would still fail on edge pre 1.8 since
			%% TX is validated against a previous blocks' wallet list then.
			case sets:is_element(TX#tx.last_tx, Mempool#mempool.tx_id_set) of
				true ->
					{invalid, last_tx_in_mempool};
				false ->
					verify_tx(
						last_tx,
						TX,
						Diff,
						Height,
						FloatingWalletList,
						WeaveState,
						Mempool
					)
			end;
		_ ->
			verify_tx(
				last_tx,
				TX,
				Diff,
				Height,
				FloatingWalletList,
				WeaveState,
				Mempool
			)
	end;
verify_tx(last_tx, TX, Diff, Height, FloatingWalletList, WeaveState, Mempool) ->
	case ar_tx:check_last_tx(FloatingWalletList, TX) of
		true ->
			NewMempool = Mempool#mempool {
				tx_id_set = sets:add_element(TX#tx.id, Mempool#mempool.tx_id_set)
			},
			NewFWL = ar_node_utils:apply_tx(FloatingWalletList, TX, Height),
			{valid, NewFWL, NewMempool};
		false ->
			verify_tx(anchor_check, TX, Diff, Height, FloatingWalletList, WeaveState, Mempool)
	end;
verify_tx(anchor_check, TX, Diff, Height, FloatingWalletList, WeaveState, Mempool) ->
	case lists:member(TX#tx.last_tx, WeaveState#state.bhl) of
		false ->
			{invalid, tx_bad_anchor};
		true ->
			verify_tx(weave_check, TX, Diff, Height, FloatingWalletList, WeaveState, Mempool)
	end;
verify_tx(weave_check, TX, Diff, Height, FloatingWalletList, WeaveState, Mempool) ->
	case weave_map_contains_tx(TX#tx.id, WeaveState#state.weave_map) of
		true ->
			{invalid, tx_already_in_weave};
		false ->
			verify_tx(mempool_check, TX, Diff, Height, FloatingWalletList, WeaveState, Mempool)
	end;
verify_tx(mempool_check, TX, _Diff, Height, FloatingWalletList, _WeaveState, Mempool) ->
	case sets:is_element(TX#tx.id, Mempool#mempool.tx_id_set) of
		true ->
			{invalid, tx_already_in_mempool};
		false ->
			NewMempool = Mempool#mempool {
				tx_id_set = sets:add_element(TX#tx.id, Mempool#mempool.tx_id_set)
			},
			NewFWL = ar_node_utils:apply_tx(FloatingWalletList, TX, Height),
			{valid, NewFWL, NewMempool}
	end.

weave_map_contains_tx(TXID, WeaveMap) ->
	lists:any(
		fun(BH) ->
			sets:is_element(TXID, maps:get(BH, WeaveMap))
		end,
		maps:keys(WeaveMap)
	).

apply_txs(TXs, Diff, Height, WalletList, WeaveState, Mempool) ->
	lists:foldl(
		fun(TX, {VerifiedTXs, FloatingWalletList, FloatingMempool}) ->
			case verify_tx(
				general_verification,
				TX,
				Diff,
				Height,
				FloatingWalletList,
				WeaveState,
				FloatingMempool
			) of
				{valid, NewFWL, NewMempool} ->
					{VerifiedTXs ++ [TX], NewFWL, NewMempool};
				{invalid, _} ->
					{VerifiedTXs, FloatingWalletList, FloatingMempool}
			end
		end,
		{[], WalletList, Mempool},
		TXs
	).

pick_txs_to_mine_post_1_8(RecentTXs, Height, Diff, WalletList, TXs) ->
	WeaveState = create_state(RecentTXs),
	{VerifiedTXs, _, _} = apply_txs(
		TXs,
		Diff,
		Height,
		WalletList,
		WeaveState,
		#mempool {}
	),
	pick_txs_under_size_limit(
		sort_txs_by_data_size_and_anchor(VerifiedTXs, WeaveState#state.bhl)
	).

pick_txs_to_mine_pre_1_8(Height, Diff, WalletList, TXs) ->
	lists:filter(
		fun(TX) ->
			ar_tx:verify(TX, Diff, Height, WalletList)
		end,
		TXs
	).

pick_txs_under_size_limit(TXs) ->
	{_, TXsUnderSizeLimit} = lists:foldl(
		fun(TX, {TotalSize, PickedTXs}) ->
			TXSize = byte_size(TX#tx.data),
			case TXSize + TotalSize of
				NewTotalSize when NewTotalSize =< ?BLOCK_TX_DATA_SIZE_LIMIT ->
					{NewTotalSize, PickedTXs ++ [TX]};
				_ ->
					{TotalSize, PickedTXs}
			end
		end,
		{0, []},
		TXs
	),
	TXsUnderSizeLimit.

sort_txs_by_data_size_and_anchor(TXs, BHL) ->
	lists:sort(fun(TX1, TX2) -> compare_txs(TX1, TX2, BHL) end, TXs).

compare_txs(TX1, TX2, BHL) when byte_size(TX1#tx.data) == byte_size(TX2#tx.data) ->
	compare_anchors(TX1, TX2, BHL);
compare_txs(TX1, TX2, _) ->
	byte_size(TX1#tx.data) > byte_size(TX2#tx.data).

compare_anchors(_Anchor1, _Anchor2, []) ->
	true;
compare_anchors(Anchor, Anchor, _) ->
	true;
compare_anchors(Anchor1, _Anchor2, [Anchor1 | _]) ->
	false;
compare_anchors(_Anchor1, Anchor2, [Anchor2 | _]) ->
	true;
compare_anchors(Anchor1, Anchor2, [_ | Anchors]) ->
	compare_anchors(Anchor1, Anchor2, Anchors).
