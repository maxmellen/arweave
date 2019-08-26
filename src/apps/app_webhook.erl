-module(app_webhook).
-export([start/1, new_block/2, new_transaction/2]).
-include("ar.hrl").
-include("ar_config.hrl").

-define(NUMBER_OF_TRIES, 10).
-define(WAIT_BETWEEN_TRIES, 30 * 1000).

start(Configs) ->
	adt_simple:start(?MODULE, Configs).

new_block(Configs, Block) ->
	call_webhooks(Block, Configs),
	Configs.

new_transaction(Configs, Tx) ->
	call_webhooks(Tx, Configs),
	Configs.

call_webhooks(Entity, Configs) ->
	lists:foreach(
		fun(#config_webhook { events = Events, url = Url, headers = Headers }) ->
			ok = case lists:member(entity_type(Entity), Events) of
				true -> try_call_webhook(Entity, Url, Headers);
				false -> ok
			end
		end,
		Configs
	).

try_call_webhook(Entity, Url, Headers) ->
	try_call_webhook(Entity, Url, Headers, 0).

try_call_webhook(Entity, Url, Headers, N) when N < ?NUMBER_OF_TRIES ->
	try
		{ok, {Scheme, _UserInfo, Host, Port, Path, Query}} = http_uri:parse(Url),
		{ok, Client} = fusco:start(atom_to_list(Scheme) ++ "://" ++ binary_to_list(Host) ++ ":" ++ integer_to_list(Port), []),
		{ok, Result} = fusco:request(Client, <<Path/binary, Query/binary>>, <<"POST">>, Headers, to_json(Entity), 10000),
		case Result of
			{{<<"200">>, _}, _, _, _, _} ->
				ok = fusco:disconnect(Client),
				ar:info([successful_webhook_call, {
					{type, entity_type(Entity)},
					{id, entity_id(Entity)},
					{url, Url},
					{headers, Headers},
					{response, Result}
				}]),
				Url;
			UnsuccessfulResult ->
				ar:warn([unsuccessful_webhook_call,
					{type, entity_type(Entity)},
					{id, entity_id(Entity)},
					{url, Url},
					{headers, Headers},
					{response, UnsuccessfulResult},
					{retry_in, ?WAIT_BETWEEN_TRIES}
				]),
				timer:sleep(?WAIT_BETWEEN_TRIES),
				try_call_webhook(Entity, Url, Headers, N+1)
		end,
		ok
	catch
		Class:Exception ->
			ar:warn([unexpected_webhook_call_error,
				{type, entity_type(Entity)},
				{id, entity_id(Entity)},
				{url, Url},
				{headers, Headers},
				{class, Class},
				{exception, Exception},
				{retry_in, ?WAIT_BETWEEN_TRIES}
			]),
			timer:sleep(?WAIT_BETWEEN_TRIES),
			try_call_webhook(Entity, Url, Headers, N+1)
	end;
try_call_webhook(Entity, Url, Headers, _) ->
	ar:warn([gave_up_webhook_call,
		{type, entity_type(Entity)},
		{id, entity_id(Entity)},
		{url, Url},
		{headers, Headers},
		{number_of_tries, ?NUMBER_OF_TRIES},
		{wait_between_tries, ?WAIT_BETWEEN_TRIES}
	]),
	ok.

entity_type(#block {}) -> block;
entity_type(#tx {}) -> transaction.

entity_id(#block { indep_hash = Id }) -> ar_util:encode(Id);
entity_id(#tx { id = Id }) -> ar_util:encode(Id).

to_json(#block {} = Block) ->
	{JsonKVPairs} = ar_serialize:block_to_json_struct(Block),
	JsonStruct = {lists:keydelete(wallet_list, 1, JsonKVPairs)},
	ar_serialize:jsonify({[{block, JsonStruct}]});
to_json(#tx {} = Tx) ->
	{JsonKVPairs} = ar_serialize:tx_to_json_struct(Tx),
	JsonStruct = {lists:keydelete(data, 1, JsonKVPairs)},
	ar_serialize:jsonify({[{transaction, JsonStruct}]}).
