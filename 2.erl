-module(my_cache).
-export([create/1, insert/3, insert/4, lookup/2, delete_obsolete/1]).

-define(TABLE_PREFIX, "cache_").

create(TableName) ->
    ets:new(?TABLE_PREFIX ++ TableName, [named_table, set, public, {keypos, 1}]).

insert(TableName, Key, Value) ->
    ets:insert(?TABLE_PREFIX ++ TableName, {Key, Value, undefined}).

insert(TableName, Key, Value, ExpiryTime) ->
    CurrentTime = calendar:universal_time(),
    ExpiryTimeInSecs = CurrentTime + ExpiryTime,
    ets:insert(?TABLE_PREFIX ++ TableName, {Key, Value, ExpiryTimeInSecs}).

lookup(TableName, Key) ->
    case ets:lookup(?TABLE_PREFIX ++ TableName, Key) of
        [{Key, Value, undefined}] -> Value;
        [{Key, Value, ExpiryTime}] when ExpiryTime == undefined; 
             ExpiryTime >= calendar:universal_time() -> Value;
        _ -> undefined
    end.

delete_obsolete(TableName) ->
    CurrentTime = calendar:universal_time(),
    ets:select(?TABLE_PREFIX ++ TableName, [{[], [{'$1', '$2', '$3'}], [{'<=', '$3', CurrentTime}]}]),
    ok.
