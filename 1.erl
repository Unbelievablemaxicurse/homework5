-module(comparison).
-export([compare/0]).

% Створення порівняльної таблиці
compare() ->
    lists:foreach(fun(M) -> compare_for(M) end, [hash_table, avl_tree, rb_tree, trie, b_tree, linked_list, dict]).

% Функція для порівняння різних механізмів
compare_for(M) ->
    io:format("Testing ~p:~n", [M]),
    test_addition(M),
    test_update(M),
    test_deletion(M),
    test_read(M),
    io:format("~n").

% Тестування додавання
test_addition(M) ->
    {Time, _} = timer:tc(M, add_elements, []),
    io:format("  Time for addition: ~p microseconds~n", [Time]).

% Тестування оновлення
test_update(M) ->
    {Time, _} = timer:tc(M, update_elements, []),
    io:format("  Time for update: ~p microseconds~n", [Time]).

% Тестування видалення
test_deletion(M) ->
    {Time, _} = timer:tc(M, delete_elements, []),
    io:format("  Time for deletion: ~p microseconds~n", [Time]).

% Тестування читання
test_read(M) ->
    {Time, _} = timer:tc(M, read_elements, []),
    io:format("  Time for read: ~p microseconds~n", [Time]).

% Приклад функцій для різних механізмів

% 1. Хеш-таблиця
hash_table:add_elements() ->
    Table = ets:new(hash_table, [set, public]),
    lists:foreach(fun(Key) -> ets:insert(Table, {Key, Key}) end, lists:seq(1, 10000)).

hash_table:update_elements() ->
    Table = ets:new(hash_table, [set, public]),
    lists:foreach(fun(Key) -> ets:insert(Table, {Key, Key * 2}) end, lists:seq(1, 10000)).

hash_table:delete_elements() ->
    Table = ets:new(hash_table, [set, public]),
    lists:foreach(fun(Key) -> ets:delete(Table, Key) end, lists:seq(1, 10000)).

hash_table:read_elements() ->
    Table = ets:new(hash_table, [set, public]),
    lists:foreach(fun(Key) -> ets:lookup(Table, Key) end, lists:seq(1, 10000)).

% 2. AVL-дерево
avl_tree:add_elements() ->
    % Реалізуйте додавання в AVL-дерево
    ok.

avl_tree:update_elements() ->
    % Реалізуйте оновлення в AVL-дерево
    ok.

avl_tree:delete_elements() ->
    % Реалізуйте видалення в AVL-дерево
    ok.

avl_tree:read_elements() ->
    % Реалізуйте читання в AVL-дерево
    ok.

% Інші механізми (червоне-чорне дерево, три, B-дерево, зв'язаний список, словники) аналогічно.

