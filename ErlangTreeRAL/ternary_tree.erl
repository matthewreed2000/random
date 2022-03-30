-module(ternary_tree).
-export([append/2, update/3, get/2]).

% ternary_tree = atom(nil)                                                                        OR
% ternary_tree = {atom(node), Count:integer, Left:ternary_tree, Middle:leaf, Right:ternary_tree}  OR

% leaf = atom(nil)              OR
% leaf = {atom(leaf), Value:a}

% % spec append::a ternary_tree -> ternary_tree
% append(Value, nil) -> append(Value, 0, nil);
% append(Value, Tree={node, Count, _, _, _}) -> append(Value, Count, Tree).

% % spec append::a integer ternary_tree -> ternary_tree
% append(Value, 0, nil) -> {node, 1, nil, {leaf, Value}, nil};
% append(Value, 1, nil) -> {node, 1, nil, nil, append(Value, 0, nil)};
% append(Value, Index, {node, Count, Left, Middle, Right}) when (Index rem 2 == 1) -> {node, Count+1, Left, Middle, append(Value, Index div 2, Right)};
% append(Value, Index, {node, Count, Left, Middle, Right}) -> {node, Count+1, append(Value, Index div 2, Left), Middle, Right}.

% % spec get::integer ternary_tree -> a
% get(0, {node, _, _, {leaf, Value}, _}) -> Value;
% get(Index, {node, _, Left, _, _}) when (Index rem 2 == 0) -> get(Index div 2, Left);
% get(Index, {node, _, _, _, Right}) -> get(Index div 2, Right);
% get(_, _) -> nil.

% % spec append::a ternary_tree -> ternary_tree
append(Value, nil) -> update(Value, 0, {0, nil, nil, nil});
append(Value, Tree={Count, _, _, _}) -> update(Value, Count, Tree).

count(nil) -> 0;
count({Count, _, _, _}) -> Count.

value(nil) -> nil;
value({_,Value,_,_}) -> Value.

% update(Value, 0, nil) -> {1, Value, nil, nil};
% update(Value, 1, nil) -> {1, Value, nil, nil};
% update(New_Value, Index, {_, Value, Left, Right}) when (Index rem 2 == 1) ->
% 	New_Tree = update(New_Value, Index div 2, Right),
% 	{1 + count(Left) + count(New_Tree), Value, Left, New_Tree};
% update(New_Value, Index, {_, Value, Left, Right}) ->
% 	New_Tree = update(New_Value, Index div 2, Left),
% 	{1 + count(New_Tree) + count(Right), Value, New_Tree, Right}.

% get(0, {_, Value, _, _}) -> Value;
% get(1, {_, Value, _, _}) -> Value;
% get(Index, {_, _, _, Right}) when (Index rem 2 == 1) -> get(Index div 2, Right);
% get(Index, {_, _, Left, _}) -> get(Index div 2, Left).

update(Value, 0, nil) -> {1, Value, nil, nil};
update(Value, 1, nil) -> {1, Value, nil, nil};
update(Value, Index, {C, V, L, R}) when (Index rem 2 == 0) ->
	New_L = update(Value, Index div 2, L),
	{count(New_L) - count(L) + C, V, New_L, R};
update(Value, Index, {C, V, L, R}) ->
	New_R = update(Value, Index div 2, R),
	{count(New_R) - count(R) + C, V, L, New_R}.

get(_, nil) -> nil;
get(0, {_, _, L, _}) -> value(L);
get(1, {_, _, _, R}) -> value(R);
get(Index, {_, _, _, Right}) when (Index rem 2 == 1) -> get(Index div 2, Right);
get(Index, {_, _, Left, _}) -> get(Index div 2, Left).