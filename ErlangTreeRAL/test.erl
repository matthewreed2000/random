-module(test).
-export([test/0]).

% Performance testing functions
start_perf() ->
    eprof:start(),
    eprof:start_profiling([self()]).

stop_perf(Title) ->
    io:format("Perf (~p): ~n",[Title]),
    eprof:stop_profiling(),
    eprof:analyze(total),
    eprof:stop().

% Build both and test both RAL and Ternary Tree
test() ->
	% Make a small Ternary Tree
	T1 = ternary_tree:append(a, nil),
	T2 = ternary_tree:append(b, T1),
	T3 = ternary_tree:append(c, T2),
	T4 = ternary_tree:append(d, T3),
	T5 = ternary_tree:append(e, T4),
	T6 = ternary_tree:append(f, T5),
	T7 = ternary_tree:append(g, T6),
	T8 = ternary_tree:append(h, T7),
	T9 = ternary_tree:append(i, T8),
	T10 = ternary_tree:append(j, T9),
	T11 = ternary_tree:append(k, T10),
	T12 = ternary_tree:append(l, T11),
	T13 = ternary_tree:append(m, T12),
	T14 = ternary_tree:append(n, T13),
	T15 = ternary_tree:append(o, T14),
	T16 = ternary_tree:append(p, T15),
	TF = ternary_tree:append(q, T16),

	io:format("~p~n", [TF]),

	% Make sure that I get what I expected
	io:format("~p~n", [ternary_tree:get(0, TF)]),
	io:format("~p~n", [ternary_tree:get(1, TF)]),
	io:format("~p~n", [ternary_tree:get(2, TF)]),
	io:format("~p~n", [ternary_tree:get(3, TF)]),
	io:format("~p~n", [ternary_tree:get(4, TF)]),
	io:format("~p~n", [ternary_tree:get(5, TF)]),
	io:format("~p~n", [ternary_tree:get(6, TF)]),
	io:format("~p~n", [ternary_tree:get(7, TF)]),
	io:format("~p~n", [ternary_tree:get(8, TF)]),
	io:format("~p~n", [ternary_tree:get(9, TF)]),
	io:format("~p~n", [ternary_tree:get(10, TF)]),
	io:format("~p~n", [ternary_tree:get(11, TF)]),
	io:format("~p~n", [ternary_tree:get(12, TF)]),
	io:format("~p~n", [ternary_tree:get(13, TF)]),
	io:format("~p~n", [ternary_tree:get(14, TF)]),
	io:format("~p~n", [ternary_tree:get(15, TF)]),
	io:format("~p~n", [ternary_tree:get(16, TF)]),
	io:format("~p~n", [ternary_tree:get(17, TF)]),

	% Create Erlang list of 1,000,000 items
    List = lists:seq(0, 999999),

    % Create RAL of 1,000,000 items
    start_perf(),
    RAL = lists:foldl(fun ral:prepend/2, [], List),
    stop_perf("RAL build"),

    % Create Tree of 1,000,000 items
    start_perf(),
    Tern = lists:foldl(fun ternary_tree:append/2, nil, List),
    stop_perf("Tern build"),

    % Get index 999999 in RAL
    start_perf(),
    0 = ral:lookup(999999, RAL),
    stop_perf("RAL lookup last item"),

    % Get index 999999 in Ternary Tree
    start_perf(),
    999999 = ternary_tree:get(999999, Tern),
    stop_perf("Tern lookup last item"),

	pass.