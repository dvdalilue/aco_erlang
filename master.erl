-module(master).

-export ([master/2]).

% Map exit signal to all ants within an array
exterminate(Ants) ->
    lists:map(fun(A) -> A ! {die} end, Ants).

% Map init message to all ants within an array
awakening(Ants) ->
    lists:map(fun(A) -> A ! {init} end, Ants).

wakeUp(0, _, _, _, Ants) when is_list(Ants) -> Ants;
wakeUp(N, Source, Nodes, Target, Ants) when
    is_integer(N),
    is_integer(Nodes),
    is_list(Ants), N > 0 ->
        A = spawn(ant, ant, [Source, [], Nodes, Target]),
        wakeUp(N-1, Source, Nodes, Target, [A|Ants]);
wakeUp(_, _, _, _, _) -> throw("Cannot awake a non natural number of ants").

master(N, NodeList) -> master(N, NodeList, []).
master(N, NodeList, Ants) ->
    receive
        {init} ->
            awakening(Ants),
            master(N, NodeList, Ants);
        {createAnts, N_ants, Source, Target} ->
            exterminate(Ants),
            NewAnts = wakeUp(N_ants, Source, N, Target, []),
            master(N, NodeList, NewAnts);
        {killAnts} ->
            exterminate(Ants),
            master(N, NodeList, []);
        {printNodes} ->
            io:format("Graph: ", []),
            lists:map(fun (Nd) -> Nd ! {print} end, NodeList),
            master(N, NodeList, Ants);
        {printAnts} ->
            io:format("Ants: ", []),
            lists:map(fun (A) -> A ! {print} end, Ants),
            master(N, NodeList, Ants);
        _ ->
            master(N, NodeList, Ants)
    end.
