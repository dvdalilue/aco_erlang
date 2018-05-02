-module (ant).

%-export ([function/arity]).

-compile([export_all]).

-include_lib("eqc/include/eqc.hrl").

exp() -> [
    [{3,2,1},{5,1,1}],
    [{3,3,1},{4,2,1},{5,1,1}],
    [{1,2,1},{2,3,1},{4,1,1}],
    [{2,2,1},{3,1,1},{5,3,1}],
    [{1,1,1},{2,1,1},{4,3,1}]
].

awakening() ->
    Master = spawn(ant, master, [exp()]),
    Ants = wakeUp(Master, 100, 5, []),
    {Master, Ants}.

wakeUp(_, 0, _, Ants) when is_list(Ants) -> Ants;
wakeUp(G, N, Nodes, Ants) when is_integer(N), is_list(Ants), N > 0 ->
    A = spawn(ant, ant, [G,1,[],Nodes]),
    A ! {init},
    wakeUp(G, N-1, Nodes, [A|Ants]);
wakeUp(_ , _, _, _) -> throw("Cannot awake a negative number of ants").

exterminate(Ants) ->
    lists:map(fun(A) -> exit(A,kill) end, Ants).

edgesReplication([], ACC) -> lists:concat(ACC);
edgesReplication([{NBR, Cost, PF}|NBH], ACC) ->
    edgesReplication(NBH, [lists:duplicate(PF, {NBR, Cost})|ACC]).

edgesReplication([], _, ACC) -> lists:concat(ACC);
edgesReplication([{NBR, Cost, PF}|NBH], Visited ,ACC) ->
    case lists:member(NBR,Visited) of
        true ->
            edgesReplication(NBH, Visited, ACC);
        _ ->
            edgesReplication(NBH, Visited,
                [lists:duplicate(PF, {NBR, Cost})|ACC])
    end.

posibleEdges(Edges) -> edgesReplication(Edges, []).

posibleEdges(Edges, Visited) -> edgesReplication(Edges, Visited, []).

chooseOneOf([]) -> nil;
chooseOneOf(Xs) ->
    lists:nth(rand:uniform(length(Xs)), Xs).

setNth(I, List, F) -> setNthAux(I, List, F, []).

setNthAux(1, [X|Xs], F, Acc) -> lists:reverse([F(X)|Acc], Xs);
setNthAux(I, [X|Xs], F, Acc) -> setNthAux(I-1, Xs, F, [X|Acc]).

updateNBH(E, NBH, Fun) -> updateNBHAux(E, NBH, Fun, []).

updateNBHAux(_, [], _, Acc) ->
    lists:reverse(Acc);
updateNBHAux(E, [{E, Cost, PF}|NBH], Fun, Acc) ->
    lists:reverse([{E, Cost, Fun(PF)}|Acc], NBH);
updateNBHAux(E, [{NBR, Cost, PF}|NBH], Fun, Acc) ->
    updateNBHAux(E, NBH, Fun, [{NBR, Cost, PF}|Acc]).

ask(Master, Ant, Node, Cost) ->
    spawn(
        fun() -> 
            timer:sleep(Cost * 500),
            Master ! {neighbours, Ant, Node}
        end
    ).

%% @spec ant(Master::PID, Node::Int, Visited::List) -> ()
%%       List = [Int]
%%       Int = int()
%%       PID = pid()
ant(Master, Node, Visited, Ns) ->
    receive
        {init} ->
            ask(Master, self(), Node, 0),
            ant(Master, Node, Visited, Ns);
        {goto, L} when is_list(L) ->
            CurrentVisited = [Node|Visited],
            Edges = posibleEdges(L,CurrentVisited),
            if
                Edges == [] ->
                    self() ! {init},
                    if
                        length(CurrentVisited) == Ns ->
                            ant(Master, Node, [], Ns);
                        true ->
                            ant(Master, lists:last(Visited), [], Ns)
                    end;
                true -> ok
            end,
            {NewNode,Cost} = chooseOneOf(Edges),
            % io:format("From ~p to ~p~n", [Node,NewNode]),
            timer:sleep(Cost * 500),
            Master ! {increasePF, Node, NewNode},
            ask(Master, self(), NewNode, Cost),
            ant(Master, NewNode, CurrentVisited, Ns);
        _ ->
            ant(Master, Node, Visited, Ns)
    end.

updatePheromone(From, To, Graph, Fun) ->
    AuxG =
        setNth(From, Graph,
            fun(F) ->
                updateNBH(To, F, Fun)
            end),
    NewG =
        setNth(To, AuxG,
            fun(T) ->
                updateNBH(From, T, Fun)
            end),
    NewG.

strengthenPheromone(From, To, AL) ->
    updatePheromone(From, To, AL, fun(X) -> X + 1 end).

weakenPheromone(From, To, AL) ->
    updatePheromone(From, To, AL,
        fun(X) ->
            if X > 1 ->
                X - 1;
            true ->
                X
            end
        end).

evaporatePheromone(Master, From, To) ->
    spawn(
        fun() ->
            timer:sleep(1000),
            Master ! {decreasePF,From,To}
        end).

master(AdjacencyList) ->
    receive
        {neighbours, Sender,X} ->
            Sender ! {goto,lists:nth(X,AdjacencyList)},
            master(AdjacencyList);
        {increasePF, From, To} ->
            NewAL = strengthenPheromone(From, To, AdjacencyList),
            evaporatePheromone(self(), From, To),
            master(NewAL);
        {decreasePF, From, To} ->
            NewAL = weakenPheromone(From, To, AdjacencyList),
            master(NewAL);
        {get} ->
            io:format("Graph: ~p", [AdjacencyList]),
            master(AdjacencyList);
        _ ->
            master(AdjacencyList)
    end.