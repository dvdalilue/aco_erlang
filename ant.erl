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
    Ants = wakeUp(Master, 10, []),
    {Master, Ants}.

wakeUp(_,0,Ants) when is_list(Ants) -> Ants;
wakeUp(G,N,Ants) when is_integer(N), is_list(Ants), N > 0 ->
    A = spawn(ant,ant,[G,1,[]]),
    A ! {init},
    wakeUp(G,N-1,[A|Ants]);
wakeUp(_,_,_) -> throw("Cannot awake a negative number of ants").

exterminate(Ants) ->
    lists:map(fun(A) -> exit(A,kill) end, Ants).

edgesReplication([],ACC) -> lists:concat(ACC);
edgesReplication([{NBR,Cost,PF}|NBH],ACC) ->
    edgesReplication(NBH,[lists:duplicate(PF,{NBR,Cost})|ACC]).

posibleEdges(Edges) -> edgesReplication(Edges,[]).

chooseOneOf(X) ->
    lists:nth(rand:uniform(length(X)), X).

setNth(I, List, F) -> setNthAux(I, List, F, []).

setNthAux(1, [X|Xs], F, Acc) -> lists:reverse([F(X)|Acc],Xs);
setNthAux(I, [X|Xs], F, Acc) -> setNthAux(I-1, Xs, F, [X|Acc]).

updateNBH(E, NBH) -> updateNBHAux(E, NBH, []).

updateNBHAux(_, [], Acc) ->
    lists:reverse(Acc);
updateNBHAux(E, [{E, Cost, PF}|NBH], Acc) ->
    lists:reverse([{E, Cost, PF+1}|Acc], NBH);
updateNBHAux(E, [{NBR, Cost, PF}|NBH], Acc) ->
    updateNBHAux(E, NBH, [{NBR, Cost, PF}|Acc]).

%% @spec ant(Graph::PID, Node::Int, Visited::List) -> ()
%%       List = [Int]
%%       Int = int()
%%       PID = pid()
ant(Graph,Node,Visited) ->
    receive
        {init} ->
            Graph ! {neighbours,self(),Node},
            ant(Graph,Node,Visited);
        {goto,L} when is_list(L) ->
            {NewNode,Cost} = chooseOneOf(posibleEdges(L)),
            io:format("From ~p to ~p~n",[Node,NewNode]), 
            timer:sleep(Cost * 1000),
            Graph ! {updatePF,Node,NewNode},
            PID = self(),
            spawn(
                fun() -> 
                    timer:sleep(Cost * 1000),
                    Graph ! {neighbours,PID,NewNode}
                end
            ),
            ant(Graph,NewNode,Visited);
        {goto,N} when is_integer(N) ->
            timer:sleep(2000),
            io:format("------> From ~p to ~p~n",[Node,N]), 
            Graph ! {neighbours,self(),Node}, 
            ant(Graph,N,Visited);
        _ ->
            ant(Graph,Node,Visited)
    end.

master(AdjacencyList) ->
    receive
        {neighbours,Sender,X} ->
            Sender ! {goto,lists:nth(X,AdjacencyList)},
            master(AdjacencyList);
        {updatePF,From,To} ->
            AuxAL = 
                setNth(From, AdjacencyList, fun(X) -> updateNBH(To, X) end),
            NewAL = 
                setNth(To, AuxAL, fun(X) -> updateNBH(From, X) end),
            master(NewAL);
        {getGraph} ->
            io:format("Graph: ~p",[AdjacencyList]), 
            master(AdjacencyList);
        _ ->
            master(AdjacencyList)
    end.