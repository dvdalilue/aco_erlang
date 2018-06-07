-module (node).

-compile([export_all]).

newGraph(N, Edges) ->
    G = createGraph(N, []),
    addEdges(G, Edges),
    G.

addEdges(_, []) -> ok;
addEdges(Graph, [{X,Y}|Es]) ->
    Node_x = lists:nth(X, Graph),
    Node_y = lists:nth(Y, Graph),
    % Tell each node that they are neighbors
    Node_x ! {add, Node_y},
    Node_y ! {add, Node_x},
    addEdges(Graph, Es).

createGraph(0, Acc) -> Acc;
createGraph(N, Acc) ->
    createGraph(N - 1, [spawn(node, node, [[],0])|Acc]).

updatePheromone(_, [], _, Acc) ->
    lists:reverse(Acc);
updatePheromone(Node, [{Node, PF}|NBH], Fun, Acc) ->
    lists:reverse([{Node, Fun(PF)}|Acc], NBH);
updatePheromone(Node,  [{PID, PF}|NBH], Fun, Acc) ->
    updatePheromone(Node, NBH, Fun, [{PID, PF}|Acc]).

strengthenPheromone(Node, NBH) ->
    updatePheromone(Node, NBH, fun(X) -> X + 1 end, []).

node(NBH, Traffic) ->
    receive
        {add, NBR} ->
            node([{NBR, 1}|NBH], Traffic);
        {antPassBy, Node} ->
            newNBH = strengthenPheromone(Node, NBH),
            node(newNBH, Traffic + 1);
        {get_nbh} ->
            io:format("NBH: ~p~n", [NBH]),
            node(NBH, Traffic);
        _ -> node(NBH, Traffic)
    end.