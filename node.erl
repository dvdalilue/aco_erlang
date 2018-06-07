-module (node).

-export ([node/2, walk/4]).

-define (DELAY_FACTOR, 500).

% Spwan a anonymous process which sends a delayed message to the nodes and
% simulate walking through the edge.
walk(Walker, From, To, Weight) ->
    spawn(
        fun() ->
            From ! {ant_choose, To},
            timer:sleep(Weight * ?DELAY_FACTOR),
            timer:sleep(Weight * ?DELAY_FACTOR),
            To ! {ant_choose, From},
            To ! {ask, Walker}
        end
    ).

% ----------------------------------------------------------- %
%                     Pheromone modifiers                     %
% ----------------------------------------------------------- %

% Generalized function to update the pheromone factor for a given node in the
% neighborhood with an anonymous function.
updatePheromone(_, [], _, Acc) ->
    lists:reverse(Acc);
updatePheromone(Node, [{Node, Weight, PF}|NBH], Fun, Acc) ->
    lists:reverse([{Node, Weight, Fun(PF)}|Acc], NBH);
updatePheromone(Node, [{PID, Weight, PF}|NBH], Fun, Acc) ->
    updatePheromone(Node, NBH, Fun, [{PID, Weight, PF}|Acc]).

% Applies a increment in the pheromones of the path for a given node in the
% neighborhood.
strengthenPheromone(Node, NBH) ->
    updatePheromone(Node, NBH, fun(X) -> X + 1 end, []).

% Applies a reduction in the pheromones of the path for a given node in the
% neighborhood.
weakenPheromone(Node, NBH) ->
    updatePheromone(Node, NBH,
        fun(X) ->
            if X > 1 ->
                X - 1;
            true ->
                X
            end
        end, []).

% Spawns 
evaporatePheromone(Node, NBR) ->
    spawn(
        fun() ->
            timer:sleep(5000),
            Node ! {evaporate, NBR}
        end).

% ----------------------------------------------------------- %
%                        Node process                         %
% ----------------------------------------------------------- %

node(NBH, Traffic) ->
    receive
        {add, NBR, Weight} ->
            node([{NBR, Weight, 1}|NBH], Traffic);
        {ask, Ant} ->
            Ant ! {goto, NBH},
            node(NBH, Traffic + 1);
        {ant_choose, Node} ->
            NewNBH = strengthenPheromone(Node, NBH),
            evaporatePheromone(self(), Node),
            node(NewNBH, Traffic);
        {evaporate, Node} ->
            NewNBH = weakenPheromone(Node, NBH),
            node(NewNBH, Traffic);
        {print} ->
            io:format("~p: { NBH: ~p, Traffic: ~p }~n", [self(), NBH, Traffic]),
            node(NBH, Traffic);
        _ -> node(NBH, Traffic)
    end.