-module (node).

-export ([node/2, walk/4]).

-define (DELAY_FACTOR, 100).

% Spwan a anonymous process which sends a delayed message to the nodes and
% simulate walking through the edge.
walk(Walker, From, To, Weight) ->
    spawn(
        fun() ->
            From ! {ant_choose, To, Weight},
            timer:sleep(Weight * ?DELAY_FACTOR),
            timer:sleep(Weight * ?DELAY_FACTOR),
            To ! {ant_choose, From, Weight},
            To ! {ask, Walker}
        end
    ).

% ----------------------------------------------------------- %
%                     Pheromone modifiers                     %
% ----------------------------------------------------------- %

% Generalized function to update the pheromone factor for a given node in the
% neighborhood with an anonymous function.
updatePheromone(_, _, [], _, Acc) ->
    lists:reverse(Acc);
updatePheromone(Node, Weight, [{Node, Weight, PF}|NBH], Fun, Acc) ->
    lists:reverse([{Node, Weight, Fun(PF)}|Acc], NBH);
updatePheromone(Node, Weight1, [{PID, Weight2, PF}|NBH], Fun, Acc) ->
    updatePheromone(Node, Weight1, NBH, Fun, [{PID, Weight2, PF}|Acc]).

% Applies a increment in the pheromones of the path for a given node in the
% neighborhood.
strengthenPheromone(Node, NBH, Weight) ->
    updatePheromone(Node, Weight, NBH, fun(X) -> X + 1 end, []).

% Applies a reduction in the pheromones of the path for a given node in the
% neighborhood.
weakenPheromone(Node, NBH, Weight) ->
    updatePheromone(Node, Weight, NBH,
        fun(X) ->
            if X > 1 ->
                X - 1;
            true ->
                X
            end
        end, []).

% Spawns a process which evaporate a pheromone in a given time.
evaporatePheromone(Node, NBR, Weight) ->
    spawn(
        fun() ->
            timer:sleep(10000),
            Node ! {evaporate, NBR, Weight}
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
        {ant_choose, Node, Weight} ->
            NewNBH = strengthenPheromone(Node, NBH, Weight),
            evaporatePheromone(self(), Node, Weight),
            node(NewNBH, Traffic);
        {evaporate, Node, Weight} ->
            NewNBH = weakenPheromone(Node, NBH, Weight),
            node(NewNBH, Traffic);
        {print} ->
            io:format("~p: { NBH: ~p, Traffic: ~p }~n", [self(), NBH, Traffic]),
            node(NBH, Traffic);
        _ -> node(NBH, Traffic)
    end.