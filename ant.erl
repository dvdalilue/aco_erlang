-module (ant).

-export ([ant/4]).

-import (node, [walk/4]).

% The inspiration of this function comes from the 'frequency'
% generator in QuickCheck, following the same intentional definition. Supported
% by next function definition 'edgesReplication'. In the first case, of arity 1,
% use the first version (arity 2) of 'edgesReplication' and so on.

% posibleEdges/1
% posibleEdges(Edges) -> edgesReplication(Edges, []).

% posibleEdges/2
posibleEdges(Edges, Visited) -> edgesReplication(Edges, Visited, []).

% Takes a list of 3-tuples {Neighbor, Weight, Pheromone_factor}, an accumulator
% of duplication and returns a list of every tuple multiplyed by its 'PF'
% (Pheromone factor), also removing the factor from the tuple leaving a 2-tuple
% {Neighbor, Weight}. The case of arity 3, checks if the current recursion
% neighbour (NBR) belongs to the list of visited.

% edgesReplication/2
% edgesReplication([], ACC) -> lists:concat(ACC);
% edgesReplication([{NBR, Weight, PF}|NBH], ACC) ->
%     edgesReplication(NBH, [lists:duplicate(PF, {NBR, Weight})|ACC]).

% edgesReplication/3
edgesReplication([], _, ACC) -> lists:concat(ACC);
edgesReplication([{NBR, Weight, PF}|NBH], Visited ,ACC) ->
    case lists:member(NBR,Visited) of
        true ->
            edgesReplication(NBH, Visited, ACC);
        _ ->
            edgesReplication(NBH, Visited,
                [lists:duplicate(PF, {NBR, Weight})|ACC])
    end.

% Inspired by the function elements/1 of QuickCheck. Choose one term randomly
% from a list.
chooseOneOf([]) -> nil;
chooseOneOf(Xs) ->
    lists:nth(rand:uniform(length(Xs)), Xs).

%% @spec ant(Node::pid(), Visited::[pid()], Ns::int(), Target::pid()) -> ()
ant(Node, Visited, Ns, Target) ->
    receive
        {init} ->
            Node ! {ask, self()},
            ant(Node, Visited, Ns, Target);
        {print} ->
            io:format("~p : ~p~n", [self(), Node]),
            ant(Node, Visited, Ns, Target);
        {goto, NBH} when is_list(NBH) ->
            CurrentVisited = [Node|Visited],
            Banned = [Target|CurrentVisited],
            Edges = posibleEdges(NBH, Banned),
            if Edges == [] ->
                if length(Banned) == Ns ->
                    {_, W} = chooseOneOf(
                        posibleEdges(NBH, CurrentVisited)),
                    walk(self(), Node, Target, W),
                    % Go back with food
                    ant(Target, [], Ns, lists:last(CurrentVisited))
                end,
                [Previous|_] = Visited,
                Previous ! {evaporate, Node},
                Node ! {evaporate, Previous},
                self() ! {init},
                % Respawn back to source (ant died)
                ant(lists:last(CurrentVisited), [], Ns, Target)
            end,
            {NewNode, Weight} = chooseOneOf(Edges),
            walk(self(), Node, NewNode, Weight),
            ant(NewNode, CurrentVisited, Ns, Target);
        {die} ->
            exit(kill);
        _ ->
            ant(Node, Visited, Ns, Target)
    end.
