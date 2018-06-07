-module (ant).

-export ([ant/3]).

-import (node, [walk/4]).

% The inspiration of this function comes from the 'frequency'
% generator in QuickCheck, following the same intentional definition. Supported
% by next function definition 'edgesReplication'. In the first case, of arity 1,
% use the first version (arity 2) of 'edgesReplication' and so on.

% posibleEdges/1
posibleEdges(Edges) -> edgesReplication(Edges, []).

% posibleEdges/2
posibleEdges(Edges, Visited) -> edgesReplication(Edges, Visited, []).

% Takes a list of 3-tuples {Neighbor, Weight, Pheromone_factor}, an accumulator
% of duplication and returns a list of every tuple multiplyed by its 'PF'
% (Pheromone factor), also removing the factor from the tuple leaving a 2-tuple
% {Neighbor, Weight}. The case of arity 3, checks if the current recursion
% neighbour (NBR) belongs to the list of visited.

% edgesReplication/2
edgesReplication([], ACC) -> lists:concat(ACC);
edgesReplication([{NBR, Weight, PF}|NBH], ACC) ->
    edgesReplication(NBH, [lists:duplicate(PF, {NBR, Weight})|ACC]).

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

%% @spec ant(Node::pid(), Visited::[pid()], Ns::int()) -> ()
ant(Node, Visited, Ns) ->
    receive
        {init} ->
            Node ! {ask, self()},
            ant(Node, Visited, Ns);
        {print} ->
            io:format("~p : ~p~n", [self(), Node]),
            ant(Node, Visited, Ns);
        {goto, NBH} when is_list(NBH) ->
            % CurrentVisited = [Node|Visited],
            % Edges = posibleEdges(NBH,CurrentVisited),
            Edges = posibleEdges(NBH),
            % if
            %     Edges == [] ->
            %         self() ! {init},
            %         if
            %             length(CurrentVisited) == Ns ->
            %                 ant(Node, [], Ns);
            %             true ->
            %                 ant(lists:last(Visited), [], Ns)
            %         end;
            %     true -> ok
            % end,
            {NewNode, Weight} = chooseOneOf(Edges),
            % io:format("From ~p to ~p~n", [Node,NewNode]),
            walk(self(), Node, NewNode, Weight),
            ant(NewNode, Visited, Ns);
            % ant(NewNode, CurrentVisited, Ns);
        {die} ->
            exit(kill);
        _ ->
            ant(Node, Visited, Ns)
    end.
