% #!/usr/bin/env escript

-module (main).

-export ([init/1]).

-include_lib("eqc/include/eqc.hrl").

% ----------------------------------------------------------- %
%                   String helper functions                   %
% ----------------------------------------------------------- %

strip(S) -> re:replace(S, "\n", "", [global,{return,list}]).

split(S, Sep) -> string:tokens(S, Sep).

to_i(S) -> {I, _} = string:to_integer(S), I.

to_i_list(S) -> lists:map(fun (X) -> to_i(X) end, split(strip(S), " ")).

get_all_lines(Device, Acc) ->
    case io:get_line(Device, "") of
        eof  -> lists:reverse(Acc);
        Line -> get_all_lines(Device, [Line|Acc])
    end.

% ----------------------------------------------------------- %
%                 Graph initializer functions                 %
% ----------------------------------------------------------- %

newGraph(N, Edges) ->
    G = createGraph(N, []),
    addEdges(G, Edges),
    G.

createGraph(0, Acc) -> Acc;
createGraph(N, Acc) ->
    createGraph(N - 1, [spawn(node, node, [[],0])|Acc]).

addEdges(_, []) -> ok;
addEdges(Graph, [[X, Y, Weight]|Es]) ->
    Node_x = lists:nth(X, Graph),
    Node_y = lists:nth(Y, Graph),
    % Tell each node that they are neighbors
    Node_x ! {add, Node_y, Weight},
    Node_y ! {add, Node_x, Weight},
    addEdges(Graph, Es).

% ----------------------------------------------------------- %
%                        main program                         %
% ----------------------------------------------------------- %

init(String) ->
    {ok, Device} = file:open(String, [read]),
    Content = get_all_lines(Device, []),

    [[N|_]|Edges] = lists:map(fun (Ss) -> to_i_list(Ss) end, Content),
    G = newGraph(N, Edges),
    M = spawn(master, master, [N,G]),
    io:format("Graph: ~p~n", [G]),
    M.