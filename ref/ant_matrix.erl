-module(ant).
-export([indicesList/1, matrix/1, printMatrix/1, randomMatrix/3,
	wrappedPath/1, pathLength/2, updatePher/3, evaporatePher/3,
	doSumWeight/7, findSumWeight/9,genPathRecurse/8,genPath/4,
	bestPathRecurse/9,main/0,parallelWork/6,parallelMain/0]).

%% ant.erl
%% Eric Rollins 2006

%% Written for open-source Erlang, http://www.erlang.org/
%%
%% This program generates a random array of distances between cities, then uses
%% Ant Colony Optimization to find a short path traversing all the cities --
%% the Travelling Salesman Problem.
%%
%% In this version of Ant Colony Optimization each ant starts in a random city.
%% Paths are randomly chosed with probability inversely proportional to to the
%% distance to the next city.  At the end of its travel the ant updates the
%% pheromone matrix with its path if this path is the shortest one yet found.
%% The probability of later ants taking a path is increased by the pheromone
%% value on that path.  Pheromone values evaporate (decrease) over time.
%%
%% In this impementation weights between cities actually represent
%% (maxDistance - dist), so we are trying to maximize the score.
%%
%% Usage: ant seed iterations
%%    seed         seed for random number generator (1,2,3...).
%%                 This seed controls the city distance array.  Parallel
%%                 evalations have their seed values fixed (1,2) so each will
%%                 produce a different result.
%%    iterations   number of ants to be run.
%%
%% The following should be parameters, but are constants for now.
%%    boost        pheromone boost for best path.  5 appears good.
%%                 0 disables pheromones, providing random search.
%%    cities       number of cities.
%%
%%   This program spawns two erlang processes, which perform the calulation 
%%   with different seed values.  They communicate using the standard erlang
%%   message passing mechanism.

% Matrix is square two dimensional array.
indicesList(N) -> [{X, Y} || X <- lists:seq(0,N-1), Y <- lists:seq(0,N-1)].

matrix(N) ->
    Ma = dict:new(),
    F = fun(Key,M) ->
		dict:store(Key,0,M)
	end,
    Indices = indicesList(N),
    lists:foldl(F,Ma,Indices).

randomMatrix(N, UpperBound,Seed) ->
    Ma = dict:new(),
    random:seed(Seed,Seed+1,Seed+2),
    F = fun(Key,M) ->
		R = random:uniform() * UpperBound, 
		dict:store(Key,R,M)
	end,
    Indices = indicesList(N),
    lists:foldl(F,Ma,Indices).
    
printMatrix(M) ->		
    Keys = dict:fetch_keys(M),
    F = fun(Key) ->
		io:write(Key),
		io:fwrite(":",[]),
		io:write(dict:fetch(Key,M)),
		io:fwrite(" ",[])
	end,
    lists:foreach(F,Keys),
    io:fwrite("\n",[]).
    
% Path with first city in path moved to end.
wrappedPath(Path) ->
    lists:append(tl(Path), [hd(Path)]).

pathLength(Cities,Path) ->
    F = fun(X) ->
		dict:fetch(X,Cities)
	end,
    Pairs = lists:zip(Path, wrappedPath(Path)),
    lists:sum(lists:map(F,Pairs)).

% Boosts pheromones for cities on path.
updatePher(Pher, Path, Boost) ->
    WPath = wrappedPath(Path),
    Pairs = lists:zip(Path,WPath),
    F = fun(Key,M) ->
		New = dict:fetch(Key,M) + Boost,
		dict:store(Key,New,M)
	end,
    lists:foldl(F,Pher,Pairs).

evaporatePher(Pher,MaxIter, Boost) ->
    Keys = dict:fetch_keys(Pher),
    Decr = Boost / MaxIter,
    F = fun(Key, M) ->
		OldV = dict:fetch(Key,M),
		if 
		    OldV >= Decr ->
			NewV = OldV - Decr;
		    true ->
			NewV = 0.0
		end,
		dict:store(Key, NewV, M)
	end,
    lists:foldl(F,Pher,Keys).

% Sum weights for all paths to cities adjacent to current.
doSumWeight(City,_,NumCities,_,_,_,RunningTotal) when City >= NumCities ->
    RunningTotal;
doSumWeight(City,Cities,NumCities,Pher,Used,Current,RunningTotal) ->
    InUsed = dict:is_key(City,Used),
    if
	InUsed ->
	    Incr = 0.0;
	 true ->
	    Incr = dict:fetch({Current,City},Cities) * 
		(1.0 + dict:fetch({Current,City},Pher))
    end,
    doSumWeight((City+1), Cities, NumCities,Pher,Used,Current,
		(RunningTotal+Incr)).

% Returns city at SoughtTotal.
findSumWeight(City,NextCity,NumCities,_,_,_,_,_,_) when City >= NumCities ->
    NextCity;
findSumWeight(City,NextCity,NumCities,Cities,Pher,Used,Current,SoughtTotal,
	      RunningTotal) ->
    InUsed = dict:is_key(City,Used),
    if
	(not InUsed) and  (RunningTotal >= SoughtTotal) ->
	    NextCity;
	true ->
	    if
		InUsed ->
		    Incr = 0.0,
		    NextNextCity = NextCity;
		true  ->
		    Incr = dict:fetch({Current,City},Cities) * 
			(1.0 + dict:fetch({Current,City},Pher)),
		    NextNextCity = City
	    end,
	    findSumWeight((City+1),NextNextCity,NumCities,Cities,Pher,Used,
			  Current,SoughtTotal,(RunningTotal+Incr))
    end.

% Returns {path, newrGen}
genPathRecurse(_,NumCities,_,_,SizeUsed,Path,_,RGen) 
  when SizeUsed >= NumCities ->
    {Path,RGen};
genPathRecurse(Cities,NumCities,Pher,Used,SizeUsed,Path,Current,RGen) ->
    SumWeight = doSumWeight(0,Cities,NumCities,Pher,Used,Current,0.0),
    {TmpRndValue, NewRGen} = random:uniform_s(RGen),
    RndValue = TmpRndValue * SumWeight,
    NextCity = findSumWeight(0,0,NumCities,Cities,Pher,Used,Current,RndValue,
			     0.0),
    NextPath = lists:append(Path,[NextCity]),
    NextUsed = dict:store(NextCity,NextCity,Used),
    genPathRecurse(Cities,NumCities,Pher,NextUsed,(SizeUsed+1),NextPath,
		   NextCity,NewRGen).

% Returns {path, newrGen}
genPath(Cities,NumCities,Pher,RGen) ->
    {Current,NewRGen} = random:uniform_s((NumCities-1),RGen),
    Used = dict:store(Current,Current,dict:new()),
    Path = [Current],
    genPathRecurse(Cities,NumCities,Pher,Used,1,Path,Current,NewRGen).

% Returns path
bestPathRecurse(_,_,_,_,_,RemainingIter,BestPathSoFar,_,_)  
  when (RemainingIter =< 0) ->
    BestPathSoFar;
bestPathRecurse(Cities,NumCities,Pher,RGen,MaxIter,RemainingIter,
		BestPathSoFar,BestLength,Boost) ->
    {Path,NewRGen} = genPath(Cities,NumCities,Pher,RGen),
    PathLen = pathLength(Cities,Path),
    if
	% Remember we are trying to maximize score.
	PathLen > BestLength ->
	    NewBestPath = Path,
	    NewBestLength = PathLen,
	    NewPher = updatePher(Pher,Path,Boost);
	true ->
	    NewBestPath = BestPathSoFar,
	    NewBestLength = BestLength,
	    NewPher = Pher
    end,
    EvaporatedPher = evaporatePher(NewPher,MaxIter,Boost),
    bestPathRecurse(Cities,NumCities,EvaporatedPher,NewRGen,MaxIter,
		    (RemainingIter-1),NewBestPath,NewBestLength,Boost).

% Returns path.
bestPath(Cities,NumCities,RSeed,NumIter,Boost) ->
    RGen = random:seed(RSeed,RSeed+1,RSeed+2),
    Pher = matrix(NumCities),
    bestPathRecurse(Cities,NumCities,Pher,RGen,NumIter,NumIter,[],0.0,Boost).

main() ->
    Seed = 1,
    Boost = 5,
    Iter = 4000,
    NumCities = 22,
    Cities = randomMatrix(NumCities,100,Seed),
    BestPath = bestPath(Cities,NumCities,1,Iter,Boost),
    io:write(BestPath),
    io:fwrite(" : ",[]),
    io:write(pathLength(Cities,BestPath)),
    io:fwrite("\n",[]).

% returns {Path,PathLength}
parallelWork(CitySeed,WorkSeed,Boost,Iter,NumCities,ParentPID) ->
    Cities = randomMatrix(NumCities,100,CitySeed),
    % Re-seed in thread appears to be ignored, so pop one value.
    if
	(WorkSeed > 1) ->
	    _ = random:uniform();
	true ->
	    _ = 0.0
    end,    
    BestPath = bestPath(Cities,NumCities,WorkSeed,Iter,Boost),
    BestPathLength = pathLength(Cities,BestPath),
    % Send results back to parent process.
    ParentPID ! {BestPath,BestPathLength}.

parallelMain() ->
    Seed = 1,
    Boost = 5,
    Iter = 100,
    NumCities = 200,
    % Spawn child processes.
    % Pid1 = 
    spawn(ant,parallelWork,[Seed,1,Boost,Iter,NumCities,self()]),
    % Pid2 = 
    spawn(ant,parallelWork,[Seed,2,Boost,Iter,NumCities,self()]),
    % Wait for replies from child processes.
    receive
	{BestPath1,BestPathLength1} ->
	    io:write(BestPath1),
	    io:fwrite(" : ",[]),
	    io:write(BestPathLength1),
	    io:fwrite("\n",[])
    end,
    receive
 	{BestPath2,BestPathLength2} ->
 	    io:write(BestPath2),
 	    io:fwrite(" : ",[]),
 	    io:write(BestPathLength2),
 	    io:fwrite("\n",[])
     end.
