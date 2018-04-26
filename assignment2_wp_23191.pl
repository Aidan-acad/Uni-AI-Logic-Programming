% candidate_number(12345).

% Find hidden identity by repeatedly calling agent_ask_oracle(oscar,o(1),link,L)
% find_identity(-A)
find_identity(A):-
  (
    part_module(2)   -> find_identity_2(A),!
  ; otherwise -> find_identity_o(A)
  ).

% loop through actors, and their list of links and compare
% do this until only one actor remains
find_identity_2(A):-
  agent_ask_oracle(oscar, o(1), link, L),
  bagof(X, actor(X), Xs),
  find_identity_2(A, [], Xs, L).
find_identity_2(X, [A|[]], [], Lt):-
  X = A.
find_identity_2(X, As, [], Lt):-
  agent_ask_oracle(oscar, o(1), link, L),
  find_identity_2(X, [], As, L).
find_identity_2(X, As, [B|Bs], Lt):-
  wp(B, WT),
  bagof(L, wt_link(WT, L), Ls),
  (
    memberchk(Lt, Ls) -> find_identity_2(X, [B|As], Bs, Lt)
  ; otherwise -> find_identity_2(X, As, Bs, Lt)
  ).

getPathCost(S, [], CB, CB).
getPathCost(S, [(X, Pos)|Rest], (Y, Path), FB):-
  solve_task_bt(go(Pos), [[c(0,0,S), []]], _, _, _, _, _, BackPath),!,
  write(found_path),nl,
  length(Path, L1),
  length(BackPath, L2),
  (
    Y < 0 -> getPathCost(S, Rest, (X, BackPath), FB)
  ; L1 > L2 -> getPathCost(S, Rest, (X, BackPath), FB)
  ; otherwise -> getPathCost(S, Rest, (Y, Path), FB)
  ).
  

% this finds nearest oracle, checks you can go there with ur
% energy, then gets a link from that oracle
goToNearOracle(S, O, C, L):-
  getPathCost(S, O, (-1, []), CB),
  CB = [ID|Path],
  Path = [End|Tail],
  write(CB),nl,
  getPathCost(End, C, (-1, []), CB2).
  % check can reach nearest charge from this point
  % take path from cb to oracle
  % query oracle to get a link

find_identity_o(S, X, [A|[]], [], Lt, O, C):-
  X = A.
% repeat the link function thing here
find_identity_o(S, X, As, [], Lt, O, C):-
  goToNearOracle(S, O, C, L),
  find_identity_o(S, X, [], As, L, O, C).
find_identity_o(S, X, As, [B|Bs], Lt, O, C):-
  wp(B, WT),
  bagof(L, wt_link(WT, L), Ls),
  (
    memberchk(Lt, Ls) -> find_identity_o(S, X, [B|As], Bs, Lt, O, C)
  ; otherwise -> find_identity_o(S, X, As, Bs, Lt, O, C)
  ).

find_identity_o(A):-
  find_oracles(Oracles),
  find_stations(Stations),
  my_agent(Agent),
  query_world(agent_current_position, [Agent, S]),
  bagof(X, actor(X), Actors),
  goToNearOracle(S, Oracles, Stations, L),
  find_identity_o(S, A, [], Actors, L, Oracles, Stations).

flood_check([],_,_,_,Acc,Acc).
flood_check(Targets,Visited,LastRound,[],Acc,Result) :-
  append(LastRound, Visited, NewVisited),
  flood_expand(Targets,NewVisited,LastRound,[],Acc,Result).
flood_check(Targets,Visited,LastRound,Remaining,Acc,Result) :-
  Remaining = [Cell|Rest],
  flood_check_targets(Targets,Cell,Acc,NewAcc,Targets,NewTargets),
  flood_check(NewTargets,Visited,LastRound,Rest,NewAcc,Result).

flood_check_targets([],_,PAcc,PAcc,TDec,TDec).
flood_check_targets(ToCheck,Cell,PAcc,PResult,TDec,TResult) :-
  ToCheck = [Target|Rest],
  (
    map_adjacent(Cell,_,Target) -> write('Found: '),write(Target),nl,delete(TDec,Target,NewTDec), flood_check_targets(Rest,Cell,[(Target|Cell)|PAcc],PResult,NewTDec,TResult)
  ; otherwise -> flood_check_targets(Rest,Cell,PAcc,PResult,TDec,TResult)
  ).

flood_expand(Targets,Visited,[],NewArea,Acc,Result) :-
  %remove duplicates
  sort(NewArea, X),
  flood_check(Targets,Visited,X,X,Acc,Result).
flood_expand(Targets,Visited,Remaining,NewArea,Acc,Result) :-
  Remaining = [Cell|Rest],
  findall(P, (map_adjacent(Cell,P,empty), \+ memberchk(P,Visited)), Ps),
  append(Ps, NewArea, NewNewArea),
  flood_expand(Targets,Visited,Rest,NewNewArea,Acc,Result).

content_search(Targets, Results):-
  my_agent(A),
  query_world(agent_current_position, [A, P]),
  flood_check(Targets,[],[P],[P],[],Results).

is_oracle((o(_)|_)).
is_charging((c(_)|_)).
preprocess(Oracles, Charging):-
  content_search([o(1),o(2),o(3),o(4),o(5),o(6),o(7),o(8),o(9),o(10),c(1),c(2)], Items),!,
  include(is_oracle, Items, Oracles),
  include(is_charging, Items, Charging).
  
