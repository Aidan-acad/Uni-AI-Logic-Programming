% candidate_number(12345).

% Find hidden identity by repeatedly calling agent_ask_oracle(oscar,o(1),link,L)
% find_identity(-A)
find_identity(A):-
  (
    part_module(2)   -> find_identity_2(A)
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
  write(CB),nl.

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
  write(oracles),nl,
  find_stations(Stations),
  write(stations),nl,
  my_agent(Agent),
  write(agent),nl,
  query_world(agent_current_position, [Agent, S]),
  write(position),nl,
  bagof(X, actor(X), Actors),
  write(actors),nl,
  goToNearOracle(S, Oracles, Stations, L),
  find_identity_o(S, A, [], Actors, L, Oracles, Stations).

bf_search([],Current,Depth,RPath,Cost,Acc,Acc).
bf_search(Targets,Current,D,RR,Cost,Acc,Results) :-
  found(Targets,Current,D,RR,Cost,Acc,Results).
bf_search(Targets,Current,D,RR,Cost,Acc,Results) :-
  Current = [c(F,P)|RPath],
  search(P,P1,R,C),
  (
    memberchk(R,RPath) -> fail
  ; otherwise -> D1 is D+1,
                 F1 is F+C,
                 bf_search(Targets,[c(F1,P1),R|RPath],D1,RR,Cost,Acc,Results)
  ).
  % \+ memberchk(R,RPath),  % check we have not been here already
  % D1 is D+1,
  % F1 is F+C,
  % bf_search(Targets,[c(F1,P1),R|RPath],D1,RR,Cost,Acc,Results).  % backtrack search

found(Targets,Current,Depth,RR,Cost,Acc,Results) :-
  Current = [c(_,NewPos)|RPath],
  RPath = [Last|_],
  write('looking for: '),write(Targets),nl,
  select(Target, Targets, NewTargets),
  map_adjacent(Last,_,Target),
  write('found: '),write(Target),nl,
  % write(remaining_targets_are_),write(NewTargets),nl,
  bf_search(NewTargets,Current,Depth,RR,Cost,[(Target, NewPos)|Acc],Results).

search(F,N,N,1) :-
  map_adjacent(F,N,empty).


content_search(Targets, Results):-
  my_agent(A),
  query_world(agent_current_position, [A, P]),
  bf_search(Targets,[c(0,P),P],0,_,_,[],Results).

preprocess(Items):-
  content_search([o(1),o(2),o(3),o(4),o(5),o(6),o(7),o(8),o(9),o(10),c(1),c(2)], Items),!.
  