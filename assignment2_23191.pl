candidate_number(23191).

solve_task(Task,Cost):-
  my_agent(Agent),
  query_world( agent_current_position, [Agent,P] ),
  solve_task_bt(Task,[[c(0,0,P),P]],0,R,Cost,_NewPos),!,  % prune choice point for efficiency
  reverse(R,[_Init|Path]),
  query_world( agent_do_moves, [Agent,Path] ).

%%%%%%%%%% Useful predicates %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% backtracking depth-first search, needs to be changed to agenda-based A*
solve_task_bt(Task,Agenda,D,RPath,[cost(Cost),depth(D)],NewPos) :-
  achieved(Task,Agenda,RPath,Cost,NewPos).
solve_task_bt(Task,Agenda,D,RR,Cost,NewPos) :-
  Agenda = [Current|Rest],
  Current = [c(F,G,P)|RPath],
  setof(P1,P^C^R^search(P,P1,R,C),PS),
  \+ memberchk(R,RPath),  % check we have not been here already
  D1 is D+1,
  G1 is G+C,
  map_distance(P,Task,Dist),
  F1 is G1 + Dist, %f(n) = g(n) + h(n)
  solve_task_bt(Task,[[c(F1,G1,P1),R|RPath]],D1,RR,Cost,NewPos).  % backtrack search

achieved(go(Exit),Agenda,RPath,Cost,NewPos) :-
  Agenda = [Current|Rest],
  Current = [c(Cost,NewPos)|RPath],
  ( Exit=none -> true
  ; otherwise -> RPath = [Exit|_]
  ).
achieved(find(O),Agenda,RPath,Cost,NewPos) :-
  Agenda = [Current|Rest],
  Current = [c(Cost,NewPos)|RPath],
  ( O=none    -> true
  ; otherwise -> RPath = [Last|_],map_adjacent(Last,_,O)
  ).

search(F,N,N,1) :-
  map_adjacent(F,N,empty).
