candidate_number(23191).

solve_task(Task,Cost):-
  my_agent(Agent),
  query_world( agent_current_position, [Agent,P] ),
  %memberchk(1,[]),
  solve_task_bt(Task,[[c(0,0,P)]],R,F,G,_NewPos),!.  % prune choice point for efficiency
  %reverse(R,[_Init|Path]),
  %query_world( agent_do_moves, [Agent,Path] ).

%%%%%%%%%% Useful predicates %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% backtracking depth-first search, needs to be changed to agenda-based A*
solve_task_bt(Task,Agenda,ClosedSet,F,G,NewPos) :-
  achieved(Task,Agenda,ClosedSet,F,G,NewPos).

solve_task_bt(go(Target),Agenda,ClosedSet,F,G,NewPos) :-
  Agenda = [Current|Rest],
  Current = [c(F,G,P)|RPath],
  NewAgenda = Rest,
  setof((P1,C),P^R^search(P,P1,R,C),PS),
  %PS = [(p(1,2),1),(p(2,1),1)],
  addChildren(PS,RPath,Current,NewAgenda,Target,Result),
  NewClosedSet = [Current|ClosedSet],
  NewestAgenda = Result,
  solve_task_bt(go(Target),NewestAgenda,NewClosedSet,F1,G1,Pos).  % backtrack search

addChildren(PS,RPath,Current,NewAgenda,Target,Result) :-
	Current = [c(F,G,P)|Rest],
	PS = [(Pos,Cost)|Others],
	\+ memberchk(Pos,RPath),  % check we have not been here already
	G1 is G+Cost,
	map_distance(Pos,Target,Dist),
	F1 is G1 + Dist, %f(n) = g(n) + h(n)
	Child = [c(F1,G1,Pos),Pos|RPath],
	\+ memberchk(Child,NewAgenda),
	( 	NewAgenda=[] -> BrandNewAgenda = [Child|NewAgenda];
		otherwise    -> append(NewAgenda,[Child],BrandNewAgenda)
	),
	addChildren(Others,RPath,Current,BrandNewAgenda,Target,Result).

addChildren([],RPath,Current,Result,Target,Result).

achieved(go(Exit),Agenda,ClosedSet,F,G,NewPos) :-
  Agenda = [Current|Rest],
  Current = [c(F,G,NewPos)|RPath],
  ( Exit=none -> true
  ; otherwise -> Current = [c(FF,FG,Exit)|_]
  ).
achieved(find(O),Agenda,RPath,F,G,NewPos) :-
  Agenda = [Current|Rest],
  Current = [c(F,G,NewPos)|RPath],
  ( O=none    -> true
  ; otherwise -> RPath = [Last|_],map_adjacent(Last,_,O)
  ).

search(F,N,N,1) :-
  map_adjacent(F,N,empty).


























