candidate_number(23191).

solve_task(Task,Cost):-
  my_agent(Agent),
  query_world( agent_current_position, [Agent,P] ),
  %memberchk(1,[]),
  solve_task_bt(Task,[[c(0,0,P),[]]],R,F,G,_NewPos,RR,BackPath),!,
  write(BackPath), % prune choice point for efficiency
  reverse(BackPath,[_Init|Path]),
  write(Path),
  %write(Path),
  query_world( agent_do_moves, [Agent,Path] ).

%%%%%%%%%% Useful predicates %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% backtracking depth-first search, needs to be changed to agenda-based A*
solve_task_bt(Task,Agenda,ClosedSet,F,G,NewPos,RR,BackPath) :-
  achieved(Task,Agenda,ClosedSet,F,G,NewPos,RR,BackPath).

solve_task_bt(go(Target),Agenda,ClosedSet,F,G,NewPos,RR,BackPath) :-
  Agenda = [Current|Rest],
  Current = [c(F,G,P)|RPath],
  NewAgenda = Rest,
  Bag = search(P,P1,R,C),
  (\+ setof((P1,C),P^R^Bag,PS) -> solve_task_bt(go(Target),Rest,[Current|ClosedSet],F,G,NewPos,RR,BackPath);
  	otherwise -> 
 	setof((P1,C),P^R^Bag,PS),
  	addChildren(PS,RPath,Current,NewAgenda,Target,Result),
  	NewClosedSet = [Current|ClosedSet],
  	NewestAgenda = Result,
  	solve_task_bt(go(Target),NewestAgenda,NewClosedSet,F1,G1,Pos,P|RPath,BackPath)
  	).  % backtrack search

addChildren([],RPath,Current,Result,Target,Result).

addChildren(PS,RPath,Current,NewAgenda,Target,Result) :-
	Current = [c(F,G,P)|Rest],
	(NewAgenda = [] -> true;
		otherwise -> NewAgenda = [BestAgenda|_],
	BestAgenda = [c(FP,GP,PP)|_]),
	PS = [(Pos,Cost)|Others],
	( memberchk(Pos,RPath) -> addChildren(Others,RPath,Current,NewAgenda,Target,Result)
		; otherwise -> 
	G1 is G+Cost,
	map_distance(Pos,Target,Dist),
	F1 is G1 + Dist,
	( RPath = [[]] -> Child = [c(F1,G1,Pos),P]
	; otherwise -> Child = [c(F1,G1,Pos),P|RPath]
	),
	(  memberchk(Child,NewAgenda) -> addChildren(Others,RPath,Current,NewAgenda,Target,Result)
		
	;  otherwise -> 
		( 	NewAgenda=[] -> BrandNewAgenda = [Child|NewAgenda]
		;	otherwise    -> 
			(F1 =< FP -> BrandNewAgenda = [Child|NewAgenda];
			otherwise  -> append(NewAgenda,[Child],BrandNewAgenda)
			)
		),
		addChildren(Others,RPath,Current,BrandNewAgenda,Target,Result)
	)).

achieved(go(Exit),Agenda,ClosedSet,F,G,NewPos,Check,NewCheck) :-
  Agenda = [Current|Rest],
  Current = [c(F,G,NewPos)|RPath],
  write(rpath),write(RPath),nl,
  NewCheck = [NewPos|RPath],
  write(check),write(NewCheck),nl,
  ( Exit=none -> true
  ; otherwise -> NewCheck = [Exit|_]
  ).
achieved(find(O),Agenda,RPath,F,G,NewPos) :-
  Agenda = [Current|Rest],
  Current = [c(F,G,NewPos)|RPath],
  ( O=none    -> true
  ; otherwise -> RPath = [Last|_],map_adjacent(Last,_,O)
  ).

search(F,N,N,1) :-
  map_adjacent(F,N,empty).


























