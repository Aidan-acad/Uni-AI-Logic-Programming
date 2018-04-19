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

find_identity_o(A):-
  A='Not yet implemented'.
