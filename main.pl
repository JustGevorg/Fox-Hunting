%%%%%%%%%%%%%%%%% Fox Hunting %%%%%%%%%%%%%%%%%%

random_list(N, T, L) :-
    findall([X,Y], (between(1,N,_), random(0, T,X), random(0, T, Y)), L).

vert_catch([_, V], [_, V]) :- writeln("Fox on vertical").
hor_catch([H, _], [H, _]) :- writeln("Fox on horizontal").

diag_catch([H, V], [H1, V1]) :-
    V>V1, H<H1,
    diag_catch_up_right([H, V], [H1, V1]), 
    writeln("Fox on diagonal"),!;
    V>V1, H>H1,
    diag_catch_up_left([H, V], [H1, V1]), 
    writeln("Fox on diagonal"), !;
    V<V1, H<H1,
    diag_catch_down_right([H, V], [H1, V1]), 
    writeln("Fox on diagonal"), !;
     V<V1, H>H1,
    diag_catch_down_left([H, V], [H1, V1]), 
    writeln("Fox on diagonal"), !.
%diag_catch([H, V], [H1, V1]) :-
%    V>V1, H>H1,
%    diag_catch_up_left([H, V], [H1, V1]), !.
%diag_catch([H, V], [H1, V1]) :-
%    V<V1, H<H1,
%    diag_catch_down_right([H, V], [H1, V1]), !.
%diag_catch([H, V], [H1, V1]) :-
%    V<V1, H>H1,
%    diag_catch_down_left([H, V], [H1, V1]), !.

diag_catch_up_right([H, V], [H, V]) :- !.
diag_catch_up_right([H, V], [H1, V1]) :- 
    NewH is H+1,
    NewV is V-1,
    diag_catch_up_right([NewH,NewV], [H1, V1]).

diag_catch_up_left([H, V], [H, V]) :- !.
diag_catch_up_left([H, V], [H1, V1]) :- 
    NewH is H-1,
    NewV is V-1,
    diag_catch_up_right([NewH,NewV], [H1, V1]).

diag_catch_down_right([H, V], [H, V]) :- !.
diag_catch_down_right([H, V], [H1, V1]) :- 
    NewH is H+1,
    NewV is V+1,
    diag_catch_down_right([NewH,NewV], [H1, V1]).

diag_catch_down_left([H, V], [H, V]) :- !.
diag_catch_down_left([H, V], [H1, V1]) :- 
    NewH is H-1,
    NewV is V+1,
    diag_catch_down_right([NewH,NewV], [H1, V1]).

catch([H, V], [H1, V1]) :-
    vert_catch([H, V], [H1, V1]),!;
    hor_catch([H, V], [H1, V1]),!;
    diag_catch([H, V], [H1, V1]).

start_game() :-
    writeln("Enter field size and foxes count").