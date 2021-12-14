%%%%%%%%%%%%%%%%% Fox Hunting %%%%%%%%%%%%%%%%%%

% Making foxes coordinates's list
random_list(N, T, L) :-
    findall([X,Y], (between(1,N,_), random(1, T, X), random(1, T, Y)), L).

% Checking foxes at horizontal and vertical
vert_catch([V, _], [V, _], _) :- writeln("Fox on horizontal").
hor_catch([_, H], [_, H], _) :- writeln("Fox on vertical").

% Checking foxes at diagonal, 
% search direction depends of hunter and fox interposition
diag_catch([V, H], [V1, H1], Size) :-
    V>V1, H<H1 ->
    diag_catch_up_right([V, H], [V1, H1], Size), 
    writeln("Fox on diagonal"),!;
    V>V1, H>H1 ->
    diag_catch_up_left([V, H], [V1, H1], Size), 
    writeln("Fox on diagonal"), !;
    V<V1, H<H1 ->
    diag_catch_down_right([V, H], [V1, H1], Size), 
    writeln("Fox on diagonal"), !;
    V<V1, H>H1 ->
    diag_catch_down_left([V, H], [V1, H1], Size), 
    writeln("Fox on diagonal"), !.

% Serching in different direcions
diag_catch_up_right([V, H], [V, H], _) :- !.
diag_catch_up_right([V, H], [V1, H1], Size) :-
    NewV is V-1, 
    NewH is H+1,
    NewH < Size,
    NewV > 0,
    diag_catch_up_right([NewV,NewH], [V1, H1], Size).

diag_catch_up_left([V, H], [V, H], _) :- !.
diag_catch_up_left([V, H], [V1, H1], Size) :- 
    NewV is V-1,
    NewH is H-1,
    NewH > 0,
    NewV > 0,
    diag_catch_up_right([NewV,NewH], [V1, H1], Size).

diag_catch_down_right([V, H], [V, H], _) :- !.
diag_catch_down_right([V, H], [V1, H1], Size) :- 
    NewV is V+1,
    NewH is H+1,
    NewH < Size,
    NewV < Size,
    diag_catch_down_right([NewV,NewH], [V1, H1], Size).

diag_catch_down_left([V, H], [V, H], _) :- !.
diag_catch_down_left([V, H], [V1, H1], Size) :- 
    NewV is V+1,
    NewH is H-1,
    NewH > 0,
    NewV < Size,
    diag_catch_down_right([NewV,NewH], [V1, H1], Size).

% Start searching in all directions
catching([V, H], [V1, H1], Size) :-
    vert_catch([V, H], [V1, H1], Size),!;
    hor_catch([V, H], [V1, H1], Size),!;
    diag_catch([V, H], [V1, H1], Size).


get_head([H|_], H).
get_tail([_|T], T).


fox_here([V, H], [V, H], 1) :- !, writeln("Fox catched"). % returns 1 if fox and hunter in same cell
fox_here(_, _, 0) :- !, writeln("Fox not catched"). % returns 0 in any other case


% Counts foxes at all directions from hunter
fox_iteration(_, [], _, 0) :- !.
fox_iteration([V, H], [FirstFox|OtherFoxes], Size, Answer) :- 
    catching([V,H], FirstFox, Size) -> fox_iteration([V, H], OtherFoxes, Size, NextAnswer), Answer is NextAnswer+1;
    fox_iteration([V, H], OtherFoxes, Size, Answer).

% Remove ONLY first appear element
remover( _, [], []) :- !.
remover( R, [R|T], T).
remover( R, [H|T], [H|T2]) :- H \= R, remover( R, T, T2).

% Remove all element appears
%remover( _, [], []).
%remover( R, [R|T], T2) :- remover( R, T, T2).
%remover( R, [H|T], [H|T2]) :- H \= R, remover( R, T, T2)


% Here must be predicate to define catched fox
%fox_right_here(_, [], _, 0) :- !.
%fox_right_here([V, H], [FirstFox|OtherFoxes], Size, Answer) :- 
%    fox_here([V,H], FirstFox, Param) -> ;
%    fox_right_here([V, H], OtherFoxes, Size, Answer).

%tttt(Hunter, [H1|T1]) :- fox_here(Hunter, H1)

% Start of the game
start :- 
    write("Enter field size: "), nl, 
    read(FieldSize),
    write("Enter count of foxes: "), nl, 
    read(Foxes),
    random_list(Foxes, FieldSize, ListOfFoxes),
    repeat,
        writeln(ListOfFoxes),
        writeln("Enter hunter position [V, H]"),
        read(HunterPosition), nl,
        remover(HunterPosition, ListOfFoxes, Res),
        Res == ListOfFoxes -> writeln("There is no fox here"),
        fox_iteration(HunterPosition, ListOfFoxes, FieldSize, Answer),
        writeln("At vertical, horizontal and diagonal we have so many foxes:"),
        writeln(Answer);
        Res \= ListOfFoxes ->
        ListOfFoxes = Res,
        writeln("Fox Catched"),
        (Res == [] ->
        writeln("Congratz, all foxes catched"),
        fail;
        writeln("Lets'go again"), !).