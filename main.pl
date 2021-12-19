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
    diag_catch_up_left([NewV,NewH], [V1, H1], Size).

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
    diag_catch_down_left([NewV,NewH], [V1, H1], Size).

% Start searching in all directions
catching([V, H], [V1, H1], Size) :-
    vert_catch([V, H], [V1, H1], Size),!;
    hor_catch([V, H], [V1, H1], Size),!;
    diag_catch([V, H], [V1, H1], Size).


position_valid([V,H], Size, Res) :- V > 0, H > 0, V < Size, H < Size, Res = true,!; Res = false.

% Counts foxes at all directions from hunter
fox_iteration(_, [], _, 0) :- !.
fox_iteration([V, H], [FirstFox|OtherFoxes], Size, Answer) :- 
    V > 0, H > 0, V < Size, H < Size, catching([V,H], FirstFox, Size) -> fox_iteration([V, H], OtherFoxes, Size, NextAnswer), Answer is NextAnswer+1;
    fox_iteration([V, H], OtherFoxes, Size, Answer).

% Remove ONLY first appear element
%remover( _, [], []) :- !.
%remover( R, [R|T], T) :- !.
%remover( R, [H|T], [H|T2]) :- H \= R, remover( R, T, T2).

% Remove all element appears
remover( _, [], []) :- !.
remover( R, [R|T], T2) :- remover( R, T, T2), !.
remover( R, [H|T], [H|T2]) :- H \= R, remover( R, T, T2).


% Start of the game
start :- 
    write("Enter field size: "), nl, 
    read(FieldSize),
    write("Enter count of foxes: "), nl, 
    read(Foxes),
    random_list(Foxes, FieldSize, ListOfFoxes),
    writeln(ListOfFoxes),
    writeln("Enter start hunter position"), read(FirstHunterPosition),
    game_loop(FieldSize, ListOfFoxes, FirstHunterPosition, []).


game_loop(_, [], _, _) :- writeln("Game over"), !.

game_loop(FieldSize, ListOfFoxes, HunterPosition, AllPositions) :-

    position_valid(HunterPosition, FieldSize, Res), Res = false, writeln("Attention, invalid position"),
    writeln("Enter hunter position [Row, Col]"),
    read(NewHunterPosition),
    game_loop(FieldSize, ListOfFoxes, NewHunterPosition, AllPositions),!;

    remover(HunterPosition, ListOfFoxes, Result),
    Result = [] -> game_loop(0, [], 0, 0), !;

    remover(HunterPosition, ListOfFoxes, Result),
    Result \= ListOfFoxes ->
    writeln("Fox Catched"),
    writeln(Result),
    writeln("Made moves"),
    append(AllPositions, [HunterPosition], NewAllPositions),
    writeln(NewAllPositions), nl,
    writeln("Enter hunter position [Row, Col]"),
    read(NewHunterPosition),
    game_loop(FieldSize, Result, NewHunterPosition, NewAllPositions), !;

    writeln("There is no fox here"),
    fox_iteration(HunterPosition, ListOfFoxes, FieldSize, Answer),
    writeln("At vertical, horizontal and diagonal we have so many foxes:"),
    writeln(Answer),
    writeln("Made moves"),
    append(AllPositions, [HunterPosition], NewAllPositions),
    write(NewAllPositions), nl,
    writeln("Enter hunter position [Row, Col]"),
    read(NewHunterPosition),
    game_loop(FieldSize, ListOfFoxes, NewHunterPosition, NewAllPositions).
