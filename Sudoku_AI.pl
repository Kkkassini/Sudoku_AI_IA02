%% Program flow predicate
main :- nl,write('Welcome to use Sudoku solver!'),nl,
    write('Please do the operation according to the following options.'),nl,
    write('1. User-defined Sudoku .'),nl,
    write('2. The program generates Sudoku randomly for you.'),nl,
    write('3. Quit.'),nl,nl,
    read(Option),
    (
        Option == 1   -> option_new
        ;Option == 2  -> write('The program is randomly generated valid sudoku for you:'),nl,option_rnd
        ;Option == 3  -> true,!
        ;                write('Your input is incorrect, please reenter it.'),nl
    ).

main_loop :- nl,write('Welcome to use Sudoku solver again!'),nl,
    write('Please do the operation according to the following options.'),nl,
    write('1. User-defined Sudoku .'),nl,
    write('2. User-defined Sudoku through the last one.'),nl,
    write('3. The program generates Sudoku randomly for you.'),nl,
    write('4. Quit.'),nl,nl,
    read(Option),
    (
        Option == 1  -> option_new
        ;Option == 2 -> option_old
        ;Option == 3 -> write('The program is randomly generated valid sudoku for you:'),nl,option_rnd
        ;Option == 4 -> true,!
        ;               write('Your input is incorrect, please reenter it.'),nl
    ).
                 
option_new :- 
    write('Please input a sudoku puzzle(enter a string of length 81, each character is 0~9).'),nl,nl,
    read(PuzzleString),
    (
        check(PuzzleString) -> parse_sudoku(PuzzleString, Puzzle),
                               retractall(puzzle(_)),asserta(puzzle(Puzzle)),
                               write('The sudoku is:'),nl,
                               puzzle(Puzzle_),
                               get_printlist(Puzzle_,PrintList),
                               print_puzzle_(PrintList),
                               sudoku(Puzzle,PrintListRes),
                               print_puzzle(PrintListRes),
                               main_loop
        ;                      write('The string of puzzle you entered is incorrect or unsolved. Please correct it and retype it.'),nl,
                               option_new
    ).   

option_old :- 
    write('Please define your new puzzle by modifying the previous puzzle.'),nl,         
    write('(Each three characters are a pair, the format is XYN, X is the row number (starting from 1), Y is the column number (from 1), and N is the character to be modified (0 is empty)).'),nl,nl,
    read(String),
    (
        modify_sudoku(String, Puzzle) -> write('The sudoku is:'),nl,
                                         puzzle_m(Puzzle_),
                                         retractall(puzzle(_)),asserta(puzzle(Puzzle_)),
                                         get_printlist(Puzzle_,PrintList),
                                         print_puzzle_(PrintList),
                                         sudoku(Puzzle,PrintListRes),
                                         print_puzzle(PrintListRes),
                                         main_loop
                    ;                    write('The string of modifying the previous puzzle you entered is incorrect or unsolved. Please correct it and retype it.'),nl,
                                         option_old
    ).  
    

option_rnd :- rnd_puzzle(Puzzle),retractall(puzzle(_)),asserta(puzzle(Puzzle)),
              (
                    sudoku(Puzzle,_) -> write('The sudoku is:'),nl,
                                        puzzle(Puzzle_),
                                        get_printlist(Puzzle_,PrintList),
                                        print_puzzle_(PrintList),
                                        sudoku(Puzzle,PrintListRes),
                                        write('The solution of sudoku is:'),nl,
                                        print_puzzle(PrintListRes),
                                        main_loop
                    ;                   option_rnd
              ).

    
% query: sudoku([_, 6, _, 5, 9, 3, _, _, _, 9, _, 1, _, _, _, 5, _, _, _, 3, _, 4, _, _, _, 9, _, 1, _, 8, _, 2, _, _, _, 4, 4, _, _, 3, _, 9, _, _, 1, 2, _, _, _, 1, _, 6, _, 9, _, 8, _, _, _, 6, _, 2, _,_, _, 4, _, _, _, 8, _, 7, _, _, _, 7, 8, 5, _, 1, _],Solution).
%----Ajout chaque element(peut etre une liste) dans la liste temporaire,puis valider si ils sont different deux par deux. 

valide([]).
valide([T|Q]):-
    all_different(T),
    %verifier_diff(T),
    valide(Q).

domain_one(E,X,Y):-
    E #=< Y,
    E #>= X.

domain([],_,_).
domain([H|T],X,Y):-
    domain_one(H,X,Y),
    domain(T,X,Y).
    
one_different(_,[]).
one_different(T,[T1|Q]):-
    T #\= T1,
    one_different(T,Q).

all_different([]).
all_different([T|Q]):-
    one_different(T,Q),
    all_different(Q).
    

    
printline_([])   :- !.
printline_([A|B]):- 
    (
        number(A) -> write(A), write('|'), printline_(B)
        ;            write(' '), write('|'), printline_(B)
    ).    
    
printline([])   :- !.
printline([A|B]):- 
    write(A), write('|'), printline(B).



%afficher Groupe de lignes
printall_([A|[]]) :- printline_(A), !.
printall_([A|B])  :- printline_(A),  nl, write('-----------------'), nl, printall_(B), nl.

printall([A|[]]) :- printline(A), !.
printall([A|B])  :- printline(A),  nl, write('-----------------'), nl, printall(B), nl.

%verifier_diff([]).
%verifier_diff([T|Q]) :- maplist(dif(T), Q), unique(Q).

get_printlist(Puzzle,PrintList):-
    Puzzle = [S11,S12,S13,S14,S15,S16,S17,S18,S19,
          S21,S22,S23,S24,S25,S26,S27,S28,S29,
          S31,S32,S33,S34,S35,S36,S37,S38,S39,
          S41,S42,S43,S44,S45,S46,S47,S48,S49,
          S51,S52,S53,S54,S55,S56,S57,S58,S59,
          S61,S62,S63,S64,S65,S66,S67,S68,S69,
          S71,S72,S73,S74,S75,S76,S77,S78,S79,
          S81,S82,S83,S84,S85,S86,S87,S88,S89,
          S91,S92,S93,S94,S95,S96,S97,S98,S99],
    Ligne1 = [S11,S12,S13,S14,S15,S16,S17,S18,S19],
    Ligne2 = [S21,S22,S23,S24,S25,S26,S27,S28,S29],
    Ligne3 = [S31,S32,S33,S34,S35,S36,S37,S38,S39],
    Ligne4 = [S41,S42,S43,S44,S45,S46,S47,S48,S49],
    Ligne5 = [S51,S52,S53,S54,S55,S56,S57,S58,S59],
    Ligne6 = [S61,S62,S63,S64,S65,S66,S67,S68,S69],
    Ligne7 = [S71,S72,S73,S74,S75,S76,S77,S78,S79],
    Ligne8 = [S81,S82,S83,S84,S85,S86,S87,S88,S89],
    Ligne9 = [S91,S92,S93,S94,S95,S96,S97,S98,S99],
    PrintList = [Ligne1, Ligne2, Ligne3, Ligne4, Ligne5, Ligne6, Ligne7, Ligne8, Ligne9].

%--------Fonction principale------------%
sudoku(Puzzle,PrintList):-
    Puzzle = [S11,S12,S13,S14,S15,S16,S17,S18,S19,
          S21,S22,S23,S24,S25,S26,S27,S28,S29,
          S31,S32,S33,S34,S35,S36,S37,S38,S39,
          S41,S42,S43,S44,S45,S46,S47,S48,S49,
          S51,S52,S53,S54,S55,S56,S57,S58,S59,
          S61,S62,S63,S64,S65,S66,S67,S68,S69,
          S71,S72,S73,S74,S75,S76,S77,S78,S79,
          S81,S82,S83,S84,S85,S86,S87,S88,S89,
          S91,S92,S93,S94,S95,S96,S97,S98,S99],
    domain(Puzzle,1,9),
    
    Ligne1 = [S11,S12,S13,S14,S15,S16,S17,S18,S19],
    Ligne2 = [S21,S22,S23,S24,S25,S26,S27,S28,S29],
    Ligne3 = [S31,S32,S33,S34,S35,S36,S37,S38,S39],
    Ligne4 = [S41,S42,S43,S44,S45,S46,S47,S48,S49],
    Ligne5 = [S51,S52,S53,S54,S55,S56,S57,S58,S59],
    Ligne6 = [S61,S62,S63,S64,S65,S66,S67,S68,S69],
    Ligne7 = [S71,S72,S73,S74,S75,S76,S77,S78,S79],
    Ligne8 = [S81,S82,S83,S84,S85,S86,S87,S88,S89],
    Ligne9 = [S91,S92,S93,S94,S95,S96,S97,S98,S99],
    
    Col1 = [S11,S21,S31,S41,S51,S61,S71,S81,S91],
    Col2 = [S12,S22,S32,S42,S52,S62,S72,S82,S92],
    Col3 = [S13,S23,S33,S43,S53,S63,S73,S83,S93],
    Col4 = [S14,S24,S34,S44,S54,S64,S74,S84,S94],
    Col5 = [S15,S25,S35,S45,S55,S65,S75,S85,S95],
    Col6 = [S16,S26,S36,S46,S56,S66,S76,S86,S96],
    Col7 = [S17,S27,S37,S47,S57,S67,S77,S87,S97],
    Col8 = [S18,S28,S38,S48,S58,S68,S78,S88,S98],
    Col9 = [S19,S29,S39,S49,S59,S69,S79,S89,S99],
    
    Bloc1 = [S11,S12,S13,S21,S22,S23,S31,S32,S33],
    Bloc2 = [S14,S15,S16,S24,S25,S26,S34,S35,S36],
    Bloc3 = [S17,S18,S19,S27,S28,S29,S37,S38,S39],
    Bloc4 = [S41,S42,S43,S51,S52,S53,S61,S62,S63],
    Bloc5 = [S44,S45,S46,S54,S55,S56,S64,S65,S66],
    Bloc6 = [S47,S48,S49,S57,S58,S59,S67,S68,S69],
    Bloc7 = [S71,S72,S73,S81,S82,S83,S91,S92,S93],
    Bloc8 = [S74,S75,S76,S84,S85,S86,S94,S95,S96],
    Bloc9 = [S77,S78,S79,S87,S88,S89,S97,S98,S99],
    
    valide([Ligne1,Ligne2,Ligne3,Ligne4,Ligne5,Ligne6,Ligne7,Ligne8,Ligne9,
          Col1,Col2,Col3,Col4,Col5,Col6,Col7,Col8,Col9,
          Bloc1,Bloc2,Bloc3,Bloc4,Bloc5,Bloc6,Bloc7,Bloc8,Bloc9]),
    PrintList = [Ligne1, Ligne2, Ligne3, Ligne4, Ligne5, Ligne6, Ligne7, Ligne8, Ligne9],
    fd_labeling(Puzzle).

    
%% helper predicate

rnd_puzzle(Puzzle) :- 
    length(PuzzleList,81),
    maplist(random(0,9),PuzzleList),
    list_puzzle(PuzzleList,Puzzle).

list_puzzle([],[]) :- !.
list_puzzle([H|PuzzleList],[HNumber|Puzzle]):-
    (
        H =< 5 -> list_puzzle(PuzzleList,Puzzle)
        ;      random(1,9,HNumber), list_puzzle(PuzzleList,Puzzle)
    ).

check(String):-
    parse_sudoku(String, Puzzle),
    sudoku(Puzzle,_).

modify_sudoku(String, Puzzle):-
    puzzle(Puzzle_old),
    atom_chars(String, StringList),
    check_string_old(StringList,Puzzle_old,Puzzle),
    retractall(puzzle_m(_)),asserta(puzzle_m(Puzzle)),
    sudoku(Puzzle,_).
    
check_string_old([],Puzzle,Puzzle).
check_string_old([XC,YC,HC|StringList],Puzzle,PuzzleLast):-
    char_code(XC,X),
    X >= 49, X =< 57,
    number_atom(XX,XC),
    char_code(YC,Y),
    Y >= 49, Y =< 57,
    number_atom(YY,YC),
    char_code(HC,H),
    H >= 48, H =< 57,
    number_atom(HH,HC),
    (
        H == 0 -> check_string_old(StringList,Puzzle,PuzzleLast)
        ;         Index is (XX-1)*9+YY-1, replace(Puzzle,Index,HH,PuzzleAcc),check_string_old(StringList,PuzzleAcc,PuzzleLast)
    ).
 
replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]):- I > -1, NI is I-1, replace(T, NI, X, R), !.
replace(L, _, _, L).
 
parse_sudoku(String, Puzzle):-
    atom_chars(String, StringList),
    check_string(StringList, Puzzle).

check_string([],[]).
check_string([H|StringList],[HNumber|Puzzle]):-
    char_code(H,X),
    X >= 48, X =< 57,
    number_atom(THNumber,H),
    (
        THNumber == 0 -> check_string(StringList,Puzzle)
        ;                HNumber = THNumber, check_string(StringList,Puzzle)
    ).
    
print_puzzle(PrintList):-
    nl,
    printall(PrintList).

print_puzzle_(PrintList):-
    nl,
    printall_(PrintList).
    