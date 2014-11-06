-module(sudoku_test).
-export([do_test/0, do_test2/0]).
-import(sudoku, [start/0, set/3]).


%%----------------------------------------------------------------------
%% Function: do_test/0
%% Purpose: Test the sudoku implementation. This is a under-determined sudoku.
%%          Verify afterwards with sudoku:printClean()
%% Returns: done
%%----------------------------------------------------------------------
do_test() ->
    start(),
    set(1,5,1),
    set(1,7,2),
    set(1,8,4),
    set(2,6,4),
    set(2,9,1),
    set(3,1,4),
    set(3,5,6),
    set(3,6,3),
    set(3,7,8),
    set(4,4,6),
    set(4,7,5),
    set(4,9,8),
    set(5,3,5),
    set(5,7,7),
    set(6,1,9),
    set(6,3,2),
    set(6,6,1),
    set(7,3,6),
    set(7,4,3),
    set(7,5,8),
    set(7,9,7),
    set(8,1,3),
    set(8,4,1),
    set(9,2,8),
    set(9,3,4),
    set(9,5,2),
    done.
    
%%----------------------------------------------------------------------
%% Function: do_test2/0
%% Purpose: Test the sudoku implementation. This is a well-determined sudoku.
%%          Verify afterwards with sudoku:printClean()
%% Returns: done
%%----------------------------------------------------------------------
do_test2() ->
    start(),
    set(1,3,8),
    set(1,5,9),
    set(1,6,2),
    set(2,2,6),
    set(2,4,3),
    set(2,5,7),
    set(3,3,2),
    set(3,6,8),
    set(3,8,7),
    set(3,9,9),
    set(4,2,2),
    set(4,4,5),
    set(4,7,8),
    set(4,9,7),
    set(5,1,8),
    set(5,3,5),
    set(5,7,1),
    set(5,9,3),
    set(6,1,4),
    set(6,3,1),
    set(6,6,7),
    set(6,8,9),
    set(7,1,2),
    set(7,2,9),
    set(7,4,8),
    set(7,7,4),
    set(8,5,2),
    set(8,6,1),
    set(8,8,8),
    set(9,4,4),
    set(9,5,3),
    set(9,7,7),
    done.
