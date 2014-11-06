-module(sudoku).
-compile(export_all).

-record(cell, {y, x, value=[1,2,3,4,5,6,7,8,9]}).

start() ->
    io:format("starting..~n"),
    Pid = spawn(?MODULE, sudoku, [[]]),
    register(master, Pid),
    create_cells(),
    io:format("~p registered as master.~n", [Pid]).

stop() ->
    master ! stop.

set(Y, X, Value) ->
    master ! {set, Y, X, Value}.

get(Y, X) ->
    master ! {get, self(), Y, X},
    receive
        {get, Y, X, V} ->
            V
    end.

printY(Y) ->
    [get(Y,X)|| X <- lists:seq(1,9)].
printX(X) ->
    [get(Y,X)|| Y <- lists:seq(1,9)].
    
printCleanY(Y) ->
    lists:map(fun(X) -> if is_list(X) -> 0; true -> X end end, printY(Y)).

printClean() ->
    [printCleanY(Y) || Y <- lists:seq(1,9)].

sudoku(Cells) ->
    receive
        {cell, Y, X, Pid} -> sudoku([{Y,X,Pid}|Cells]);
        {set, Y, X, Value} -> findCell(Y,X, Cells) ! {set, Value},
                              sudoku(Cells);
        {get, Pid, Y, X} -> findCell(Y,X, Cells) ! {get},
                            receive
                                {value, V} ->
                                    Pid ! {get, Y, X, V}
                            end,
                            sudoku(Cells);
        {fixed, Y, X, V} -> io:format("MASTERfixed:~p~p:~p~n", [Y,X,V]),
                            broadcast(Cells, {fixed, Y, X, V}),
                            sudoku(Cells);
        stop -> io:format("bcst!~n"),
                broadcast(Cells, stop)
    end.

findCell(Y, X, [{Y,X,Pid}|_]) -> 
    Pid;
findCell(_, _, []) -> 
    error;
findCell(Y,X, [_|Cells]) -> 
    findCell(Y,X,Cells).
 
broadcast([{_Y,_X,Pid}|Cells], Message) ->
    Pid ! Message,
    broadcast(Cells, Message);
broadcast([], _) -> 
    done.
    
create_cells() ->
    [create_cell(Y,X) || Y <- lists:seq(1,9),
              X <- lists:seq(1,9)].

create_cell(Y,X) ->
    Pid = spawn(?MODULE, cell, [#cell{y=Y,x=X}]),
    master ! {cell, Y, X, Pid}.

cell(#cell{y=Y, x=X, value=Value} = State) ->
    %%io:format("cell:~p~n", [State]),
    receive
        {set, V} ->
            io:format("cell: ~p, fixed to:~p~n", [State,V]),
            NewState = #cell{y=Y, x=X, value=V},
            %%io:format("newVal: ~p~n", [NewState]),
            master ! {fixed, Y, X, V}, 
            cell(NewState);
        {get} ->
            master ! {value, Value}, 
            cell(State);
        {fixed, Yotr, Xotr, V} when is_list(Value) ->
            IR =isRelevant(Y,X,Yotr, Xotr),
            if IR -> 
                    NewValueList = Value -- [V],
                    io:format("~p~p otr:~p~p:~p->~p~n",[Y,X,Yotr,Xotr,V,NewValueList]),
                    if length(NewValueList) =:= 1  ->
                            [NewValue] = NewValueList,
                            master ! {fixed, Y, X, NewValue};
                       true ->
                            NewValue = NewValueList
                    end,
                    cell(#cell{y=Y, x=X, value=NewValue});
               true -> 
                    cell(State)
            end;
        {stop} -> io:format("bye: ~p~n", State)
    end.

isRelevant(Y,X,Y,X) ->
    % it's me
    false;
isRelevant(Y,_X,Y,_Xotr) ->
    % same column
    true;
isRelevant(_Y,X,_Yotr,X) ->
    % same row
    true;
isRelevant(Y,X,Yotr,Xotr) when ((Y-1) div 3 =:= (Yotr-1) div 3) and 
                               ((X-1) div 3 =:= (Xotr-1) div 3) ->
    true;
isRelevant(_,_,_,_) ->
    false.



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
    set(9,5,2).
    
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
    set(9,7,7).


    
    
    
    
    
