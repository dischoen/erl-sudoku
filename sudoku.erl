-module(sudoku).
-compile(export_all).

-record(cell, {y, x, value=[1,2,3,4,5,6,7,8,9]}).

start() ->
    Pid = spawn(?MODULE, sudoku, [[]]),
    register(master, Pid),
    create_cells().

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
        {fixed, Y, X, V} -> broadcast(Cells, {fixed, Y, X, V}),
                            sudoku(Cells);
        stop -> broadcast(Cells, stop)
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
    receive
        {set, V} ->
            NewState = #cell{y=Y, x=X, value=V},
            master ! {fixed, Y, X, V}, 
            cell(NewState);
        {get} ->
            master ! {value, Value}, 
            cell(State);
        {fixed, Yotr, Xotr, V} when is_list(Value) ->
            IR =isRelevant(Y,X,Yotr, Xotr),
            if IR -> 
                    NewValueList = Value -- [V],
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
        {stop} -> stop
    end.

isRelevant(Y,X,Y,X) ->
    % it's me
    false;
isRelevant(Y,_X,Y,_Xotr) ->
    % same row
    true;
isRelevant(_Y,X,_Yotr,X) ->
    % same column
    true;
isRelevant(Y,X,Yotr,Xotr) when ((Y-1) div 3 =:= (Yotr-1) div 3) and 
                               ((X-1) div 3 =:= (Xotr-1) div 3) ->
    true;
isRelevant(_,_,_,_) ->
    false.



