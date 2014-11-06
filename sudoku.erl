-module(sudoku).
%% TODO: hide internals
-compile(export_all).


%%---------------------------------------------------------------------
%% Data Type: cell
%% where:
%%    y: row of the sudoku cell
%%    x: column of the sudoku cell
%%    value: Initially a list with all possible values for this cell, i.e. [1..9]
%%           Later, when the possibilities are limited by already determined
%%           neighbours, the list gets shorter until this cell's value is
%%           determined, then it is changed to a single integer 
%%----------------------------------------------------------------------
-record(cell, {y, x, value=[1,2,3,4,5,6,7,8,9]}).

%%----------------------------------------------------------------------
%% Function: start/0
%% Purpose: spawn the master process, which in turn spawns the cells
%% Args:   None
%% Returns: A list of cell pids
%%----------------------------------------------------------------------
start() ->
    Pid = spawn(?MODULE, sudoku, [[]]),
    register(master, Pid),
    create_cells().

%%----------------------------------------------------------------------
%% Function: stop/0
%% Purpose: stops all processes
%% Args:   None
%% Returns: Nothing
%%----------------------------------------------------------------------
stop() ->
    master ! stop.

%%----------------------------------------------------------------------
%% Function: set/3
%% Purpose: stops all processes
%% Args:   Y,X .. coordinates
%%         Value .. 
%% Returns: Nothing
%%----------------------------------------------------------------------
set(Y, X, Value) when is_integer(Value) ->
    master ! {set, Y, X, Value}.

%%----------------------------------------------------------------------
%% Function: get/0
%% Purpose: fetches the current value from a cell
%% Args:   Y,X .. coordinates
%% Returns: Value, can be a list or an integer
%%----------------------------------------------------------------------
get(Y, X) ->
    master ! {get, self(), Y, X},
    receive
        {get, Y, X, V} ->
            V
    end.

%%----------------------------------------------------------------------
%% Function: printY/Y
%% Purpose: gets the values of all cells in a row
%% Args:   Y row identifier
%% Returns: List of values
%%----------------------------------------------------------------------
printY(Y) ->
    [get(Y,X)|| X <- lists:seq(1,9)].
    
%%----------------------------------------------------------------------
%% Function: printCleanY/1
%% Purpose: gets the values of all cells and replaces lists by '0'
%%          this gives a more readable output
%% Args:   Y row identifier
%% Returns: List of cleaned values
%%----------------------------------------------------------------------
printCleanY(Y) ->
    lists:map(fun(X) -> if is_list(X) -> 0; true -> X end end, printY(Y)).

%%----------------------------------------------------------------------
%% Function: printClean/0
%% Purpose: gets the cleaned values of all cells of the sudoku
%% Args:   None
%% Returns: List of lists of cell values
%%----------------------------------------------------------------------
printClean() ->
    [printCleanY(Y) || Y <- lists:seq(1,9)].

%%----------------------------------------------------------------------
%% Function: sudoku/1
%% Purpose: master process
%%          spawns the cells and acts as communication center
%% Args:   List of cells, {tuple with Y,X and Pid}
%% Returns: Nothing
%%----------------------------------------------------------------------
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

%%----------------------------------------------------------------------
%% Function: findCell/3
%% Purpose: finds the cell with coordinates Y,X in the list of cells
%% Args:   Y,X .. coordinates
%%         Cells .. list of cells with {Y,X,Pid} elements
%% Returns: Pid of cell or error if not found
%%----------------------------------------------------------------------
findCell(Y, X, [{Y,X,Pid}|_]) -> 
    Pid;
findCell(_, _, []) -> 
    error;
findCell(Y,X, [_|Cells]) -> 
    findCell(Y,X,Cells).
 
%%----------------------------------------------------------------------
%% Function: broadcast/2
%% Purpose: sends a message to all cells
%% Args:   List of cells (as above), Message
%% Returns: Nothing
%%----------------------------------------------------------------------
broadcast([{_Y,_X,Pid}|Cells], Message) ->
    Pid ! Message,
    broadcast(Cells, Message);
broadcast([], _) -> 
    done.
    
%%----------------------------------------------------------------------
%% Function: create_cells/0
%% Purpose: create all cells. Sudoku size of 9x9 is hardcoded
%% Args:   None
%% Returns: Nothing.
%%----------------------------------------------------------------------
create_cells() ->
    [create_cell(Y,X) || Y <- lists:seq(1,9),
                         X <- lists:seq(1,9)],
    done.

%%----------------------------------------------------------------------
%% Function: create_cell/2
%% Purpose: create one cell with given parameters
%% Args:   Y,X .. coordinates
%% Returns: Nothing
%%----------------------------------------------------------------------
create_cell(Y,X) ->
    Pid = spawn(?MODULE, cell, [#cell{y=Y,x=X}]),
    master ! {cell, Y, X, Pid},
    done.

%%----------------------------------------------------------------------
%% Function: cell/1
%% Purpose: cell process loop
%%          waits for messages from other cells and adapts own state,
%%          if appropriate. Broadcasts own state, if value is fixed to
%%          a single value.
%% Args:   State record
%% Returns: Nothing
%%----------------------------------------------------------------------
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

%%----------------------------------------------------------------------
%% Function: isRelevant/0
%% Purpose: used by cell/1 to determine if the message is of relevance for
%%          the own state
%% Args:   None
%% Returns: Nothing
%%----------------------------------------------------------------------
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



