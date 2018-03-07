cell_to_coord('A1', 0, 0).
cell_to_coord('A2', 1, 0).
cell_to_coord('A3', 2, 0).
cell_to_coord('B1', 0, 1).
cell_to_coord('B2', 1, 1).
cell_to_coord('B3', 2, 1).
cell_to_coord('C1', 0, 2).
cell_to_coord('C2', 1, 2).
cell_to_coord('C3', 2, 2).
cell_to_coord('a1', 0, 0).
cell_to_coord('a2', 1, 0).
cell_to_coord('a3', 2, 0).
cell_to_coord('b1', 0, 1).
cell_to_coord('b2', 1, 1).
cell_to_coord('b3', 2, 1).
cell_to_coord('c1', 0, 2).
cell_to_coord('c2', 1, 2).
cell_to_coord('c3', 2, 2).
cell_to_coord(_, -1, -1).

coord_to_cell(0, 0, 'A1').
coord_to_cell(1, 0, 'A2').
coord_to_cell(2, 0, 'A3').
coord_to_cell(0, 1, 'B1').
coord_to_cell(1, 1, 'B2').
coord_to_cell(2, 1, 'B3').
coord_to_cell(0, 2, 'C1').
coord_to_cell(1, 2, 'C2').
coord_to_cell(2, 2, 'C3').
coord_to_cell(_, _, 'XX').

snapshot_board(_board) :-
  writeln("\n     A  B  C"),
  nth0(0,_board,Line1),nth0(1,_board,Line2),nth0(2,_board,Line3),
  snapLine("  1  ", Line1),snapLine("  2  ", Line2),snapLine("  3  ", Line3),nl.

snapLine(_num, Line) :-
  nth0(0,Line,N1),nth0(1,Line,N2),nth0(2,Line,N3),
  atom_concat(_num,N1,_temp1),atom_concat(_temp1,"  ",_temp2),
  atom_concat(_temp2,N2,_temp3),atom_concat(_temp3,"  ",_temp4),
  atom_concat(_temp4,N3,_temp5),writeln(_temp5).

replace_line(_line, _index, _shape, R):-
  nth0(0,_line, Y0),nth0(1,_line, Y1),nth0(2,_line, Y2),
  (_index =:= 0 -> (append([],[_shape],T1),append(T1,[Y1],T2),append(T2,[Y2],R));
   _index =:= 1 -> (append([],[Y0],T1),append(T1,[_shape],T2),append(T2,[Y2],R));
   _index =:= 2 -> (append([],[Y0],T1),append(T1,[Y1],T2),append(T2,[_shape],R));
   append([],[],R)).

get_board_pos( _row, _col, _board,R):-
  nth0(_row,_board,_line),nth0(_col,_line,R).

create_list(Item, List, [Item|List]).

alter_board(_row, _col,_shape, _board,R):-
  nth0(0,_board,_row0),nth0(1,_board,_row1),nth0(2,_board,_row2),
  (_row =:= 0 -> (replace_line(_row0, _col, _shape, _mod),create_list(_row2, [], List1),
                  create_list(_row1, List1, List2),create_list(_mod,List2, R));
   _row =:= 1 -> (replace_line(_row1, _col, _shape, _mod),create_list(_row2, [], List1),
                  create_list(_mod,List1,List2),create_list(_row0,List2,R));
   _row =:= 2 -> (replace_line(_row2, _col, _shape, _mod),create_list(_mod, [], List1),
                  create_list(_row1,List1,List2),create_list(_row0,List2,R));
   append([],[],R)).

check_right_diagonal(_shape, _board):-
  get_board_pos(0, 0, _board, R1),
  get_board_pos(1, 1, _board, R2),
  get_board_pos(2, 2, _board, R3),
  R1 == _shape,R2 == _shape,R3 == _shape.

check_left_diagonal(_shape, _board):-
  get_board_pos(0, 2, _board, R1),
  get_board_pos(1, 1, _board, R2),
  get_board_pos(2, 0, _board, R3),
  R1 == _shape,R2 == _shape,R3 == _shape.

check_all_diagonals(_shape, _board):-
  check_left_diagonal(_shape, _board);
  check_right_diagonal(_shape, _board).

check_column(_colum, _shape, _board):-
  get_board_pos(0, _colum, _board, R1),
  get_board_pos(1, _colum, _board, R2),
  get_board_pos(2, _colum, _board, R3),
  R1 == _shape,R2 == _shape,R3 == _shape.

check_all_columns(_shape, _board):-
  check_column(0, _shape, _board);
  check_column(1, _shape, _board);
  check_column(2, _shape, _board).

check_row(_row, _shape, _board):-
  get_board_pos(_row, 0, _board, R1),
  get_board_pos(_row, 1, _board, R2),
  get_board_pos(_row, 2, _board, R3),
  R1 == _shape,R2 == _shape,R3 == _shape.

check_all_rows(_shape, _board):-
    check_row(0, _shape, _board);
    check_row(1, _shape, _board);
    check_row(2, _shape, _board).

check_for_victory(_shape, _board):-
  check_all_diagonals(_shape, _board);
  check_all_columns(_shape, _board);
  check_all_rows(_shape, _board).

pow(X,Y,Z) :- Z is X**Y.

distance(_org_row, _org_col,_des_row, _des_col, R):-
  X is (_des_col - _org_col),
  Y is (_des_row - _org_row),
  pow(X,2,X2),
  pow(Y,2,Y2),
  R2 is X2 + Y2,
  R1 is sqrt(R2),
  R is floor(R1).

is_adjacent(_org_row, _org_col,_des_row, _des_col):-
  distance(_org_row, _org_col,_des_row, _des_col, R),
  R == 1.

is_valid_plcmnt(_row, _col, _board):-
  ( (_row > 2;_row < 0) -> false;
    (_col > 2;_col < 0) -> false;
     (get_board_pos(_row,_col,_board,R),
      Y = '_', ($R == $Y) -> true; false) ).

is_valid_origin(_shape,_row, _col,_board):-
  ((_row > 2;_row < 0) -> false;
   (_col > 2;_col < 0) -> false;
     (get_board_pos(_row,_col,_board,R),
      ($R == $_shape) -> true; false)).

is_valid_mvmnt(_shape,_org_row, _org_col,_des_row, _des_col,_board):-
  is_valid_origin(_shape,_org_row, _org_col,_board),
  is_valid_plcmnt(_des_row, _des_col, _board),
  is_adjacent(_org_row, _org_col,_des_row, _des_col).

place_piece(_row, _col,_shape, _board,R):-
  alter_board(_row, _col,_shape, _board,R).

move_piece(_org_row, _org_col,_des_row, _des_col,_board, R):-
  get_board_pos(_org_row, _org_col, _board,_shape),
  alter_board(_des_row, _des_col,_shape, _board,R1),
  alter_board(_org_row, _org_col,'_', R1,R).

create_player(_name, _pieces, _shape, Player) :-
  Player = [_name, _pieces, _shape].

select_shape(Shape_one, Player_name, Shape) :-
  atom_concat(Player_name, " choose the shape of your piece:", R1),
  writeln(R1),
  read_line_to_codes(user_input, T1),
  string_to_atom(T1, _shape),
  (Shape_one \== _shape -> (Shape = _shape));
  (atom_concat(Player_name,', please choose a different shape', R2),
    writeln(R2), select_shape(Shape_one, Player_name, Shape)).

select_name(Name_one, Name) :-
  writeln("\nPlayer two choose the name of your player:"),
  read_line_to_codes(user_input, T1),
  string_to_atom(T1, _name),
  (Name_one \== _name -> (Name = _name));
  (writeln("\nPlayer two, please choose a different Name"),
    select_name(Name_one, Name)).

create_player_two_human(_pieces, Player_one, Player) :-
  nth0(0, Player_one, _name),
  nth0(2, Player_one, _shape),
  select_name(_name, Name),
  select_shape(_shape, Name, Shape),
  atom_concat('\nWelcome player ', Name, R5),
  atom_concat(R5, ' your shape is ', R6),
  atom_concat(R6, Shape, R7),
  writeln(R7),
  create_player(Name, _pieces, Shape, Player).

create_player_one_human(_pieces, Player) :-
  writeln("\nPlayer one choose the name of your player:"),
  read_line_to_codes(user_input, T1),
  string_to_atom(T1, _name),
  atom_concat("\nPlayer ", _name, R1),
  atom_concat(R1, " choose the shape of your piece:", R2),
  writeln(R2),
  read_line_to_codes(user_input, T2),
  string_to_atom(T2, _shape),
  atom_concat('\nWelcome player ', _name, R5),
  atom_concat(R5, ' your shape is ', R6),
  atom_concat(R6, _shape, R7),
  writeln(R7),
  create_player(_name, _pieces, _shape, Player).

create_player_computer(Player_one, Player) :-
  nth0(2,Player_one,_shape),
  (_shape \== 'X' -> (create_player('computer', [], 'X', Player)));
  (create_player('computer', [], 'O', Player)).

receive_placement(_board, _row, _col) :- 
  read_line_to_codes(user_input, T1),
  string_to_atom(T1, C1),
  cell_to_coord(C1, _row_1, _col_1),
  (is_valid_plcmnt(_row_1, _col_1, _board) -> (_row = _row_1, _col = _col_1);
  (writeln('\nPlease choose a valid coordinate for your placement:'), receive_placement(_board, _row, _col))). 

first_phase(_, _, _board, 0, R) :- R = _board. 
first_phase(P1, P2, _board, _rodada, R) :-
  nth0(0, P1, _name_one),
  atom_concat(_name_one, ', please choose a coordinate to place your cell:', _one),
  writeln(_one),
  receive_placement(_board, _row_1, _col_1),
  player_shape(P1, _shape_1),
  place_piece(_row_1, _col_1, _shape_1, _board, _board_new),
  snapshot_board(_board_new),
  (check_for_victory(_shape_1, _board_new) -> R = _board_new;
  (player_name(P2, N2),
  _rodada_new is (_rodada - 1),
  ((N2 \= 'Computer') ->
  (nth0(0, P2, _name_two),
    atom_concat(_name_two, ', please choose a coordinate to place your cell:', _two),
    writeln(_two),
  receive_placement(_board_new, _row_2, _col_2),
  player_shape(P2, _shape_2),
  place_piece(_row_2, _col_2, _shape_2, _board_new, _board_new_2),
  snapshot_board(_board_new_2),
  first_phase(P1, P2, _board_new_2, _rodada_new, R));
  (writeln('\nMovement of the computer.(Needs implementation)'),
  first_phase(P1, P2, _board_new, _rodada_new, R))))).

receive_movement(_board, _player, _row_ori, _col_ori, _row_from, _col_from) :- 
  nth0(0, _player, _name_player),
  atom_concat(_name_player, ', please choose a piece to be moved:', _one), 
  writeln(_one),
  read_line_to_codes(user_input, T1),
  string_to_atom(T1, C1),
  atom_concat(_name_player, ', please choose to where it should be moved:', _two),
  writeln(_two),
  read_line_to_codes(user_input, T2),
  string_to_atom(T2, C2),
  cell_to_coord(C1, _row_temp_ori, _col_temp_ori),
  cell_to_coord(C2, _row_temp_from, _col_temp_from),
  player_shape(_player, _shape_player),
  (is_valid_mvmnt(_shape_player, _row_temp_ori, _col_temp_ori, _row_temp_from, _col_temp_from, _board) -> (_row_ori = _row_temp_ori, _col_ori = _col_temp_ori, _row_from = _row_temp_from, _col_from = _col_temp_from);
  (writeln('\nPlease choose a valid movement!'), receive_movement(_board, _player, _row_ori, _col_ori, _row_from, _col_from))).

second_phase(P1, P2, _board) :-
  receive_movement(_board, P1, _row_ori_1, _col_ori_1, _row_from_1, _col_from_1),
  move_piece(_row_ori_1, _col_ori_1, _row_from_1, _col_from_1, _board, _board_new),
  snapshot_board(_board_new),
  player_shape(P1, _shape_1),
  (check_for_victory(_shape_1, _board_new) -> (victory_message(P1, _message), writeln(_message));
  (player_name(P2, N2),
  ((N2 \= 'Computer') ->
  (receive_movement(_board_new, P2, _row_ori_2, _col_ori_2, _row_from_2, _col_from_2),
  move_piece(_row_ori_2, _col_ori_2, _row_from_2, _col_from_2, _board_new, _board_new_2),
  snapshot_board(_board_new_2),
  player_shape(P2, _shape_2),
  (check_for_victory(_shape_2, _board_new_2) -> (victory_message(P2, _message_2), writeln(_message_2));
  second_phase(P1, P2, _board_new_2)));
  (writeln('\nMovement of the computer.(Needs implementation)'),
  second_phase(P1, P2, _board_new))))).  

victory_message(_player, _message) :-
  player_name(_player, _name), 
  atom_concat('\nPlayer ', _name, R),
  atom_concat(R, ' has won!\n', _message).  

/**
* player_info(_index, _player, _info) :- nth0(_index, _player, _info).
*
* 0 - _name
* 1 - _pieces
* 2 - _shape
*/

player_name([_name, _, _], _name).
player_pieces([_, _pieces, _], _pieces).
player_shape([_, _, _shape], _shape).

welcome_screen() :-
  writeln("\t################################################################"),
  writeln("\t#                MAREL - GAME OF THE THREE TRAILS              #"),
  writeln("\t#                   Play and have lots of fun                  #"),
  writeln("\t#                                                              #"),
  writeln("\t################################################################"),
  writeln("\nChoose an option:"),
  writeln("Option (1): play with a friend."),
  writeln("Option (2): play with the computer."),
  writeln("Option (anything): quit the game."),
  writeln("\nOption is: ").

:- initialization main.

main :-
  _board = [['_','_','_'],['_','_','_'],['_','_','_']],
  /** Menu and options*/
  welcome_screen(),
  read_line_to_codes(user_input, T1),
  string_to_atom(T1, OP),
  /** Creating players according to the options or quit the game*/
  ((OP == '1', create_player_one_human([], P1), create_player_two_human([], P1, P2));
  (OP == '2', create_player_one_human([], P1), create_player_computer(P1, P2));
  (halt(0))),
  writeln('\n-- The first phase ---'),
  snapshot_board(_board),
  first_phase(P1, P2, _board, 3, _board_new),
  player_shape(P1, _shape_1),
  player_shape(P2, _shape_2),
  ((check_for_victory(_shape_1, _board_new), victory_message(P1, _message), writeln(_message));
  (check_for_victory(_shape_2, _board_new), victory_message(P2, _message), writeln(_message));
  (writeln('\n-- The second phase ---'),
  snapshot_board(_board_new),
  second_phase(P1, P2, _board_new))),
  halt(0).
