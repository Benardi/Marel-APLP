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
  is_valid_plcmnt(_des_row, _des_col, _board).

place_piece(_row, _col,_shape, _board,R):-
  alter_board(_row, _col,_shape, _board,R).

move_piece(_org_row, _org_col,_des_row, _des_col,_board, R):-
  get_board_pos(_org_row, _org_col, _board,_shape),
  alter_board(_des_row, _des_col,_shape, _board,R1),
  alter_board(_org_row, _org_col,'_', R1,R).

create_player(_name, _pieces, _shape, Player) :-
  Player = [_name, _pieces, _shape].

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

:- initialization main.

main :-
  _board = [['_','S','_'],['@','_','_'],['_','_','&']],
  snapshot_board(_board),
  place_piece(2, 1, '%', _board,R),
  place_piece(1, 1, '%', R,R1),
  place_piece(0, 1, '%', R1,R2),
  move_piece(0, 1,0,2,R2, R3),
  snapshot_board(R3),
  (check_for_victory('%',R3) -> writeln('Winner');writeln('No Winner')),
  (is_valid_mvmnt('%',0,1,2,2,R3) -> writeln('Valid');writeln('Invalid')),
  /**Player 1*/
  create_player('Lucas', [], 'O', X), 
  player_name(X, _name1), writeln(_name1),
  player_pieces(X, _pieces1), writeln(_pieces1),
  player_shape(X, _shape1), writeln(_shape1),
  /**Player 2*/
  create_player('Carlos', [], 'Z', Y),
  player_name(Y, _name2), writeln(_name2),
  player_pieces(Y, _pieces2), writeln(_pieces2),
  player_shape(Y, _shape2), writeln(_shape2).
