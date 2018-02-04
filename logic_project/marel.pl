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



:- initialization main.

main :-
  _board = [['_','S','_'],['@','_','_'],['_','_','&']],
  snapshot_board(_board),
  % get_board_pos(2,2,_board,R),
  % nth0(0,_board,Line1),
  % replace_line(Line1, 2, '@', R),
  alter_board(1, 1,'%', _board,R),
  snapshot_board(R),
  halt(0).
