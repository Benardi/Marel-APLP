snapshot_board(Board) :-
  writeln("\n     A  B  C"),
  nth0(0,Board,Line1),
  nth0(1,Board,Line2),
  nth0(2,Board,Line3),
  snapLine("  1  ", Line1),
  snapLine("  2  ", Line2),
  snapLine("  3  ", Line3),nl.

snapLine(_num, Line) :-
  nth0(0,Line,N1),
  nth0(1,Line,N2),
  nth0(2,Line,N3),
  atom_concat(_num,N1,_temp1),
  atom_concat(_temp1,"  ",_temp2),
  atom_concat(_temp2,N2,_temp3),
  atom_concat(_temp3,"  ",_temp4),
  atom_concat(_temp4,N3,_temp5),
  writeln(_temp5).




:- initialization main.

main :-
  Board = [['_','S','_'],['@','_','_'],['_','_','&']],
  snapshot_board(Board),
  halt(0).
