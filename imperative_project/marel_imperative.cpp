#include <iostream>
#include <string>
using namespace std;

char marel_board [3][3] = { {'_','_','_'}, {'_','_','_'}, {'_','_','_'} };
char column_index [3] = {'A', 'B', 'C'};

struct Coordinate {
  int row;
  int column;
};

struct Piece {
  char shape;
  Coordinate coordinate;
};

struct Player {
  string name;
  Piece pieces[3];
};

Player create_player(string name, char piece_shape) {
  struct Player player;

  struct Piece piece1, piece2, piece3;

  struct Coordinate coord1, coord2, coord3;

  coord1.row = -1;
  coord1.column = -1;
  coord2.row = -1;
  coord2.column = -1;
  coord3.row = -1;
  coord3.column = -1;

  piece1.shape = piece_shape;
  piece1.coordinate = coord1;

  piece2.shape = piece_shape;
  piece2.coordinate = coord2;

  piece3.shape = piece_shape;
  piece3.coordinate = coord3;

  player.name = name;
  player.pieces[0] = piece1;
  player.pieces[1] = piece2;
  player.pieces[2] = piece3;

  return player;
}

bool check_coordinate(struct Coordinate coordinate) {
  if (coordinate.row < 0 || coordinate.row > 2
    || coordinate.column < 0 || coordinate.column > 2) {
    return false;
  } else if (marel_board[coordinate.row][coordinate.column] != '_') {
    return false;
  }

  return true;
}

void place_piece(char piece, struct Coordinate coordinate) {
  if (check_coordinate(coordinate)) {
    marel_board[coordinate.row][coordinate.column] = piece;
  }
}

bool check_move(char current_player_piece_shape, struct Coordinate current_coord,
  struct Coordinate final_coord) {
  char piece_shape = marel_board[current_coord.row][current_coord.column];

  if (current_coord.row < 0 || current_coord.row > 2
    || current_coord.column < 0 || current_coord.column > 2) {
    return false;
  } else if (piece_shape == '_') {
    return false;
  } else if (current_player_piece_shape != piece_shape) {
    return false;
  } else if (! check_coordinate(final_coord)) {
    return false;
  }

  return true;
}

void snapshot_board(char platform [3][3]) {
  endl(cout);
  cout << ' ' << ' ';

  for(int i = 0; i < 3; i++){
    cout << ' ' << column_index[i];
  }

  endl(cout);

  for(int i = 0; i < 3; i++){
    cout << ' ' << i+1 << ' ';
    for(int j = 0; j < 3; j++){
      cout << platform[i][j] << ' ';
    }

    endl(cout);
  }

  endl(cout);
}

bool move_piece(char current_player_piece_shape, struct Coordinate current_coord,
  struct Coordinate final_coord) {
    if (check_move(current_player_piece_shape, current_coord, final_coord)) {
      marel_board[final_coord.row][final_coord.column] = marel_board[current_coord.row][current_coord.column];
      marel_board[current_coord.row][current_coord.column] = '_';
      return true;
  } else {
      return false;
  }
}

Player get_human_player() {
  string name;
  char piece_shape;
  cin >> name;
  cin >> piece_shape;

  return create_player(name, piece_shape);
}

Player get_computer_player(char human_piece_shape) {
  string name = "Computer";
  char piece_shape = 'X';

  if (human_piece_shape == 'X') {
    piece_shape = 'O';
  }

  return create_player(name, piece_shape);
}

int main() {
  
  Player human_player = get_human_player();
  Player computer_player = get_computer_player(human_player.pieces[0].shape);  

  snapshot_board(marel_board);

  struct Coordinate coord1, coord2, coord3;
  coord1.row = 1;
  coord1.column = -1;
  coord2.row = 1;
  coord2.column = 1;
  coord3.row = 2;
  coord3.column = 2;

  place_piece('X', coord1); // rejected
  place_piece('O', coord2); // accepted
  place_piece('X', coord3); // accepted
  place_piece('O', coord3); // rejected

  snapshot_board(marel_board);

  struct Coordinate current_coord, final_coord1, final_coord2;
  current_coord.row = 1;
  current_coord.column = 1;
  final_coord1.row = 2;
  final_coord1.column = 1;
  final_coord2.row = 2;
  final_coord2.column = 2;

  // moves the piece to a position below if the piece shape of the human player is O
  char current_player_piece_shape = human_player.pieces[0].shape;

  move_piece(current_player_piece_shape, current_coord, final_coord1);
  snapshot_board(marel_board);

  return 0;
}
