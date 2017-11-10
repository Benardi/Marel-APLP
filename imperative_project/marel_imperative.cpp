#include <iostream>
using namespace std;

char marel_board [3][3] = { {'_','_','_'}, {'_','_','_'}, {'_','_','_'} };
char column_index [3] = {'A', 'B', 'C'};

struct Coordinate {
  int row;
  int column;
};

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

bool check_move(struct Coordinate current_coordinate, 
  struct Coordinate final_coordinate) {
  if (current_coordinate.row < 0 || current_coordinate.row > 2 
    || current_coordinate.column < 0 || current_coordinate.column > 2) {
    return false;
  } else if (marel_board[current_coordinate.row][current_coordinate.column] == '_') {
    return false;
  } else if (! check_coordinate(final_coordinate)) {
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

int main() {
  snapshot_board(marel_board);

  struct Coordinate coordinate1, coordinate2, coordinate3;
  coordinate1.row = 1;
  coordinate1.column = -1;
  coordinate2.row = 1;
  coordinate2.column = 1;
  coordinate3.row = 2;
  coordinate3.column = 2;

  place_piece('X', coordinate1); // rejected
  place_piece('O', coordinate2); // accepted
  place_piece('X', coordinate3); // accepted
  place_piece('O', coordinate3); // rejected

  snapshot_board(marel_board);

  struct Coordinate current_coordinate, final_coordinate1, final_coordinate2;
  current_coordinate.row = 1;
  current_coordinate.column = 1;
  final_coordinate1.row = 2;
  final_coordinate1.column = 1;
  final_coordinate2.row = 2;
  final_coordinate2.column = 2;

  cout << check_move(current_coordinate, final_coordinate1) << endl; // accept
  cout << check_move(current_coordinate, final_coordinate2) << endl; // rejected

  return 0;
}
