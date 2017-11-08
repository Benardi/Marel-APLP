#include <iostream>
using namespace std;

int marel_board [3][3] = { {0,1,2}, {1, 0,2}, {1,2,0} };



char map_cell(int content){
  if(content == 1){
    return 'X';
  }
  else if (content == 2){
    return 'O';
  }
  else {
   return '_';
  }

}

void snapshot_board(int platform [3][3]){
  endl(cout);
  for(int i = 0; i < 3; i++){
    for(int j = 0; j < 3; j++){
      cout << map_cell(platform[i][j]) << ' ';
    }
    endl(cout);
  }
  endl(cout);
}

int main()
{
  snapshot_board(marel_board);

}
