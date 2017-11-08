#include <iostream>
using namespace std;

int marel_board [3][3] = {{ 0,1,2 }, {3,4,5}, {6,7,8} };


void snapshot_board(int platform [3][3]) {

  for(int i = 0; i < 3; i++){
    for(int j = 0; j < 3; j++){
      cout << platform[i][j] << ' ';
    }
    endl(cout);
  }


}

int main()
{
  snapshot_board(marel_board);

}
