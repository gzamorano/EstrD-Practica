#include <iostream>
using namespace std;

int nextPow2(int n, int m) {
    if(m>n){
      return m;
    } else {
      return nextPow2(n, m*m);
    }
}

int main() {
  cout << nextPow2(4,2) ;
}