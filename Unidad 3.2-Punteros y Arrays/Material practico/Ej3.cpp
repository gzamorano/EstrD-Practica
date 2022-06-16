#include <iostream>
#include "ArrayList.h"
using namespace std;

int main() {
    ArrayList AL = newArrayListWith(5);

    add(20, AL);
    add(88, AL);
    add(30, AL);
    add(56, AL);

    // for(int i=0; i<lengthAL(AL); i++) {
    //     cout << "AL[" << i << "]: "  << get(i,AL) << endl;
    // }

    // resize(2, AL);
    // cout << endl;
    
    for(int i=0; i<lengthAL(AL); i++) {
        cout << "AL[" << i << "]: "  << get(i,AL) << endl;
    }
    
    resize(10, AL);
    add(21, AL);
    add(22, AL);
    add(23, AL);
    add(24, AL);
    add(25, AL);
    add(26, AL);
    cout << endl;
    for(int i=0; i<lengthAL(AL); i++) {
        cout << "AL[" << i << "]: "  << get(i,AL) << endl;
    }
    add(30, AL);
    //cout << get(4, AL) << endl;
    
}