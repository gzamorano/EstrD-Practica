#include <iostream>
#include "LinkedList.h"
using namespace std;


int main() {

    LinkedList list = nil();

    string s;
    if(isEmpty(list)){
        s = "Si";
    } else {
       s = "No";
    }

    Cons(20, list);
    Cons(35, list);
    Cons(9, list);

    Tail(list);

    //cout << "El primer elemento de la lista es: " << head(list) << endl;
    cout << "La longitud de la lista es: " << length(list) << endl;
}