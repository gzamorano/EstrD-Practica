#include <iostream>
#include "LinkedList.h"
using namespace std;


int main() {

    LinkedList list = nil();

    Snoc(4,list);
    Snoc(5,list);
    Snoc(6,list);
    Snoc(7,list);

    

    string s;
    if(isEmpty(list)){
        s = "Si";
    } else {
       s = "No";
    }

    //cout << "El primer elemento de la lista es: " << head(list) << endl;
    //cout << "La longitud de la lista es: " << length(list) << endl;

    ListIterator iterador = getIterator(list);
    while(!atEnd(iterador)){
        SetCurrent(current(iterador), iterador);
        cout << current(iterador) << endl;
        Next(iterador);
    }
    cout << current(iterador) << endl;
    DisposeIterator(iterador);
}