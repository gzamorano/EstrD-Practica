#include <iostream>
#include "LinkedList.h"
#include "Set.h"
using namespace std;


void printLL(LinkedList xs) {
    if(length(xs) != 0) {
        ListIterator iterador = getIterator(xs);
        while(!atEnd(iterador)){
            cout << current(iterador) << ", ";
            Next(iterador);
        }
        cout << current(iterador) << endl;
        DisposeIterator(iterador);
    } else {
        cout << "No hay elementos en la lista" << endl;
    }
}



int main() {
    Set conj = emptyS();

    AddS(15, conj);
    AddS(10, conj);

    RemoveS(15,conj);
   
    cout << "tamanio conjunti: " << sizeS(conj) << endl;

    //printLL(setToList(conj));
}