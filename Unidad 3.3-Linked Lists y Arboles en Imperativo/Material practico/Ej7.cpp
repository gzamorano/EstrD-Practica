#include <iostream>
#include "Tree.h"
using namespace std;

// 1. Dado un árbol binario de enteros devuelve la suma entre sus elementos. 
int sumarT(Tree t) {
    if(isEmptyT(t)) {
        return 0;
    } else {
        return rootT(t) + sumarT(left(t)) + sumarT(right(t));
    }
}

// 2. Dado un árbol binario devuelve su cantidad de elementos, es decir, el tamaño del árbol (size en inglés).
int sizeT(Tree t)




int main() {
    Tree empty = emptyT();

    Tree left = nodeT(2, nodeT(3, empty, empty), nodeT(4, empty, empty));

    Tree right = nodeT(5, nodeT(6, empty, empty), nodeT(7, empty, empty));
    
    Tree t = nodeT(1, left, right);

    cout << "Suma del arbol: " << sumarT(t) << endl;    

}