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
int sizeT(Tree t) {
    if(t == NULL) {
        return 0;
    } else {
        return 1 + sizeT(left(t)) + sizeT(right(t));
    }
}

// 3. Dados un elemento y un árbol binario devuelve True si existe un elemento igual a ese en el árbol.
bool perteneceT(int e, Tree t) {
   if(!isEmptyT(t)){
        return e == rootT(t) || perteneceT(e, left(t)) || perteneceT(e, right(t));
   } else {
        return false;
   }
}

// 4. Dados un elemento e y un árbol binario devuelve la cantidad de elementos del árbol que son
// iguales a e.
int aparicionesT(int e, Tree t) {
    int c = 0;
    if(!isEmptyT(t)){
        if(e == rootT(t)){
            c++;    
        }
        return c + aparicionesT(e, left(t)) + aparicionesT(e, right(t)); 
    }
    return c;
}

// 5. Dado un árbol devuelve su altura.
int heightT(Tree t) {
    // sumar 1 + el maximo entre la altura de left y right cuando no es vacio
    // cuando es vacio devolver 0
    return 0;
}






int main() {
    Tree left = nodeT(2, nodeT(5, NULL, NULL), nodeT(4, NULL, NULL));

    Tree right = nodeT(5, nodeT(6, NULL, NULL), nodeT(7, nodeT(5, NULL, NULL), NULL));
    
    Tree t = nodeT(1, left, right);

    //cout << "Cantidad de elementos: " << sizeT(t) << endl;    
    cout << "Apariciones 5: " << aparicionesT(5,t) << endl;    

}