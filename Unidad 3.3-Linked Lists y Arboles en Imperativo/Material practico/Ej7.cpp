#include <iostream>
#include "Tree.h"
#include "../../Unidad 3.2-Punteros y Arrays/Material Practico/ArrayList.h"
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
    if(!isEmptyT(t)) {
        return 1 + max(heightT(left(t)), heightT(right(t)));
    } else {
        return 0;
    }
}

// 6. Dado un árbol devuelve una lista con todos sus elementos. 
ArrayList toList(Tree t) {
    if(!isEmptyT(t)) {
        ArrayList r = newArrayList();
        add(rootT(t), r);
        return append(append(r, toList(left(t))), toList(right(t)));
    } else {
        return newArrayList();
    }
    
}


// 7. Dado un árbol devuelve los elementos que se encuentran en sus hojas.
ArrayList leaves(Tree t) {
    if(!isEmptyT(t)) {
        if(isEmptyT(left(t)) && isEmptyT(right(t))) {
            ArrayList r = newArrayList();
            add(rootT(t), r);
            return r;
        } else {
            return append(leaves(left(t)), leaves(right(t)));
        }
    } else {
        return newArrayList();
    }
}

//8. Dados un número n y un árbol devuelve una lista con los nodos de nivel n.
ArrayList levelN(int n, Tree t) {
    if(!isEmptyT(t)) {
        if(n == 0) {
            ArrayList r = newArrayList();
            add(rootT(t), r);
            return r;
        } else {
            return append(levelN(n-1, left(t)), levelN(n-1, right(t)));
        }
    } else {
        return newArrayList();
    }
}




int main() {
    Tree left = nodeT(2, nodeT(8, NULL, NULL), nodeT(8, NULL, NULL));

    Tree right = nodeT(5, nodeT(8, NULL, NULL), nodeT(7, nodeT(8, NULL, NULL), NULL));
    
    Tree t = nodeT(1, left, right);

    ArrayList al = levelN(3,t);

    //cout << "Altura del arbol: " << heightT(t) << endl;    
    //cout << "Apariciones 5: " << aparicionesT(5,t) << endl; 
     
    cout << lengthAL(al) << endl;
    for(int i=0; i<lengthAL(al); i++) {
        cout << "AL[" << i << "]: "  << get(i,al) << endl;
    }  
}