#include <iostream>
#include "Set.h"
#include "LinkedList.h"
using namespace std;

struct NodoS {
    int elem;         // valor del nodo
    NodoS* siguiente; // puntero al siguiente nodo
}
struct SetSt {
    int cantidad;   // cantidad de elementos diferentes
    NodoS* primero; // puntero al primer nodo
}

Set emptyS() {
    SetSt* s = new SetSt;
    s->cantidad = 0;
    s->primero = NULL;
    return s;
}

bool isEmptyS(Set s) {
    return (s->cantidad > 0);
}

bool belongsS(int x, Set s) {
    bool b = false;
    int i=0;
    int current = s->primero;
    while(i < s->cantidad) {
        if(x == current->elem){
            return !b;
        }
        current = current->siguiente;
        i++;
    }
    return b;
}

// agrego por delante porque cuesta O(1), pero al hacer belongS se vuelve O(N)
void addS(int x, Set s) {
    if(!belongS(x, s)) {
        NodoS* n = new NodoS;
        n->elem = x;
        n->siguiente = s->primero;
        s->primero = n;
        s->cantidad++;
    }
}

// Prec: existe el elemento a eliminar en el conjunto dada
void RemoveS(int x, Set s) {
    int i=0;
    int current = s->primero;
    while(i < s->cantidad) {
        if(x == current->siguiente){
            NodoS* temp = current->siguiente;
            current->siguiente = current->siguiente->siguiente;
            delete temp;
        }
        current = current->siguiente;
        i++;
    }
}

int sizeS(Set s) {
    return s->cantidad;
}

LinkedList setToList(Set s) {
    
    
    return ;
}