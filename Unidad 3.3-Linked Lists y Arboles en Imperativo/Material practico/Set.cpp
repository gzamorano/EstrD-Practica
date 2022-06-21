#include <iostream>
#include "Set.h"
#include "LinkedList.h"
using namespace std;

struct NodoS {
    int elem;         // valor del nodo
    NodoS* siguiente; // puntero al siguiente nodo
};
struct SetSt {
    int cantidad;   // cantidad de elementos diferentes
    NodoS* primero; // puntero al primer nodo
};

Set emptyS() {
    SetSt* s = new SetSt;
    s->cantidad = 0;
    s->primero = NULL;
    return s;   
}

bool isEmptyS(Set s) {
    return (s->primero == NULL);
}

bool belongsS(int x, Set s) {
    bool b = false;
    int i=0;
    NodoS* current = s->primero;
    while(i < s->cantidad) {
        if(x == current->elem){
            return !b;
        }
        current = current->siguiente;
        i++;
    }
    delete current;
    return b;
}

// agrego por delante porque cuesta O(1), pero al hacer belongS se vuelve O(N)
void AddS(int x, Set s) {
    if(!belongsS(x, s)) {
        NodoS* n = new NodoS;
        n->elem = x;
        n->siguiente = s->primero;
        s->primero = n;
        s->cantidad++;
    }
}

// Prec: existe el elemento a eliminar en el conjunto dado
// probar usando un puntero al nodo previo
void RemoveS(int x, Set s) {
    int i=0;
    NodoS* current = s->primero;
    while(i < s->cantidad) {
        // Cuando el conjunto tiene un solo elemento
        if(current->siguiente == NULL && x == current->elem) {
            NodoS* temp = current;
            s->primero = current->siguiente;
            s->cantidad--;
        }
        // Cuando el conjunto tiene más de un elemento
        if(current->siguiente != NULL && x == current->siguiente->elem){
            NodoS* temp = current->siguiente;
            current->siguiente = current->siguiente->siguiente;
            s->cantidad--;
            delete temp;
        }
        current = current->siguiente;
        i++;
    }
    delete current;
}

int sizeS(Set s) {
    return s->cantidad;
}

LinkedList setToList(Set s) {
    LinkedList list = nil();
    int i=0;
    NodoS* current = s->primero;
    while(i < sizeS(s)) {
        Cons(current->elem, list);
        current = current->siguiente;
        i++;
    }
    delete current;
    return list;
}

void DestroyS(Set s) {
    delete s;
}