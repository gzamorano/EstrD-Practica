#include <iostream>
#include "LinkedList.h"
using namespace std;

struct NodoL {
    int elem; // valor del nodo
    NodoL* siguiente; // puntero al siguiente nodo
};

struct LinkedListSt {
    // INV.REP.: cantidad indica la cantidad de nodos que se pueden recorrer
    // desde primero por siguiente hasta alcanzar a NULL
    int cantidad; // cantidad de elementos
    NodoL* primero; // puntero al primer nodo
};

struct IteratorSt {
    NodoL* current;
};


LinkedList nil() {
    LinkedListSt* list = new LinkedListSt;
    list->cantidad = 0;
    list->primero = NULL;

    return list;
}

bool isEmpty(LinkedList xs) {
    return (xs->cantidad == 0);
}

// Prec: no puede ser vacio xs
int head(LinkedList xs) {
    return (xs->primero->elem);
}

void Cons(int x, LinkedList xs) {
    // creacion del nodo
    NodoL* nodo = new NodoL;
    nodo->elem = x;
    nodo->siguiente = xs->primero;
    // Se asigna el nodo como primero de la lista (adelante de todo) y se aumenta el size.   
    xs->primero = nodo;
    xs->cantidad++;
}

// Prec: xs no es vacio
void Tail(LinkedList xs) {
    NodoL* temp = xs->primero;
    xs->primero = xs->primero->siguiente;
    delete temp;
    xs->cantidad--;
}

int length(LinkedList xs) {
    return (xs->cantidad);
}

void Snoc(int x, LinkedList xs) {
    //Creacion del nodo
    NodoL* nodo = new NodoL;
    nodo->elem = x;
    nodo->siguiente = NULL;
     // Caso cuando está vacía
    if(xs->cantidad == 0) {
        xs->primero = nodo;
    } else {
        // Mientras no esté vacía, se recorre
        NodoL* current = xs->primero;
        while(current->siguiente != NULL) {
            current = current->siguiente;
        }
        current->siguiente = nodo;
    } 
    xs->cantidad++;
}

ListIterator getIterator(LinkedList xs) {
    IteratorSt* iterador = new IteratorSt;
    iterador->current = xs->primero;
    return iterador;
}

int current(ListIterator ixs) {
    return (ixs->current->elem);
}

void SetCurrent(int x, ListIterator ixs) {
    ixs->current->elem = x;
}

void Next(ListIterator ixs) {
    ixs->current = ixs->current->siguiente;
}

bool atEnd(ListIterator ixs) {
    return (ixs->current->siguiente == NULL);
}

void DisposeIterator(ListIterator ixs) {
    delete ixs->current;
    delete ixs;
}

void DestroyL(LinkedList xs) {
    delete xs;
}
