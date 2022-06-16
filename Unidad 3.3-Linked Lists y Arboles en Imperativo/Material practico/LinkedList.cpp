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


int head(LinkedList xs) {
    return (xs->primero->elem);
}

void Cons(int x, LinkedList xs) {
    // creacion del nodo
    NodoL* nodo = new NodoL;
    nodo->elem = x;
    // Se verifica si la lista esta vacia para enlazar el siguiente nodo.
    if (xs->primero != NULL) {
        nodo->siguiente = xs->primero;
    } else {
        nodo->siguiente = NULL;
    }
    // Se asigna el nodo como primero de la lista (adelante de todo) y se aumenta el size.   
    xs->primero = nodo;
    xs->cantidad++;
}

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
     // Caso cuando está vacía
    if(xs->primero == NULL) {
        nodo->siguiente = NULL;
        xs->primero = nodo;
    }
    NodoL* current = xs->primero;
    // Mientras no esté vacía, se recorre
    while(current != NULL) {
        if (current->siguiente == NULL) {
            current->siguiente = nodo;
            nodo->siguiente = NULL;
        }
        current = current->siguiente;
    }
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
    return (ixs->current == NULL);
}

void DisposeIterator(ListIterator ixs) {
    delete ixs->current;
    delete ixs;
}

void DestroyL(LinkedList xs) {
    delete xs;
}
