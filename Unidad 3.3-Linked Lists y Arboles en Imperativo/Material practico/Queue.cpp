#include <iostream>
#include "Queue.h"
using namespace std;

struct NodoQ {
    int elem; // valor del nodo
    NodoQ* siguiente; // puntero al siguiente nodo
};
struct QueueSt {
    int cantidad; // cantidad de elementos
    NodoQ* primero; // puntero al primer nodo
    NodoQ* ultimo; // puntero al ultimo nodo
};


Queue emptyQ() {
    QueueSt* q = new QueueSt;
    q->cantidad = 0;
    q->primero = NULL;
    q->ultimo = NULL;
    return q;
}

bool isEmptyQ(Queue q) {
    return (q->cantidad == 0);
}

// Prec: la cola no está vacía
int firstQ(Queue q) {
    return (q->primero->elem);
}
// esta de ver el ultimo elemento va de onda, no estaría en la interfaz pedida
int lastQ(Queue q) {
    return (q->ultimo->elem);
}

void Enqueue(int x, Queue q) {
    NodoQ* n = new NodoQ;
    n->elem = x;
    n->siguiente = NULL;
    if(q->ultimo == NULL) {
        q->primero = n;
    } else {
        q->ultimo->siguiente = n;
    }
    q->ultimo = n;
    q->cantidad++;  
}

// Prec: la cola no está vacía
void Dequeue(Queue q) {
    NodoQ* temp = q->primero;
    q->primero = q->primero->siguiente;
    // if(q->primero == NULL) {
    //     q->ultimo = NULL;
    // }
    delete temp;
    q->cantidad--;
}

int lengthQ(Queue q) {
    return q->cantidad;
}



