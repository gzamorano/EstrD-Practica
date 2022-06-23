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
    delete temp;
    q->cantidad--;
}

int lengthQ(Queue q) {
    return q->cantidad;
}

void MergeQ(Queue q1, Queue q2) {
    if(isEmptyQ(q1)) {
        q1->primero = q2->primero;
    } else {
        q1->ultimo->siguiente = q2->primero;
    }
    q1->ultimo = q2->ultimo;
    q1->cantidad += q2->cantidad;
    delete q2;
}

void DestroyQ(Queue q) {
    delete q;
}


