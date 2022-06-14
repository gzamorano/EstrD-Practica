#include <iostream>
#include "ArrayList.h"
using namespace std;

struct ArrayListSt {
    int  cantidad;   // cantidad de elementos
    int* elementos;  // array de elementos
    int  capacidad;  // tamaño del array
};

ArrayList newArrayList() {
    ArrayList arrayLi = new ArrayListSt;
    arrayLi->cantidad  = 0;
    arrayLi->capacidad = 16;
    arrayLi->elementos = new int[16];

    return arrayLi;
}

ArrayList newArrayListWith(int capacidad) {
    ArrayList arrayLi = new ArrayListSt;
    arrayLi->cantidad  = 0;
    arrayLi->capacidad = capacidad;
    arrayLi->elementos = new int[capacidad];

    return arrayLi;
}

int lengthAL(ArrayList xs) {
    return (xs->cantidad);
}

int get(int i, ArrayList xs) {
    return (xs->elementos[i]); 
}

void set(int i, int x, ArrayList xs) {
    xs->elementos[i] = x;
}

void resize(int capacidad, ArrayList xs) {
    int* temp = new int[capacidad];
    for(int i=0; i<(lengthAL(xs)-1); i++) {
        temp[i] = xs->elementos[i];
    }
    if (capacidad < lengthAL(xs)) {
        xs->cantidad = capacidad;
    }
    delete xs->elementos;
    xs->capacidad = capacidad;
    xs->elementos = temp;
}

void add(int x, ArrayList xs) {
    if (lengthAL(xs) < xs->capacidad) {
        xs->elementos[lengthAL(xs)] = x;
        xs->cantidad++;
    } else {
        cout << "No es posible agregar más elementos al ArrayList, esta lleno" << endl;
    }
}

void remove(ArrayList xs) {
    if(lengthAL(xs) > 0) {
        xs->cantidad--; 
    } else {
        cout << "No se puede borrar elemento, el ArrayList esta vacio" << endl;
    }
}