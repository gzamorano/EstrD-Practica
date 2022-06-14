#include <iostream>
#include "ArrayList.h"
using namespace std;

// 1. Devuelve la suma de todos los elementos
int sumatoria(ArrayList xs) {
    int r = 0;
    for(int i=0; i<lengthAL(xs); i++) {
        r += get(i,xs);
    }
    return r;
}

// 2. Incrementa en uno todos los elementos.
void sucesores(ArrayList xs) {
    for(int i=0; i<lengthAL(xs); i++) {
        set(i, get(i,xs)+1, xs);
    }
}

// 3. Indica si el elemento pertenece a la lista.
bool pertenece(int x, ArrayList xs) {
    bool b = false;
    for(int i=0; i<lengthAL(xs); i++) {
        if(x == get(i,xs)) {
            return !b;
        }
    }
    return b;
}

// 4. Indica la cantidad de elementos iguales a x.
int apariciones(int x, ArrayList xs) {
    int c = 0;
    for(int i=0; i<lengthAL(xs); i++) {
        if(x == get(i,xs)) {
            c++;
        }
    }
    return c;
}


// 5. Crea una nueva lista a partir de la primera y la segunda (en ese orden). 
ArrayList append(ArrayList xs, ArrayList ys) {
    ArrayList zs = newArrayListWith((lengthAL(xs)+lengthAL(ys))*2);
    for(int i=0; i<lengthAL(xs); i++) {
        add(get(i,xs),zs);
    }
    for(int i=0; i<lengthAL(ys); i++) {
        add(get(i,ys),zs);
    }
    return zs;
}

// 6. Devuelve el elemento más chico de la lista. (La lista no debe estar vacía)
int minimo(ArrayList xs) {
    int min = get(0,xs);
    for(int i=1; i<lengthAL(xs); i++) {
        if(get(i,xs) < min) {
            min = get(i,xs);
        }
    }
    return min;
}





int main() {
    ArrayList al = newArrayList();

    add(1,al);
    add(2,al);
    add(3,al);
    add(4,al);

    ArrayList al2 = newArrayList();

    add(5,al2);
    add(6,al2);
    add(7,al2);
    add(8,al2);
    add(1,al2);

    ArrayList juntas = append(al,al2);

    // string s;
    // if(pertenece(7,al)) {
    //     s = "Si";
    // } else {
    //     s = "No";
    // }

   
    cout << "El minimo de al2: " << minimo(al2) << endl;


    remove(al);
    for(int i=0; i<lengthAL(al); i++) {
        cout << "AL[" << i << "]: "  << get(i,al) << endl;
    }
}