#include <iostream>
#include "LinkedList.h"
using namespace std;

// Crear las funciones utilizando la interfaz de LinkedList, indicar costos.

// (N) siendo N la cantidad de elementos de xs
// 1. Devuelve la suma de todos los elementos.
int sumatoria(LinkedList xs) {
    int sum = 0;
    if(length(xs) != 0) {
        ListIterator iterador = getIterator(xs);
        while(!atEnd(iterador)){
            sum += current(iterador);
            Next(iterador);
        }
        // sumo el último elemento, caso borde
        sum += current(iterador);
        DisposeIterator(iterador);
    }
    return sum;
}

// O(N) siendo N la cantidad de elementos de xs
// 2. Incrementa en uno todos los elementos.
void Sucesores(LinkedList xs) {
    if (length(xs) != 0) {
        ListIterator iterador = getIterator(xs);
        while(!atEnd(iterador)) {
            SetCurrent((current(iterador)+1), iterador);
            Next(iterador);
        }
        SetCurrent((current(iterador)+1), iterador);
        DisposeIterator(iterador);
    }
}


// O(N) siendo N la cantidad de elementos de xs
// 3. Indica si el elemento pertenece a la lista.
bool pertenece(int x, LinkedList xs) {
    bool b = false;
    if(length(xs) != 0) {
        ListIterator iterador = getIterator(xs);
        while(!atEnd(iterador)) {
            if (current(iterador) == x) {
                return !b;
            }
            Next(iterador);
        }
        if (current(iterador) == x) {
            return !b;
        }    
        DisposeIterator(iterador);
    }
    return b;
}

// O(N) siendo N la cantida de elementos de xs
// 4. Indica la cantidad de elementos iguales a x. 
int apariciones(int x, LinkedList xs) {
    int a = 0;
    if(length(xs) != 0) {
        ListIterator iterador = getIterator(xs);
        while(!atEnd(iterador)) {
            if (current(iterador) == x) {
                a++;
            }
            Next(iterador);
        }
        if (current(iterador) == x) {
            a++;
        }    
        DisposeIterator(iterador);
    }
    return a;
}

// O(N) siendo N la cantidad de elementos de xs
// 5. Devuelve el elemento más chico de la lista.
int minimo(LinkedList xs) {
    ListIterator iterador = getIterator(xs);
    int min = current(iterador);
    while(!atEnd(iterador)) {
        if (current(iterador) < min) {
            min = current(iterador);
        }
        Next(iterador);
    }
    if (current(iterador) < min) {
        min = current(iterador);
    }
    DisposeIterator(iterador);
    return min;
}


// O(N^2) siendo N la cantidad de elementos de la lista xs, la cual se recorre por iteración, y en cada
// instancia de la misma se realiza la operación Snoc de costo lineal sobre la lista.
// 6.  Dada una lista genera otra con los mismos elementos, en el mismo orden.
//     Nota: notar que el costo mejoraría si Snoc fuese O(1), ¿cómo podría serlo?
// Para que Snoc tenga costo constante, se debe agregar last al TAD, entonces
// solo se tendrían que organizar los punteros sin importar la cantidad de elementos que haya.
LinkedList copy(LinkedList xs) {
    LinkedList copia = nil();
    ListIterator iterador = getIterator(xs);
    while(!atEnd(iterador)) {
        Snoc(current(iterador), copia);
        Next(iterador);
    }
    Snoc(current(iterador), copia);
    DisposeIterator(iterador);
    return copia;
}

// O(N * M) siendo M la cantidad de elementos de la lista ys, la cual se recorre por iteración, y en cada
// instancia de la misma se realiza la operación Snoc de costo lineal sobre la lista xs y siendo N la cantidad
// de elementos de esta última lista.
// 7. Agrega todos los elementos de la segunda lista al final de los de la primera.
//    La segunda lista se destruye.
//    Nota: notar que el costo mejoraría si Snoc fuese O(1), ¿cómo podría serlo?
/*void Append(LinkedList xs, LinkedList ys){
    ListIterator iterador = getIterator(ys);
    while(!atEnd(iterador)) {
        Snoc(current(iterador), xs);
        Next(iterador);
    }
    Snoc(current(iterador), xs);
    DisposeIterator(iterador);
    DestroyL(ys);
}*/



void printLL(LinkedList xs) {
    if(length(xs) != 0) {
        ListIterator iterador = getIterator(xs);
        while(!atEnd(iterador)){
            cout << current(iterador) << ", ";
            Next(iterador);
        }
        cout << current(iterador) << endl;
        DisposeIterator(iterador);
    } else {
        cout << "No hay elementos en la lista" << endl;
    }
}

int main() {

    LinkedList list = nil();

    Snoc(1,list);
    Snoc(2,list);
    Snoc(3,list);
    Snoc(4,list);

    LinkedList list2 = nil();

    Snoc(5,list2);
    Snoc(6,list2);
    Snoc(7,list2);
    
    //Sucesores(list);

    //cout << "El minimo de la lista es: " << minimo(list) << endl;
    // printLL(list);
    // cout << endl;
    // printLL(copy(list));
    Append(list, list2);
    printLL(list);

}