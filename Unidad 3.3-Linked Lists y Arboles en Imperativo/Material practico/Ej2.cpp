#include <iostream>
#include "LinkedList.h"
using namespace std;

// Crear las funciones utilizando la interfaz de LinkedList, indicar costos.

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

// 2. Incrementa en uno todos los elementos. ARREGLAR Ver que onda SetCurrent
void Sucesores(LinkedList xs) {
    if (length(xs) != 0) {
        ListIterator iterador = getIterator(xs);
        while(!atEnd(iterador)){
            SetCurrent(current(iterador)+1, iterador);
            Next(iterador);
        }
        DisposeIterator(iterador);
    }
}

// 3. Indica si el elemento pertenece a la lista.
//bool pertenece(int x, LinkedList xs)

// 4. Indica la cantidad de elementos iguales a x. 
//int apariciones(int x, LinkedList xs)

// 5. Devuelve el elemento más chico de la lista.
//int minimo(LinkedList xs)

// 6.  Dada una lista genera otra con los mismos elementos, en el mismo orden.
//     Nota: notar que el costo mejoraría si Snoc fuese O(1), ¿cómo podría serlo?
//LinkedList copy(LinkedList xs)

// 7. Agrega todos los elementos de la segunda lista al final de los de la primera.
//    La segunda lista se destruye.
//    Nota: notar que el costo mejoraría si Snoc fuese O(1), ¿cómo podría serlo?
//void Append(LinkedList xs, LinkedList ys)

void printLL(LinkedList xs) {
    int i = 0;
    if(length(xs) != 0) {
        ListIterator iterador = getIterator(xs);
        while(!atEnd(iterador)){
            cout << "LL[" << i << "]: " << current(iterador) << endl;
            i++;
            Next(iterador);
        }
        cout << "LL[" << i << "]: " << current(iterador) << endl;
        DisposeIterator(iterador);
    }
}

int main() {

    LinkedList list = nil();

    Snoc(1,list);
    Snoc(2,list);
    Snoc(3,list);
    
    Sucesores(list);

    // cout << "La suma de todos los elementos es: " << sumatoria(list) << endl;
    printLL(list);
}