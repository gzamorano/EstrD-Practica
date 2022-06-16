#include <iostream>
#include "LinkedList.h"
using namespace std;

// Crear las funciones utilizando la interfaz de LinkedList, indicar costos.

// 1. Devuelve la suma de todos los elementos.
int sumatoria(LinkedList xs)

// 2. Incrementa en uno todos los elementos.
void Sucesores(LinkedList xs)

// 3. Indica si el elemento pertenece a la lista.
bool pertenece(int x, LinkedList xs)

// 4. Indica la cantidad de elementos iguales a x. 
int apariciones(int x, LinkedList xs)

// 5. Devuelve el elemento más chico de la lista.
int minimo(LinkedList xs)

// 6.  Dada una lista genera otra con los mismos elementos, en el mismo orden.
//     Nota: notar que el costo mejoraría si Snoc fuese O(1), ¿cómo podría serlo?
LinkedList copy(LinkedList xs)

// 7. Agrega todos los elementos de la segunda lista al final de los de la primera.
//    La segunda lista se destruye.
//    Nota: notar que el costo mejoraría si Snoc fuese O(1), ¿cómo podría serlo?
void Append(LinkedList xs, LinkedList ys)

