// Ejercicio 4
#include <iostream>
using namespace std;

struct NodoS;

struct SetSt;

typedef SetSt* Set;

// Crea un conjunto vacio
Set emptyS();
// Inidica si el conjunto está vacío
bool isEmptyS(Set s);
// Indicia si el elemento pertenece al conjunto
bool belongsS(int x, Set s);
// Agrega un elemento al conjunto
void AddS(int x, Set s);
// Quita un elemento del conjunto
void RemoveS(int x, Set s);
// Devuelve la cantidad de elementos
int sizeS(Set s);
// Devuelve una lista con los elementos del conjunto
LinkedList setToList(Set s);
// Libera la memoria ocupada por el conjunto
void DestroyS(Set s);

