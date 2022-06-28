#include <iostream>
#include <iomanip>
using namespace std;

#include "../Material Teorico/Clase11/Tree.h"
#include "../Material Teorico/Clase11/ListOfTrees.h"

// Definir las mismas funciones que el Ej7, pero utilizando BFS(recorrido iterativo a lo ancho),
// para heighT, leaves y levelN usar Queue de Tree.

void AgregarTSiNoEmptyT(TList ts, Tree t) {
  if (!isEmptyT(t)) {
    SnocTL(ts, t);
  }
}

// 1. Dado un árbol binario de enteros devuelve la suma entre sus elementos. 
int sumarT(Tree t) {
    int totalVisto = 0;
    Tree actual;
    TList faltanProcesar = emptyTL(); // nunca habrá emptyT en la lista
    AgregarTSiNoEmptyT(faltanProcesar, t);
    while(!isEmptyTList(faltanProcesar)) {
        actual = headTL(faltanProcesar); // actual NO es empty
        TailTL(faltanProcesar);
        totalVisto += actual->value;
        AgregarTSiNoEmptyT(faltanProcesar, actual->left);
        AgregarTSiNoEmptyT(faltanProcesar, actual->right);
    }
    LiberarTL(faltanProcesar);
    return totalVisto;
}




// 2. Dado un árbol binario devuelve su cantidad de elementos, es decir, el tamaño del árbol (size en inglés).
// int sizeT(Tree t) {

// }

// 3. Dados un elemento y un árbol binario devuelve True si existe un elemento igual a ese en el árbol.
// bool perteneceT(int e, Tree t) {

// }

// 4. Dados un elemento e y un árbol binario devuelve la cantidad de elementos del árbol que son
// iguales a e.
// int aparicionesT(int e, Tree t) {

// }

// 5. Dado un árbol devuelve su altura.
// int heightT(Tree t) {

// }

// 6. Dado un árbol devuelve una lista con todos sus elementos. 
// ArrayList toList(Tree t) {

// }


// 7. Dado un árbol devuelve los elementos que se encuentran en sus hojas.
// ArrayList leaves(Tree t) {
    
// }

//8. Dados un número n y un árbol devuelve una lista con los nodos de nivel n.
// ArrayList levelN(int n, Tree t) {
   
// }


int main() {
    Tree left = nodeT(2, nodeT(8, NULL, NULL), nodeT(8, NULL, NULL));
    Tree right = nodeT(5, nodeT(8, NULL, NULL), nodeT(7, nodeT(8, NULL, NULL), NULL));
    Tree t = nodeT(1, left, right);

    cout << "La suma de los elementos del arbol es: " << sumarT(t) << endl;
}