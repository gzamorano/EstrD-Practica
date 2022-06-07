#include <iostream>
using namespace std;

struct Par {
    int x;
    int y;
};


Par consPar(int x, int y);         // construye un par
int fst(Par p);                    // devuelve la primera componente
int snd(Par p);                    // devuelve la segunda componente
int maxDelPar(Par p);              // devuelve la mayor componente
Par swap(Par p);                   // devuelve un par con las componentes intercambiadas
Par divisionYResto(int n, int m);  // devuelve un par donde la primer componente
                                   // es la división y la segunda el resto entre ambos números