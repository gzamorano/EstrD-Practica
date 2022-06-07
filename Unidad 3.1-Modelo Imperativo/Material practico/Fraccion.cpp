#include <iostream>
#include "Fraccion.h"
using namespace std;


Fraccion consFraccion(int numerador, int denominador) {
    Fraccion f;
    f.numerador = numerador;
    f.denominador = denominador;
    return f;
}


int numerador(Fraccion f) {
    return f.numerador;
}

int denominador(Fraccion f) {
    return f.denominador;
}


float division(Fraccion f) {
    return f.numerador/f.denominador;
}

Fraccion multF(Fraccion f1, Fraccion f2) {
    Fraccion r;
    r.numerador = f1.numerador * f2.numerador;
    r.denominador = f1.denominador * f2.denominador;
    return r;
}

Fraccion simplificada(Fraccion p) {
    if ((numerador(p)%2) == 0 && (denominador(p)%2) == 0) {
        p.numerador = numerador(p) / 2;
        p.denominador = denominador(p) / 2;
        return simplificada(p);
    } 
    else if (numerador(p)%denominador(p) == 0) {
        p.numerador = division(p);
        p.denominador = 1;
    }
    return p;
}

Fraccion sumF(Fraccion f1, Fraccion f2) {
    Fraccion r;
    if (denominador(f1) == denominador(f2)) {
        r.numerador = numerador(f1)+numerador(f2);
        r.denominador = denominador(f1);
    }
    else if (denominador(f1)%denominador(f2) == 0) {
        r.numerador = numerador(f1)+(numerador(f2)*(denominador(f1)/denominador(f2)));
        r.denominador = denominador(f1);
    } else if (denominador(f2)%denominador(f1) == 0) {
        r.numerador = numerador(f2)+(numerador(f1)*(denominador(f2)/denominador(f1)));
        r.denominador = denominador(f2);
    } else {
        r.numerador = (numerador(f1)*denominador(f2))+(numerador(f2)*denominador(f1));
        r.denominador = denominador(f1)*denominador(f2);
    }
    return r;

}


int main() {
    Fraccion f, r;
    f = sumF(consFraccion(1,4), consFraccion(3,2));
   
    cout << numerador(f) << "/" << denominador(f) << endl;
    //cout << division(f) << endl;
}


