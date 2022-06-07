#include <iostream>
using namespace std;


// 1.  Precondición: c1 < c2
// Propósito: Muestra una secuencia de enteros que representan a los caracteres en ASCII,
//            que van desde el caracter c1 hasta el c2.
void printFromTo(char c1, char c2) {
    for(int i = 0; c1 + i <= c2; i++) {
        cout << c1 + i << ", ";
    }
    cout << endl;
}


// 2.  Precondición: n >= 0
// Propósito: Devuelve el factorial de un número entero dado
int fc(int n) {
    int x = 1;
    while(n > 0) {
        x = x * n;
        n--;
    }
    return x;
}

// 3.  Precondición: n <= m
// Propósito: Devuelve el resultado de sumar n con su siguiente hasta m.
int ft(int n, int m) {
    if (n == m) {
       return n;
    }
    return n + ft(n+1, m);
    //     5 + (6 + 7)
}



int main() {
    cout << ft(1,3) << endl;
}