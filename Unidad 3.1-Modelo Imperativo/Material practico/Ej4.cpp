 #include <iostream>
using namespace std;

// 1. Propósito: imprime n veces un string s.

// Iterativa
// Precondición: n >= 0
void printN(int n, string s) {
    for (int i = 0; i < n; i++) {
        cout << s << " ";
    }
    cout << endl;
}

// Recursiva
void printNR(int n, string s) {
    if (n == 1) {
        cout << s << endl;
    } else {
        cout << s << " "; printNR(n-1,s);
    }
    
}


// 2. Propósito: imprime los números desde n hasta 0, separados por saltos de línea.

//Iterativa
void cuentaRegresiva(int n) {
    for(int i = n; i >= 0; i--) {
        cout << i << endl;
    }
}

//Recursiva
void cuentaRegresivaR(int n) {
    if (n == 0) {
        cout << n << endl;
    } 
    else {
        cout << n << endl; 
        cuentaRegresivaR(n-1);
    }
     
}


// 3. Propósito: imprime los números de 0 hasta n, separados por saltos de línea.

// Iterativa
void desdeCeroHastaN(int n) {
    for(int i = 0; i <= n; i++) {
        cout << i << endl;
    }
}



// Recursiva
void desdeCeroHastaNR(int n) {
    /*if(n == 0) {
        cout << 0 << endl;
    }
    else {
        cout << n-n+1 << endl;
        desdeCeroHastaNR(n-1);
    }*/
}


// 4. Propósito: realiza la multiplicación entre dos números (sin utilizar la operación * de C++). 

// Iterativa
int mult(int n, int m) {
    int r = 0;
    for(int i = 1; i <= m; i++) {
        r += n;
    }
    return r;
}

// Recursiva
int multR(int n, int m) {
    if (m == 1) {
        return n;
    }
    return n + multR(n, m-1);
}



// 5. Propósito: imprime los primeros n char del string s, separados por un salto de línea.
//    Precondición: el string tiene al menos n char.

// Iterativa
void primerosN(int n, string s) {
    for(int i = 0; i < n; i++) {
        cout << s[i] << endl;
    }
}

// Recursiva
void primerosNR(int n, string s) {
    if (n == 0) {
        cout << s[n] << endl;
    } 
    else {
        cout << s[n] << endl;
        primerosNR(n-1, s);
    }
}


// 6. Propósito: indica si un char c aparece en el string s.

// Propósito: devuelve la longitud del string dado
int longitud(string s) {
    int i = 0;
    while(s[i] != 0) { // 0 es sinonimo de NULL que se representa con todos los bits en cero(bajo nivel) 
        i++;           // en la celda en memoria, y señala el fin de un string.
    }
    return i;
}


// Iterativa
bool pertenece(char c, string s) {
    bool b = 0;
    for(int i = 0; i < longitud(s); i++) {
        if (c == s[i]) {
            b = !b;
        }
    }
    return b;
}


// Recursiva
//bool pertenece(char c, string s) {
 
//}


// 7. Propósito: devuelve la cantidad de apariciones de un char c en el string s.

// Iterativa
int apariciones(char c, string s) {
    int i = 0; int n = 0;
    while (s[i] != 0) {
        if (c == s[i]) {
            n++;
        }
        i++;
    }
    return n;
}


// Recursiva
//int aparicionesR(char c, string s) {
  
//}





int main () { 
    primerosNR(2,"timeline");
}