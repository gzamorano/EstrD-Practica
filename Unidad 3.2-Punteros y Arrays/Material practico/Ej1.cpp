#include <iostream>
#include "Persona.h"
using namespace std;


int main() {
    Persona r = consPersona("Gonza",21);

    cout << "Nombre: " << r -> nombre << " Edad: " << r -> edad << endl;
}