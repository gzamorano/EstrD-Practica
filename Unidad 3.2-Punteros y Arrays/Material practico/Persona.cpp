#include <iostream>
#include "Persona.h"
using namespace std;

struct PersonaSt {
    string nombre;
    int    edad;
};

Persona consPersona(string nombre, int edad) {
    Persona p = new PersonaSt;

    (*p).nombre = nombre;
    (*p).edad = edad;

    return p;
}

