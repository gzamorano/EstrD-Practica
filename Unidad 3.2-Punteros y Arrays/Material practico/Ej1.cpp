#include <iostream>
#include "Persona.h"
using namespace std;


int main() {
    Persona g = consPersona("Gonzalo",21);
    Persona r = consPersona("Roberto",30);
    Persona m = laQueEsMayor(r,g);
    // cout << "Persona:[" << g << "] " << "Nombre: " << nombre(g) << " Edad: " << edad(g) << endl;
    // cambioDeNombre("David",g);
     cout << "Persona:[" << m << "] " << "Nombre: " << nombre(m) << " Edad: " << edad(m) << endl;
}