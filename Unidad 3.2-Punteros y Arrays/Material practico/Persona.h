#include <iostream>
using namespace std;

struct PersonaSt;
   
typedef PersonaSt* Persona;

Persona consPersona(string nombre, int edad); // Crea una persona con nombre y edad dada
// string nombre(Persona p);                     // Devuelve el nombre de una persona
// int edad(Persona p);                          // Devuelve la edad de una persona
// void crecer(Persona p);                       // Aumenta en uno la edad de una persona
// void cambioDeNombre(string nombre, Persona p) // Modifica el nombre de una persona
// bool esMayorQueLaOtra(Persona p1, Persona p2) // Dadas dos personas indica si la primera es mayor que la segunda
// Persona laQueEsMayor(Persona p1, Persona p2)  // Dadas dos personas devuelve a la persona que sea mayor