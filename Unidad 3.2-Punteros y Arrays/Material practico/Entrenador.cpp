#include <iostream>
#include "Entrenador.h"
#include "Pokemon.h"
using namespace std;

struct EntrenadorSt {
    string nombre;
    Pokemon* pokemon;
    int cantPokemon;
};

Entrenador consEntrenador(string nombre, int cantidad, Pokemon* pokemon) {
    Entrenador e = new EntrenadorSt;
    e -> nombre = nombre; 
    e -> cantPokemon = cantidad;
    e -> pokemon = pokemon;

    return e;
}

string nombreDeEntrenador(Entrenador e) {
    return (e -> nombre);
}

int cantidadDePokemon(Entrenador e) {
    return (e -> cantPokemon);
}

int cantidadDePokemonDe(TipoDePokemon tipo, Entrenador e) {
    int c = 0;
    for(int i=0; i<cantidadDePokemon(e); i++) {
        if(tipo == (e -> (*pokemon[i]))) {
            c++;
        }
    }
    return c;
}