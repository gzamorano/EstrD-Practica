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
        if(tipo == tipoDePokemon(e -> pokemon[i])) {
            c++;
        }
    }
    return c;
}

Pokemon pokemonNro(int i, Entrenador e) {
    return (e -> pokemon[i-1]);
} 


bool leGanaATodos(Entrenador e1, Entrenador e2) {
    bool b = true;
    for(int i=0; i<cantidadDePokemon(e1); i++) {
        for(int j=0; j<cantidadDePokemon(e2); j++) {
            b &= superaA (e1->pokemon[i], e2->pokemon[j]);
        }
        if(b){
            return b;
        }
    }
    return b;
}