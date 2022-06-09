#include <iostream>
#include "Pokemon.h"
using namespace std;

Pokemon consPokemon(TipoDePokemon tipo) {
    PokemonSt* p = new PokemonSt;
    (*p).tipo = tipo;
    (*p).vida = 100;
    return p;
}

tipo tipoDePokemon(Pokemon p) {
    return (*p).tipo;
}


int energia(Pokemon p) {
    return (*p).vida;
}

void perderEnergia(int energia, Pokemon p) {
    (*p).vida = energia(p) - energia;
}

bool esDeTipo(Pokemon p, TipoDePokemon tipo) {
    return ((*p).tipo == tipo);
}

bool superaA(Pokemon p1, Pokemon p2) {
    bool casoGanador1 = esDeTipo(p1, 'agua') && esDeTipo(p2, 'fuego');
    bool casoGanador2 = esDeTipo(p1, 'fuego') && esDeTipo(p2, 'planta');
    bool casoGanador3 = esDeTipo(p1, 'planta') && esDeTipo(p2, 'agua');

    return (casoGanador1 || casoGanador2 || casoGanador3);
}

