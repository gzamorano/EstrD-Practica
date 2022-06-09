#include <iostream>
#include "Pokemon.h"
using namespace std;

struct PokeSt {
    TipoDePokemon tipo;
    int vida;
};

Pokemon consPokemon(TipoDePokemon tipo) {
    Pokemon p = new PokeSt;
    (*p).tipo = tipo;
    (*p).vida = 100;
    return p;
}

TipoDePokemon tipoDePokemon(Pokemon p) {
    return (*p).tipo;
}

int energia(Pokemon p) {
    return (*p).vida;
}

void perderEnergia(int energiaAPerder, Pokemon p) {
    (*p).vida -= energiaAPerder;
}

// Esta es una función auxiliar que no estaría incluida en la interfaz del TAD, por ende
// no la voy a poder usar fuera de este archivo.
bool esDeTipo(Pokemon p, TipoDePokemon tipo) {
    return ((*p).tipo == tipo);
}

bool superaA(Pokemon p1, Pokemon p2) {
    bool casoGanador1 = esDeTipo(p1, "Agua") && esDeTipo(p2, "Fuego");
    bool casoGanador2 = esDeTipo(p1, "Fuego") && esDeTipo(p2, "Planta");
    bool casoGanador3 = esDeTipo(p1, "Planta") && esDeTipo(p2, "Agua");

    return (casoGanador1 || casoGanador2 || casoGanador3);
}

