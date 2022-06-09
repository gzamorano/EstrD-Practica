#include <iostream>
#include "Pokemon.h"
#include "Entrenador.h"
using namespace std;

int main() {
    Pokemon charizard = consPokemon("Fuego");
    Pokemon bulbasaur = consPokemon("Planta");

    // perderEnergia(35, charizard);

    // if (superaA(charizard, bulbasaur)) {
    //      cout << "Pokemon de tipo: " << tipoDePokemon(charizard) << " con energia: " << energia(charizard) << endl;
    // }

    Pokemon* ashPokemon = new Pokemon[2];  
   
    Entrenador ash = consEntrenador("Ash", 2, ashPokemon);

    //cout << "Entrenador: " << nombreDeEntrenador(ash) << endl;
    //cout << "con pokemon: " << for(int i=0; i<cantidadDePokemonDe)

    cout << "Cantidad de pokemon: " << cantidadDePokemon(ash) << endl;
    

}

