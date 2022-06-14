#include <iostream>
#include "Pokemon.h"
#include "Entrenador.h"
using namespace std;

int main() {
    Pokemon charizard = consPokemon("Fuego");
    Pokemon bulbasaur = consPokemon("Planta");
    Pokemon squirtle = consPokemon("Agua");
    Pokemon charmander = consPokemon("Fuego");


    // perderEnergia(35, charizard);

    // if (superaA(charizard, bulbasaur)) {
    //      cout << "Pokemon de tipo: " << tipoDePokemon(charizard) << " con energia: " << energia(charizard) << endl;
    // }

    Pokemon* ashPokemon = new Pokemon[2];
    ashPokemon[0] = charmander;  
    ashPokemon[1] = charizard;
   
    Entrenador ash = consEntrenador("Ash", 2, ashPokemon);

    Pokemon* ivyPokemon = new Pokemon[3];

    ivyPokemon[0] = bulbasaur;
    ivyPokemon[1] = bulbasaur;
    ivyPokemon[2] = bulbasaur;

    Entrenador ivy = consEntrenador("Ivy", 3, ivyPokemon);

    //cout << nombreDeEntrenador(ash) << " tiene " << cantidadDePokemonDe("Fuego", ash) << " pokemon de Fuego" << endl;
    //cout << "El 2do pokemon de " << nombreDeEntrenador(ash) << " es de tipo " << tipoDePokemon(pokemonNro(2,ash)) << endl;
  
    if (leGanaATodos(ash, ivy)) {
        cout << "Ash tiene un poke que le gana a todos los de ivy " << endl;
    } else {
        cout << "Ash no le gana a ivy" << endl;
    }
    
}

