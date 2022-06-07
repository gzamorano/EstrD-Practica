#include <iostream>
#include "Par.h"
using namespace std;

int main() {
    //Par p = consPar(20,5);
    Par dr = divisionYResto(20,5);
    //cout << "Par normal: (" << p.x << ", " << p.y << ")" << endl;
    //cout << "Par cambiado: (" << s.x << ", " << s.y << ")" << endl;

    cout << "Division de 20 5: " << dr.x << endl;
    cout << "Resto de 20 5: " << dr.y << endl;

}