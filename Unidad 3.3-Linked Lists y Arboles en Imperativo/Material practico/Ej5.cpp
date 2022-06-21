#include <iostream>
#include "Queue.h"
using namespace std;

int main() {
    Queue q = emptyQ();

    // Enqueue(10, q);
    // Enqueue(25, q);
    Enqueue(30, q);

    Dequeue(q);

    cout << "Esta vacia q: " << isEmptyQ(q) << endl;
    //cout << "First q: " << firstQ(q) << endl;
    //cout << "Last q: " << lastQ(q) << endl;
}