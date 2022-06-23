#include <iostream>
#include "Queue.h"
using namespace std;

void printQ(Queue q) {
    while(!isEmptyQ(q)) {
        if(lengthQ(q) == 1) {
            cout << firstQ(q) << endl;
        } else {
            cout << firstQ(q) << ", ";
        }
        Dequeue(q);
    }
}

int main() {
    Queue q = emptyQ();
    Queue q1 = emptyQ();

    Enqueue(10, q);
    Enqueue(25, q);
    Enqueue(30, q);

    Enqueue(11, q1);
    Enqueue(26, q1);
    // Enqueue(31, q1);

    MergeQ(q, q1);

   printQ(q);


    //cout << "Esta vacia q: " << isEmptyQ(q) << endl;
    //cout << "First q: " << firstQ(q) << endl;
}