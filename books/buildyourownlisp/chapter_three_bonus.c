#include <stdio.h>

void say_hello (int n);

int main (int argc, char **argv) {

    // Print "Hello World!" 5 times using a for loop
    for (int i = 0; i < 5; i++){
        puts("Hello World! 1");
    }

    // Print "Hello World!" 5 times using a while loop
    int i = 0;
    while (i < 5) {
        puts("Hello World! 2");
        i++;
    }

    // Call function say_hello
    say_hello(5);

    // More Data types:
    // short int (2 bytes)
    // long double (16 bytes)
    // Signs:
    // unsigned (only positive)
    // signed (both positive and negative)

    // Conditional Operators:
    // < <=
    // > >=
    // ==
    // !=

    // Mathematical Operators:
    // + - / * %

    // Do-while loop:
    // Do something for an unknown number of timers,
    // but at least once

    // Switch:
    // Conditional branches formed with constants
    // If input matches one of the contant-expressions,
    //  switch hands control to that branch
    // If input does not match any, switch ahnds control
    //  to the "default" branch
    // Switches "fall through" unless using break

    // Break:
    // Exits out of a loop or switch

    // Continue:
    // Used in loops (for, while, do-while)
    // Skips to the end of the loop body, skipping all
    //  code in loop for the current loop iteration

    // Typedef:
    // Declare an alaias for a type
    // Used to replace a possible complex type name

}

// Int -> Terminal Output
// Prints "Hello World!" Int times
// say_hello(2) ->
// "Hello World!"
// "Hello World!"
void say_hello (int n) {
    for (int i = 0; i < n; i++) {
        puts("Hello World!");
    }
}
