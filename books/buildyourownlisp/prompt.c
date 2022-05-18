#include <stdio.h>

// A buffer for user input of size 2048
static char input[2048];

int main(int argc, char** argv) {
    // Print Version and Exit Information
    puts("Lispy Version 0.0.0.0.1");
    puts("Press Ctrl+C to Exit\n");

    // In a never ending loop
    while (1) {

        // Output prompt
        fputs("lispy> ", stdout);

        // Read a line of user input of max. size 2048
        fgets(input, 2048, stdin);

        // Echo input back to user
        printf("No you're a %s", input);
    }

    // Exit successfully
    return 0;
}
