#include <stdio.h>
#include <stdlib.h>

/* If compiling on Windows, compile these functions */
#ifdef _WIN32
#include <string.h>

static char buffer[2048];

// Fake readline function
char *readline(char *prompt) {
    fputs(prompt, stdout);
    fgets(buffer, 2048, stdin);
    char *cpy = malloc(strlen(buffer) + 1);
    strcpy(cpy, buffer);
    cpy[strlen(cpy) - 1] = '\0';
    return cpy;
}

// Fake add_history function
void add_history(char *unused) {}


/* Otherwise include editline header */
#else
#include <editline/readline.h>
#endif

int main(int argc, char** argv) {
    // Print Version and Exit Information
    puts("Lispy Version 0.0.0.0.4");
    puts("Press Ctrl+C to Exit\n");

    // In a never ending loop
    while (1) {

        // Output prompt and get input
        char *input = readline(">> ");

        // Add input to history
        add_history(input);

        // Echo input back to user
        printf("Echo: %s\n", input);

        // Free input
        free(input);
    }

    // Exit successfully
    return 0;
}
