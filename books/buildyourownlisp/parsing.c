#include <stdio.h>
#include <stdlib.h>

#include "mpc.h"

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
    // Create parsers
    mpc_parser_t *Number   = mpc_new("number");
    mpc_parser_t *Operator = mpc_new("operator");
    mpc_parser_t *Expr     = mpc_new("expr");
    mpc_parser_t *Lispy    = mpc_new("lispy");

    // Define the parsers with the following Language
    mpca_lang(MPCA_LANG_DEFAULT, 
    "                                                  \
    number   : /-?([0-9]+\\.[0-9]+|[0-9]+)/ ;          \
    operator : '+' | '-' | '*' | '/' | '%' |           \
               \"add\" | \"sub\" | \"mul\" | \"div\" ; \
    expr     : <number> | '(' <operator> <expr>+ ')' ; \
    lispy    : /^/ <operator> <expr>+ /$/ ;            \
    ",
    Number, Operator, Expr, Lispy);

    // Print Version and Exit Information
    puts("Lispy Version 0.0.0.0.5");
    puts("Press Ctrl+C to Exit\n");

    // In a never ending loop
    while (1) {

        // Output prompt and get input
        char *input = readline(">> ");

        // Add input to history
        add_history(input);

        // Attempt to parse user input
        mpc_result_t r;
        if (mpc_parse("<stdin>", input, Lispy, &r)) {
            // On success print the AST
            mpc_ast_print(r.output);
            mpc_ast_delete(r.output);
        } else {
            // Otherwise print the error
            mpc_err_print(r.error);
            mpc_err_delete(r.error);
        }

        // Free input
        free(input);
    }

    // Undefine and delete our Parsers
    mpc_cleanup(4, Number, Operator, Expr, Lispy);

    // Exit successfully
    return 0;
}
