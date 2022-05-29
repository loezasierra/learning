// Regular expression matching strings of all a or b:
// [ab]+

// Regular expression matching consecutive a and b
// (ab)+a?

// Regular expression matching pit, pot and respite 
//  but not peat, spit, or part
// (^|.{2,})p(i|o)t.*

/* Parse Doge */
#include <stdio.h>
#include <stdlib.h>
#include <editline/readline.h>

#include "../mpc.h"

int main(int argc, char **argv) {
    // Create parsers
    mpc_parser_t *Adjective = mpc_new("adjective");
    mpc_parser_t *Noun      = mpc_new("noun");
    mpc_parser_t *Phrase    = mpc_new("phrase");
    mpc_parser_t *Doge      = mpc_new("doge");
    
    // Define parsers with the following language
    mpca_lang(MPCA_LANG_DEFAULT,
    "                                             \
    adjective : \"wow\" | \"many\" |              \
                \"so\"  | \"such\" ;              \
    noun      : \"lisp\" | \"language\" |         \
                \"book\" | \"build\"    | \"c\" ; \
    phrase    : <adjective> <noun> ;              \
    doge      : /^/ <phrase>* /$/ ;               \
    ",
    Adjective, Noun, Phrase, Doge);

    // Print Title and Exit Information
    puts("Doge");
    puts("Press Ctrl+C to Exit \n");

    // In a never ending loop
    while(1) {

        // Output prompt and get input
        char *input = readline("doge> ");

        // Add input to history
        add_history(input);

        // Attempt to parse user input
        mpc_result_t r;
        if (mpc_parse("<stdin>", input, Doge, &r)) {
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

    // Undefine and delete parsers
    mpc_cleanup(4, Adjective, Noun, Phrase, Doge);

    // Return successfully
    return 0;
}
