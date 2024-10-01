#ifndef COMP_LEXER_H
#define COMP_LEXER_H

#include <stdio.h>
#include "token.h"

token_t* scan_token(FILE *);

/* Inner functions */
token_t* scan_whitespace(char *, FILE *);
token_t* scan_symbol(char *, FILE *);
token_t* scan_string(char *, FILE *);
token_t* scan_int(char *, FILE *);
token_t* scan_float(char *, FILE *);
token_t* scan_identifier(char *, FILE *);

#endif
