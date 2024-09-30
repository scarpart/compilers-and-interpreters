#include "token.h"
#include <stdio.h>

Token* scan_token(FILE *);

/* Inner functions */
Token* scan_whitespace(char *, FILE *);
Token* scan_symbol(char *, FILE *);
Token* scan_string(char *, FILE *);
Token* scan_int(char *, FILE *);
Token* scan_float(char *, FILE *);
Token* scan_identifier(char *, FILE *);

