#include "lexer.h"
#include <stdlib.h>

Token* scan_int(char * c, FILE *file) {
	Token *token = NULL;
	char *init_pos = c;
	int is_float = 0;
	int *number = malloc(sizeof(int));

	while (1) {
		if (char_is_digit(c)) {
			*number = *number * 10 + (*c - '0');
			*c = fgetc(file);
		} else if (*c == '.' || *c == 'e' || *c == 'E') {
			is_float = 1;
			break;	
		} else break;
	}

	if (is_float) {
		// Se for float, volta para o inicio do lexema
		fseek(file, init_pos - c, SEEK_CUR);
		return NULL;
	}

	token = malloc(sizeof(Token));
	token->type = Int;
	token->lexeme = (void*)number;
	return token;	
}

Token* scan_token(FILE *file) {
	char c = fgetc(file);
	Token *token;

	if (char_is_digit(&c)) {
		token = scan_int(&c, file);

		if (token != NULL) {
			token_print_type(Int);
		} else {
			token = scan_float(&c, file);
			token_print_type(Float);
		}
	} else if (char_is_symbol(&c)) {
		token = scan_symbol(&c, file);
		token_print_type(Symbol);
	}

	return token;
}







