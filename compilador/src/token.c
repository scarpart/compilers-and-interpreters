#include "token.h"
#include <stdio.h>
#include <stdlib.h>

int is_symbol(const char *const c) {
	if (*c == '(' || *c == ')' || *c == '{' || *c == '}'
			|| *c == '[' || *c == ']') 
		return 1;
	return 0;
}

int is_digit(const char *const c) {
	if (*c >= 30 && *c <= 39) 
		return 1;
	return 0;
}

int is_whitespace(const char *const c) {
	if (*c == '\n' || *c == '\t' || *c == ' ') 
		return 1;
	return 0;
}

void token_print_type(enum token_type_t type) {
	switch (type) {
		case ID: printf("IDENT "); break;
		case INT: printf("NI "); break;
		case FLOAT: printf("NPF "); break;
		default: printf("OUTRO "); break;
	}
}

