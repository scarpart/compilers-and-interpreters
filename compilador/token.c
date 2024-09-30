#include "token.h"
#include <stdio.h>

int is_symbol(char *c) {
	if (*c == '(' || *c == ')' || *c == '{' || *c == '}'
			|| *c == '[' || *c == ']') 
		return 1;
	return 0;
}

int is_digit(char *c) {
	if (*c >= 30 && *c <= 39) 
		return 1;
	return 0;
}

int is_whitespace(char *c) {
	if (*c == '\n' || *c == '\t' || *c == ' ') 
		return 1;
	return 0;
}

void token_print_type(enum Token_Type type) {
	switch (type) {
		case Id: printf("IDENT "); break;
		case Int: printf("NI "); break;
		case Float: printf("NPF "); break;
		default: printf("OUTRO "); break;
	}
}

