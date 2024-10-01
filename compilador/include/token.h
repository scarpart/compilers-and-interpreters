#ifndef COMP_TOKEN_H
#define COMP_TOKEN_H

enum token_type_t {
	ID,
	FLOAT,
	INT,
	STRING,
	OPERATOR,
	COMPARISON_OPERATOR,
	SYMBOL
};

enum symbol_t {
	OPEN_PAREN,
	CLOSE_PAREN,
	OPEN_BRACKET,
	CLOSE_BRACKET,
	OPEN_SQUARE_BRACKET,
	CLOSE_SQUARE_BRACKET,
};

enum operator_t {
	PLUS,
	MINUS,
	DIV,
	MULT,
	MOD,
};

enum comparison_operator_t {
	LT, LTEQ,
	GT, GTEQ,
	EQ, NEQ,
};

typedef struct {
	const enum token_type_t type;
	void * const lexeme;
} token_t;

int char_is_symbol(const char * const);
int char_is_digit(const char * const);
int char_is_whitespace(const char * const);

/* Helper functions */
void token_print_type(enum token_type_t); 

#endif
