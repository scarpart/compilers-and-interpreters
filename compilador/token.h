enum Token_Type {
	Id,
	Float,
	Int,
	String,
	Operator,
	ComparisonOperator,
	Symbol
};

enum Symbol {
	OpenParen,
	CloseParen,
	OpenBracket,
	CloseBracket,
	OpenSquareBracket,
	CloseSquareBracket,
};

enum Operator {
	Plus,
	Minus,
	Div,
	Mult,
	Mod,
};

enum ComparisonOperator {
	Lt, Lteq,
	Gt, Gteq,
	Eq, Neq,
};

typedef struct {
	enum Token_Type type;
	void *lexeme;
} Token;

int char_is_symbol(char *);
int char_is_digit(char *);
int char_is_whitespace(char *);

/* Helper functions */
void token_print_type(enum Token_Type); 


