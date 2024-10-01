#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <ctype.h>
#include "lexer.h"

extern char *optarg;

int main(int argc, char** argv) {
	int opt;
	char* filename = NULL;

	while ((opt = getopt(argc, argv, ":f"))) {
		switch (opt) {
			case 'f':
				filename = optarg;
				break;
			default:
				printf("Nenhum arquivo de código fonte foi providenciado. Utilize a opção -h para informações de utilização.\n");
				return 1;
		}
	}

	FILE *file = fopen(filename, "r");
	token_t *token = scan_token(file);

	printf("lexeme = %s\n", (char*)token->lexeme);

	return 0;
}
