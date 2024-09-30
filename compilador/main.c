#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "token.h"

int main(int argc, char** argv) {
	int opt;
	char* filename = NULL;

	while ((opt = getopt(argc, argv, "f:"))) {
		switch (opt) {
			case 'f':
				filename = optarg;
				break;
			default:
				printf("Nenhum arquivo de código fonte foi providenciado. Utilize a opção -h para informações de utilização.");
				return 1;
		}
	}

	FILE *file = fopen(filename, "r");
	scan_token(file);
}
