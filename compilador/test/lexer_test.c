#include <signal.h>
#include <stdio.h>
#include <criterion/criterion.h>
#include <criterion/redirect.h>
#include <string.h>
#include "token.h"

Test(skip_whitespace, basic) {
	char * const buffer = "    for ( x = 0; x < 32.5; x++ )";
	//
	//FILE * file = fmemopen(buffer, strlen(buffer), "r");
	FILE *file = tmpfile();
	fwrite(buffer, sizeof(char), strlen(buffer), file);
	rewind(file);  

	//token_t *t = scan_token(file);
	//cr_assert_neq(t, NULL);
	//cr_assert_eq(t->type, ID);
}

