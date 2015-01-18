#include <stdio.h>
#include <stdlib.h>

#define CHUNKSIZE 1024

char* read_stream(FILE* stream)
{
	char* buf = NULL;
	size_t bufsize = 0;

	do {
		buf = realloc(buf, bufsize + CHUNKSIZE);
		size_t len = fread(buf+bufsize, 1, CHUNKSIZE - 1, stream);
		bufsize += len;
	} while (!feof(stream));
	buf[bufsize] = '\0';

	return buf;
}

int main(int argc, char* argv[])
{
	char* expr = read_stream(stdin);
	printf("\nREAD:\n%s\n", expr);
	
	return 0;
}

