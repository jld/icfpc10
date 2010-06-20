#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#define MATSIZE 4

typedef int64_t matrix_t[MATSIZE][MATSIZE];

static void matmult(matrix_t a, matrix_t b, matrix_t c)
{
	int i, j, k;
	int64_t sum;

	for (i = 0; i < MATSIZE; ++i)
		for (k = 0; k < MATSIZE; ++k) {
			sum = 0;
			for (j = 0; j < MATSIZE; ++j)
				sum += a[i][j] * b[j][k];
			c[i][k] = sum;
		}
}

static void matmultn(matrix_t *as, int n, matrix_t b)
{
	int i;
	matrix_t c;
	
	if (!n) {
		memset(b, 0, sizeof(b));
		for (i = 0; i < MATSIZE; ++i)
			b[i][i] = 1;
		return;
	}
	memcpy(b, as[0], sizeof(b));

	for (i = 1; i < n; i += 2) {
		matmult(as[i], b, c);
		if (i + 1 < n)
			matmult(as[i + 1], c, b);
		else
			memcpy(b, c, sizeof(b));
	}
}

