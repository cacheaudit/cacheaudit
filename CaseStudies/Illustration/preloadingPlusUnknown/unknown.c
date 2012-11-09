#define linesize 32
#define numberofsets 64
#define associativity 4

int main() {
	int cachesize = linesize*numberofsets*associativity;
	unsigned int A[cachesize/4];
	unsigned int B[cachesize/4];
	unsigned int secret;
	unsigned int temp;

	for (int i = 0; i < cachesize; i++) 
		temp = A[i];

	temp = B[secret];
}
