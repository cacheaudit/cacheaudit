#define linesize 32
#define numberofsets 14  //that are to be accessed (there might be more cache sets)
#define associativity 4
#define intsize 4

int main() {
	unsigned int A[1024];
	unsigned int secret;
	unsigned int temp;
	unsigned int i;
	unsigned int stepsize = linesize / intsize;

	if (secret) {
		for (i = 0; i < stepsize * numberofsets; i = i+stepsize) 
			temp = A[i];
	}
}
