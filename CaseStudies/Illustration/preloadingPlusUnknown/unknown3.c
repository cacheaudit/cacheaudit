#define	arrsize 64

int main() {
        unsigned long A[arrsize];
	unsigned long B[arrsize];
	unsigned int secret;
        register unsigned int temp asm("ecx");
	register unsigned int i asm("edx");

        for (i = 0; i < arrsize; i++)
        	temp = A[i];

        if (secret < arrsize)
		temp = B[secret];
}
