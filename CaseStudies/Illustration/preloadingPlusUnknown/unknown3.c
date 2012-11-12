#define	arrsize 64

int main() {
        unsigned long A[arrsize];
	register unsigned int secret asm ("ebx");
        register unsigned int temp asm("ecx");
	register unsigned int i asm("edx");

        for (i = 0; i < arrsize; i++)
                temp = A[i];

        if (secret < arrsize)
	  temp = A[secret];
}
