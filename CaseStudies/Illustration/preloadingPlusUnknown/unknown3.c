#define	arrsize 128

unsigned long volatile A[arrsize];
unsigned long volatile B[arrsize];

int main(unsigned int secret) {
        unsigned int temp;
	unsigned int i;

        for (i = 0; i < arrsize; i++)
        	temp = A[i];

        if (secret < arrsize)
		temp = B[secret];
}
