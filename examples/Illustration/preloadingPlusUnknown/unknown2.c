#define	arrsize 64

int main() {
        unsigned long A[arrsize];
        unsigned int secret;
        unsigned int temp;
	unsigned int i;

        for (i = 0; i < arrsize; i++)
                temp = A[i];

        if (secret < arrsize)
	  temp = A[secret];
}
