#define mincachesize 4096
#define	arrsize mincachesize/4

int main() {
        unsigned int A[arrsize];
        unsigned int secret;
        unsigned int temp;
	unsigned int i;

        for (i = 0; i < arrsize; i++)
                temp = A[i];

        if (secret < arrsize)
	  temp = A[secret];
}
