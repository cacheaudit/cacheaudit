// InsertionSort
//# include <stdio.h>

#ifndef ARRAY_SIZE
#define ARRAY_SIZE 8
#endif

int main(){

  unsigned int a[ARRAY_SIZE];
  unsigned int i, j, index;

  for (i = 1; i < ARRAY_SIZE; ++i)
    {
      index = a[i];
      for (j = i; j > 0 && a[j-1] > index; j--)
	a[j] = a[j-1];
      
      a[j] = index;
    }

  //for (i =0;i<ARRAY_SIZE;i++)
  //   printf ("%d\n", a[i]);

}

