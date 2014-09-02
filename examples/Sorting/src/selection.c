// SelectionSort
//# include <stdio.h>
#ifndef ARRAY_SIZE
#define ARRAY_SIZE 64
#endif

int main(){

  unsigned int a[ARRAY_SIZE];

  unsigned int i;
  for (i = 0; i < ARRAY_SIZE - 1; ++i)
    {
      unsigned int j, min, temp;
      min = i;
      for (j = i+1; j < ARRAY_SIZE; ++j)
	{
	  if (a[j] < a[min])
	    min = j;
	}

      temp = a[i];
      a[i] = a[min];
      a[min] = temp;
    }

  //   for (i =0;i<ARRAY_SIZE;i++)
  //   printf ("%d\n", a[i]);

}
