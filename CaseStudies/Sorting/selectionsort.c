// SelectionSort
//# include <stdio.h>

int main(){

  unsigned int a[128];
  unsigned int array_size=128;  

  int i;
  for (i = 0; i < array_size - 1; ++i)
    {
      int j, min, temp;
      min = i;
      for (j = i+1; j < array_size; ++j)
	{
	  if (a[j] < a[min])
	    min = j;
	}

      temp = a[i];
      a[i] = a[min];
      a[min] = temp;
    }

  //   for (i =0;i<array_size;i++)
  //   printf ("%d\n", a[i]);

}