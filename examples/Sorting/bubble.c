// BubbleSort
//# include <stdio.h>
#ifndef ARRAY_SIZE
#define ARRAY_SIZE 64
#endif

int main(){

  unsigned int a[ARRAY_SIZE];  
  unsigned int i, j, temp;

  for (i = 0; i < (ARRAY_SIZE - 1); ++i){
    for (j = 0; j < ARRAY_SIZE - 1 - i; ++j ){
      if (a[j] > a[j+1]){
	temp = a[j+1];
	a[j+1] = a[j];
	a[j] = temp;
      }
    }
  }
  //  for (i =0;i<ARRAY_SIZE;i++)
  // printf ("%d\n", a[i]);

}

