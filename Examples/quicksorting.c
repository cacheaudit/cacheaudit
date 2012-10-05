//  quickSort
//
//  This public-domain C implementation by Darel Rex Finley.
//
//  * Returns YES if sort was successful, or NO if the nested
//    pivots went too deep, in which case your array will have
//    been re-ordered, but probably not sorted correctly.
//
//  * This function assumes it is called with valid parameters.
//
//  * Example calls:
//    quickSort(&myArray[0],5); // sorts elements 0, 1, 2, 3, and 4
//    quickSort(&myArray[3],5); // sorts elements 3, 4, 5, 6, and 7




int main()
{
  #define NUM_ELEMENTS 20
  unsigned int elements = NUM_ELEMENTS;
  unsigned int arr[NUM_ELEMENTS];// = {7, 11, 4, 13, 8, 18, 12, 14, 2, 9, 19, 6, 17, 16, 10, 1, 0, 3, 15, 5};
  
  
// void quickSort(int *arr, int elements) {

  #define  MAX_LEVELS  300

  unsigned int  piv, beg[MAX_LEVELS], end[MAX_LEVELS], i=0, L, R, swap ;

  beg[0]=0; end[0]=elements;
  while (i>=0) {
    L=beg[i]; R=end[i]-1;
    if (L<R) {
      piv=arr[L];
      while (L<R) {
        while (arr[R]>=piv && L<R) 
	  R--; 
	if (L<R) 
	  arr[L++]=arr[R];
        while (arr[L]<=piv && L<R) 
	  L++; 
	if (L<R) 
	  arr[R--]=arr[L]; 
      }
      arr[L]=piv; 
      beg[i+1]=L+1; 
      end[i+1]=end[i]; 
      end[i++]=L;
      if (end[i]-beg[i]>end[i-1]-beg[i-1]) {
        swap=beg[i]; beg[i]=beg[i-1]; beg[i-1]=swap;
        swap=end[i]; end[i]=end[i-1]; end[i-1]=swap; 
      }
    }
    else {
      if (i == 0) break;
      else i--; 
    }
  }
  return 0;
}