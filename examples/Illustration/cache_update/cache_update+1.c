#define size 256
#define sm_size 16
 int main() {
  unsigned int A[size];
  unsigned int secret;
  unsigned int x;

  //Put block 8 and 40 in the cache, such that they are considered during the update
  x = A[8];	//5ffff6a
  x = A[40];	//5ffff6e
  //Move them out of the cache by adding other blocks
  x = A[72];
  x = A[104];
  x = A[136];
  x = A[168];
  //Assure that 8 may be in the cache or not
  if (secret == 8){
    x = A[secret];
  }
  //After accessing 40, 8 can no longer have age 4
  x = A[40];
}
