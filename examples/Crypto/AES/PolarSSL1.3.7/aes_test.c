#include "config.h"
#include "aes.h"





int main(){

  aes_context aes;

  // Preloading of tables


  
  


  unsigned char key[8];

  const unsigned char input [16];
  unsigned char output[16];

//   // Homegrown preloading
//   aes_preloading(); 
  

  /* Currently we're using the POLAR_AES_ROM_TABLES option, which means that we're using precomputed tables. Uncommenting this option could help to avoid preloading, but the code does not go through due some "imul" operation that we don't cover.
   */

  aes_setkey_enc( &aes, key, 256 );

  aes_crypt_ecb (&aes, AES_ENCRYPT, input, output);



  return 0;
}




