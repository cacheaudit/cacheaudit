#include "config.h"
#include "aes.h"
#include <string.h>
#include <stdio.h>

#ifndef KEYSIZE
  #define KEYSIZE 256
#endif


int main(){

  aes_context aes;
  
  unsigned char key[(KEYSIZE + 7)/8];

  const unsigned char input [16];
  unsigned char output[16];

#ifdef PRELOADING
  // Homegrown preloading
  aes_preloading();
#endif
  

  /* Currently we're using the POLAR_AES_ROM_TABLES option, which means that we're using precomputed tables. Uncommenting this option could help to avoid preloading, but the code does not go through due some "imul" operation that we don't cover.
   */

  aes_setkey_enc( &aes, key, KEYSIZE );

  aes_crypt_ecb (&aes, AES_ENCRYPT, input, output);



  return 0;
}

