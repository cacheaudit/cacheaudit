#include "config.h"
#include "aes.h"





int main(){

  aes_context aes;

  unsigned char key[8];

  const unsigned char input [16];
  unsigned char output[16];


  aes_setkey_enc( &aes, key, 256 );

  aes_crypt_ecb (&aes, AES_ENCRYPT, input, output);



  return 0;
}




