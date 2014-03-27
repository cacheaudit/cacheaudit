#include <stdio.h>
#include <openssl/md5.h>
#include "ecrypt-sync.h"

u8 m[4096];
u8 c[4096];
u8 d[4096];
u8 k[32];
u8 v[8];

main()
{
  MD5_CTX fingerprint;
  ECRYPT_ctx x;
  int i;
  int bytes;
  int loop;

  for (loop = 0;loop < 10;++loop) {
    MD5_Init(&fingerprint);
    for (bytes = 0;bytes <= 4096;++bytes) {
      if (loop & 1)
        ECRYPT_keysetup(&x,k,256,64);
      else
        ECRYPT_keysetup(&x,k,128,64);
      ECRYPT_ivsetup(&x,v);
      ECRYPT_encrypt_bytes(&x,m,c,bytes);
      MD5_Update(&fingerprint,c,bytes);
      ECRYPT_ivsetup(&x,v);
      ECRYPT_decrypt_bytes(&x,c,d,bytes);
      for (i = 0;i < bytes;++i)
        if (d[i] != m[i]) printf("mismatch at position %d/%d\n",i,bytes);
      switch(bytes % 3) {
	case 0: for (i = 0;(i < bytes) && (i < 32);++i) k[i] ^= c[i]; break;
	case 1: for (i = 0;(i < bytes) && (i < 8);++i) v[i] ^= c[i]; break;
	case 2: for (i = 0;i < bytes;++i) m[i] = c[i]; break;
      }
    }
    MD5_Final(k,&fingerprint);
    for (i = 0;i < 32;++i) printf("%02x",k[i]); printf("\n"); fflush(stdout);
  }

  MD5_Init(&fingerprint);
  for (loop = 0;loop < 134217728;++loop) {
    ECRYPT_encrypt_bytes(&x,c,c,4096);
    MD5_Update(&fingerprint,c,4096);
  }
  MD5_Final(k,&fingerprint);
  for (i = 0;i < 16;++i) printf("%02x",k[i]); printf("\n"); fflush(stdout);

  return 0;
}
