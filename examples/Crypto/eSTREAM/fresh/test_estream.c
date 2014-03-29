// #include "Salsa20/ecrypt-sync.h"
// #include "HC128/ecrypt-sync.h"
// #include "Rabbit/code/ecrypt-sync.h"
// #include "Sosemanuk/ecrypt-sync.h"


#include ECRLIB

int main()
{
	ECRYPT_ctx mycontext;
	ECRYPT_ctx* ctx = &mycontext; 
	const u32 keysize = ECRYPT_KEYSIZE(0);
	const u8 key[ECRYPT_KEYSIZE(0)];
	const u32 ivsize = ECRYPT_IVSIZE(0);
	const u8 iv[ECRYPT_IVSIZE(0)];
	
#define MSGLEN 128
	u32 msglen = MSGLEN;
	const u8 plaintext[MSGLEN];
	u8 ciphertext[MSGLEN];
	
	ECRYPT_init();
	
	ECRYPT_keysetup(ctx, key, keysize, ivsize);
	
	ECRYPT_ivsetup(ctx, iv);
	
	ECRYPT_encrypt_bytes(ctx, plaintext, ciphertext, msglen);
	
	return 0;
}
