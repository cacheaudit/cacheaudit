#include "ecrypt-sync.h"

#define ROTATE(v,c) (ROTL32(v,c))
#define XOR(v,w) ((v) ^ (w))
#define PLUS(v,w) (U32V((v) + (w)))
#define PLUSONE(v) (PLUS((v),1))

static const char sigma[16] = "expand 32-byte k";
static const char tau[16] = "expand 16-byte k";

int main()
{
	ECRYPT_ctx* ctx; 
	const u32 keysize = ECRYPT_KEYSIZE(0);
	u8 key[ECRYPT_KEYSIZE(0)];
	const u32 ivsize = ECRYPT_IVSIZE(0);
	u8 iv[ECRYPT_IVSIZE(0)];
	
#define MSGLEN 128
	u32 msglen = MSGLEN;
	const u8 plaintext[MSGLEN];
	u8 ciphertext[MSGLEN];
	int i;
// // 	ECRYPT_init();
	
// // // 	ECRYPT_keysetup(ctx, key, keysize, ivsize);
// 	
// 	static const char *constants;
// 	u8 *k = key;
// 	ctx->input[1] = U8TO32_LITTLE(k + 0);
// 	ctx->input[2] = U8TO32_LITTLE(k + 4);
// 	ctx->input[3] = U8TO32_LITTLE(k + 8);
// 	ctx->input[4] = U8TO32_LITTLE(k + 12);
// 	if (keysize == 256) { /* recommended */
// 	k += 16;
// 	constants = sigma;
// 	} else { /* keysize == 128 */
// 	constants = tau;
// 	}
// 	ctx->input[11] = U8TO32_LITTLE(k + 0);
// 	ctx->input[12] = U8TO32_LITTLE(k + 4);
// 	ctx->input[13] = U8TO32_LITTLE(k + 8);
// 	ctx->input[14] = U8TO32_LITTLE(k + 12);
// 	ctx->input[0] = U8TO32_LITTLE(constants + 0);
// 	ctx->input[5] = U8TO32_LITTLE(constants + 4);
// 	ctx->input[10] = U8TO32_LITTLE(constants + 8);
// 	ctx->input[15] = U8TO32_LITTLE(constants + 12);
	
// // // 	ECRYPT_ivsetup(ctx, iv);
// 	
// 	ctx->input[6] = U8TO32_LITTLE(iv + 0);
// 	ctx->input[7] = U8TO32_LITTLE(iv + 4);
// 	ctx->input[8] = 0;
// 	ctx->input[9] = 0;
	
// // 	ECRYPT_encrypt_bytes(ctx, plaintext, ciphertext, msglen);
	
	const u8 *m = plaintext;
	u8 *c = ciphertext;
	
	u8 output[64];
	
	if (!msglen) return;
	for (;;) {
// //		salsa20_wordtobyte(output,x->input);
		u32 x[16];

		for (i = 0;i < 16;++i) x[i] = ctx->input[i];
		for (i = 20;i > 0;i -= 2) {
		x[ 4] = XOR(x[ 4],ROTATE(PLUS(x[ 0],x[12]), 7));
		x[ 8] = XOR(x[ 8],ROTATE(PLUS(x[ 4],x[ 0]), 9));
		x[12] = XOR(x[12],ROTATE(PLUS(x[ 8],x[ 4]),13));
		x[ 0] = XOR(x[ 0],ROTATE(PLUS(x[12],x[ 8]),18));
		x[ 9] = XOR(x[ 9],ROTATE(PLUS(x[ 5],x[ 1]), 7));
		x[13] = XOR(x[13],ROTATE(PLUS(x[ 9],x[ 5]), 9));
		x[ 1] = XOR(x[ 1],ROTATE(PLUS(x[13],x[ 9]),13));
		x[ 5] = XOR(x[ 5],ROTATE(PLUS(x[ 1],x[13]),18));
		x[14] = XOR(x[14],ROTATE(PLUS(x[10],x[ 6]), 7));
		x[ 2] = XOR(x[ 2],ROTATE(PLUS(x[14],x[10]), 9));
		x[ 6] = XOR(x[ 6],ROTATE(PLUS(x[ 2],x[14]),13));
		x[10] = XOR(x[10],ROTATE(PLUS(x[ 6],x[ 2]),18));
		x[ 3] = XOR(x[ 3],ROTATE(PLUS(x[15],x[11]), 7));
		x[ 7] = XOR(x[ 7],ROTATE(PLUS(x[ 3],x[15]), 9));
		x[11] = XOR(x[11],ROTATE(PLUS(x[ 7],x[ 3]),13));
		x[15] = XOR(x[15],ROTATE(PLUS(x[11],x[ 7]),18));
		x[ 1] = XOR(x[ 1],ROTATE(PLUS(x[ 0],x[ 3]), 7));
		x[ 2] = XOR(x[ 2],ROTATE(PLUS(x[ 1],x[ 0]), 9));
		x[ 3] = XOR(x[ 3],ROTATE(PLUS(x[ 2],x[ 1]),13));
		x[ 0] = XOR(x[ 0],ROTATE(PLUS(x[ 3],x[ 2]),18));
		x[ 6] = XOR(x[ 6],ROTATE(PLUS(x[ 5],x[ 4]), 7));
		x[ 7] = XOR(x[ 7],ROTATE(PLUS(x[ 6],x[ 5]), 9));
		x[ 4] = XOR(x[ 4],ROTATE(PLUS(x[ 7],x[ 6]),13));
		x[ 5] = XOR(x[ 5],ROTATE(PLUS(x[ 4],x[ 7]),18));
		x[11] = XOR(x[11],ROTATE(PLUS(x[10],x[ 9]), 7));
		x[ 8] = XOR(x[ 8],ROTATE(PLUS(x[11],x[10]), 9));
		x[ 9] = XOR(x[ 9],ROTATE(PLUS(x[ 8],x[11]),13));
		x[10] = XOR(x[10],ROTATE(PLUS(x[ 9],x[ 8]),18));
		x[12] = XOR(x[12],ROTATE(PLUS(x[15],x[14]), 7));
		x[13] = XOR(x[13],ROTATE(PLUS(x[12],x[15]), 9));
		x[14] = XOR(x[14],ROTATE(PLUS(x[13],x[12]),13));
		x[15] = XOR(x[15],ROTATE(PLUS(x[14],x[13]),18));
		}
		for (i = 0;i < 16;++i) x[i] = PLUS(x[i],ctx->input[i]);
		for (i = 0;i < 16;++i) U32TO8_LITTLE(output + 4 * i,x[i]);
		
		// // END  OF salsa20_wordtobyte
		
	ctx->input[8] = PLUSONE(ctx->input[8]);
	if (!ctx->input[8]) {
		ctx->input[9] = PLUSONE(ctx->input[9]);
		/* stopping at 2^70 bytes per nonce is user's responsibility */
	}
	if (msglen <= 64) {
		for (i = 0;i < msglen;++i) c[i] = m[i] ^ output[i];
		return;
	}
	for (i = 0;i < 64;++i) c[i] = m[i] ^ output[i];
	msglen -= 64;
	c += 64;
	m += 64;
  }
	
	return 0;
}
