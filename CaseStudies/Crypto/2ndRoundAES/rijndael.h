
// Copyright in this code is held by Dr B. R. Gladman but free direct or
// derivative use is permitted subject to acknowledgement of its origin.
// Dr B. R. Gladman                               .   25th January 2000.

#ifdef  __cplusplus

#   define  RIJNDAEL(x) rijndael::##x

    class rijndael : public AES
    {
    private:
        u4byte      k_len;
        u4byte      e_key[64];
        u4byte      d_key[64];
    public:
#else

#   define  RIJNDAEL(x) rijndael_##x

    STATIC u4byte  RIJNDAEL(k_len);
    STATIC u4byte  RIJNDAEL(e_key)[64];
    STATIC u4byte  RIJNDAEL(d_key)[64];

#endif

    char*   RIJNDAEL(name(void));
    void    RIJNDAEL(set_key(const u1byte key[], const u4byte key_len, const enum dir_flag f));
    void    RIJNDAEL(encrypt(const u1byte in_blk[], u1byte out_blk[]));
    void    RIJNDAEL(decrypt(const u1byte in_blk[], u1byte out_blk[]));

#ifdef  __cplusplus
    };
#endif
