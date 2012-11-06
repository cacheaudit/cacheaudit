
// Copyright in this code is held by Dr B. R. Gladman but free direct or
// derivative use is permitted subject to acknowledgement of its origin.
// Dr B. R. Gladman                               .   25th January 2000.

#ifdef  __cplusplus

#   define  MARS(x) mars::##x

    class mars : public AES
    {
    private:
        u4byte   l_key[40];
    public:

#else

#   define  MARS(x) mars_##x

    STATIC u4byte MARS(l_key[40]);

#endif

    char* MARS(name(void));
    void  MARS(set_key(const u1byte key[], const u4byte key_bits, const enum dir_flag f));
    void  MARS(encrypt(const u1byte in_blk[], u1byte out_blk[]));
    void  MARS(decrypt(const u1byte in_blk[], u1byte out_blk[]));

#ifdef  __cplusplus
    };
#endif
