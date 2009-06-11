%%[8
#include "../rts.h"

/* Make sure these numbers are the same as generated by Grin/ToSilly.cag */

#define CFalse  1
#define CTrue   2
#define Ccolon  3
#define Csubbus 4
#define CEQ     5
#define CGT     6
#define CLT     7
#define Ccomma0 8

#define CEHC_Prelude_AppendBinaryMode     9
#define CEHC_Prelude_AppendMode          10
#define CEHC_Prelude_ReadBinaryMode      11
#define CEHC_Prelude_ReadMode            12
#define CEHC_Prelude_ReadWriteBinaryMode 13
#define CEHC_Prelude_ReadWriteMode       14
#define CEHC_Prelude_WriteBinaryMode     15
#define CEHC_Prelude_WriteMode           16



%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Integer related primitives
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8

PRIM GrWord packedStringToInteger(GrWord s)
{
	GrWord res;
    res = heapalloc(1);
    ((Pointer)res)[0] = atoi( (char*)s );
    return res;
}


PRIM GrWord primIntToInteger(GrWord n)
{
	GrWord res;
    res = heapalloc(1);
    ((Pointer)res)[0] = n;
    return res;
}

PRIM GrWord primInt32ToInteger(Word32 n)
{
	GrWord res;
    res = heapalloc(1);
    ((Pointer)res)[0] = n;
    return res;
}

PRIM GrWord primIntegerToInt(GrWord p)
{
	GrWord res;
    res = ((Pointer)p)[0];
    return res;
}

PRIM Word32 primIntegerToWord32(GrWord p)
{
	Word32 res;
    res = ((Pointer)p)[0];
    return res;
}


PRIM GrWord primCmpInteger(GrWord x, GrWord y)
{   if (((Pointer)x)[0] > ((Pointer)y)[0])
        return CGT;
    if (((Pointer)x)[0] == ((Pointer)y)[0])
        return CEQ;
    return CLT;
}

PRIM GrWord primEqInteger(GrWord x, GrWord y)
{
    if (((Pointer)x)[0] == ((Pointer)y)[0])
        return CTrue;
    return CFalse;
}

PRIM GrWord primAddInteger(GrWord x, GrWord y)
{   
	GrWord res;
    res = heapalloc(1);
    ((Pointer)res)[0] = ((Pointer)x)[0] + ((Pointer)y)[0];
    return res;
}

PRIM GrWord primSubInteger(GrWord x, GrWord y)
{   
	GrWord res;
    res = heapalloc(1);
    ((Pointer)res)[0] = ((Pointer)x)[0] - ((Pointer)y)[0];
    return res;
}

PRIM GrWord primMulInteger(GrWord x, GrWord y)
{   
	GrWord res;
    res = heapalloc(1);
    ((Pointer)res)[0] = ((Pointer)x)[0] * ((Pointer)y)[0];
    return res;
}

PRIM GrWord primNegInteger(GrWord x)
{   
	GrWord res;
    res = heapalloc(1);
    ((Pointer)res)[0] = -((Pointer)x)[0];
    return res;
}


%%]


