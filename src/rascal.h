#ifndef RASCAL_H
#define RASCAL_H

#include "alps.h"

union ieee754_double
  {
    double d;
 #ifdef SPARC
    struct   /* This is the IEEE 754 big endian double-precision format.  */
    {
      uint negative:1;
      uint exponent:11;
      uint mantissa0:20;
      uint mantissa1:32;
    } ieee;
#else 
    struct  /* machines with a little end */
    {
      uint mantissa1:32;
      uint mantissa0:20;
      uint exponent:11;
      uint negative:1;
    } ieee;
#endif
};

#define IEEE754_DOUBLE_BIAS	0x3ff /* Added to exponent.  */
#define MAXBNDIGS 34                  /* (+ 2 (c (/ (hex "3FF") 32))) */

typedef unsigned long long ull;

typedef struct bignum {
  uint sign:1;
  uint nwds:7;
  int  wexp:24;
  uint d[MAXBNDIGS];
} bignum;


typedef union ului {
    unsigned long long ul; 
#ifdef SPARC 
  struct {
    uint hi;
    uint lo;
  } ui;
#else
  struct {
    uint lo;
    uint hi;
  } ui;
#endif
} ului;

inline static int nzmsb(unsigned long v) {
  int i;
  for (i=0;i<32 && !(v & 0x80000000); i++) v <<= 1;
  return i;
}

inline static int nzlsb(unsigned long v) {
  int i;
  for (i=0;i<32 && !(v & 1); i++) v >>= 1;
  return i;
}

inline static int numlobits(bignum *m) {
  int i,j=0;
  for (i=0;i<m->nwds;i++) 
    if (m->d[i]) return j+nzlsb(m->d[i]);
    else j += 32;
  return j;
}

inline static int wbits(bignum *m) { 
  return 32*m->nwds - nzmsb(m->d[m->nwds-1]); 
}

inline static int bnzerop(bignum *m) { 
  return ((m->nwds == 1) && (m->d[0] == 0)) ? 1 : 0;
}

#define MTST(M,I) ((M->d[(I) >> 5]) &   (1 << ((I)&0x1f)))
#define MSET(M,I) ((M->d[(I) >> 5]) |=  (1 << ((I)&0x1f)))
#define MCLR(M,I) ((M->d[(I) >> 5]) &= ~(1 << ((I)&0x1f)))


#endif
