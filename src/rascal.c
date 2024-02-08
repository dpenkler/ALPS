/*  TODO check rounding in bn2r */
#include "rascal.h"

#define RASCAL_DEBUG 0  

static bignum *cpyBn(bignum *, bignum *);
static uint   idivbn(bignum *, uint);

#if (RASCAL_DEBUG)
#include <stdio.h>
static void pieee(char * s, double d) {
  union ieee754_double u;
  u.d = d;
  printf("%s: %.18g,m1  %08x m0  %05x exp %04x s %x\n", 
	 s,d, u.ieee.mantissa1, u.ieee.mantissa0,
	 u.ieee.exponent, u.ieee.negative);
}

static void pbn(char * s, bignum * m) {
  int i;
  printf("%s: nwds %d wexp %d wbits %d sign %d\n",
	 s,m->nwds,m->wexp,wbits(m),m->sign);
  for (i = m->nwds-1; i>=0; i--) if (m->d[i]) printf("  %2d %08x;",i, m->d[i]);
  printf("\n");
}

static void woosprtbn(bignum *m) {
  int i,j;
  j = ((wbits(m)+3)/4)*4-1;
  for (i=j;i>=0;i--)
    printf("%c%c%d",((i+1)%4)?0:' ',(i==(wbits(m)-1))?'*':0,
	   (MTST((m),i) != 0));
  printf("\n");
}

static bignum *pdbn(char *s,bignum *m) {
  bignum *t,tn;
  char *b,buf[1024];
  int i=0;
  b    = &buf[1023];
  *b   = 0;
  t    = cpyBn(m,&tn);
  while (wbits(t)) *--b = 48 + idivbn(t,10);
  printf("%s: %s*2^%d s:%d\n",s,b,m->wexp,m->sign);
  return m;
}

static void bncheck(bignum *n) {
  if (n->nwds <0 || n->nwds >= MAXBNDIGS)
    printf("bncheck: invalid number of words %d\n",n->nwds);
}

#endif

static inline int lobits(bignum *m, int j) {
  for (;j>=0;j--) if (m->d[j]) return 1;
  return 0;
}

static bignum *mkBigNum(double d, bignum *m) {
  union ieee754_double u;
  uint   exp,wd0,wd1,t0,t1;
  int    i,i0,i1,j,k,n,mexp;
  u.d     = d;
  m->sign = u.ieee.negative;
  if (d==0.0) { m->nwds  = 1; m->d[0]  = 0; m->wexp  = 0; return m; }
  exp =      u.ieee.exponent;
  wd0 = t0 = u.ieee.mantissa0;
  wd1 =      u.ieee.mantissa1;
  if (exp) t0 |= 0x100000; /* put back imaginary leading bit */
  mexp  = exp - IEEE754_DOUBLE_BIAS; /* remove bias */
  if (wd1) j = i0 = nzlsb(wd1);
  else j = 32 + (i1 = nzlsb(t0)); 
  mexp = mexp - 52 + j;
  if (mexp < 0) { m->wexp = mexp; n=k=0; }
  else {  m->wexp = 0; n = mexp / 32;  k = mexp % 32;}

  for (j=0;j<n;j++) m->d[j] = 0UL; /* trailing zeros */

  if ((t1 = wd1)) { /* have bits in ieee lsw */
    i=i0; /* number of consecutive zero lsbs */
    if (i == k)  m->d[n] = t1;
    else
      if (i > k) { /* can fit k trailing zeros just nudge up the difference */
	i -= k;
	m->d[n] = (t1 >> i) | t0 << (32 - i);  /* lsw */
	t0 >>= i; 
      } else { /* no room */
	i = k - i; /* shortfall */
	m->d[n] = t1 << i;
	if (i < 12) { /* have 11 spare bits in t0 */
	  t0 = (t0 << i) | t1 >> (32 - i);
	} else {
	  m->d[++n] = (t0 << i) | t1 >> (32 - i);
	  t0 = t0 >> (32 - i);
	}
    }
    if (t0) m->d[++n] = t0;
  } else { /* zero mantissa 1 */
    i = i1;
    if (i == k)  m->d[n] = t0;
    else 
      if (i > k) m->d[n] = t0 >> (i - k);
      else {
	i = k - i;
	if (i < 12) m->d[n] = t0 << i;
	else {
	  m->d[n++] = t0 << i;
	  m->d[n]   = t0 >> (32 - i);
	}
    }
  }
  m->nwds  = n + 1;
  return m;
}

static bignum *cpyBn(bignum *m1, bignum *m2) {
  int i;
  uint *u1,*u2;
  /* m1 -> m2 */
  u1 = (uint *)m1;
  u2 = (uint *)m2;
  *u2++ = *u1++;
  for (i=0;i<m1->nwds;i++) *u2++=*u1++;
  return m2;
}

static double bn2r(bignum *m) {
  union ieee754_double u;
  uint exp,t,t0,t1,k;
  int i,j,n,mwbits,owbits;
  ului a;

  /* TODO check if sthg has to be done for sign */
  mwbits = wbits(m);
 again:
  if (!(owbits = mwbits)) return 0.0;
  if (mwbits > 53) { /* round ? */
    i = mwbits - 54; /* Next bit after the 53 IEEE double mantissa bits */
    j = i >> 5;      /* j is index of word containing bit i */
    if ((k = (m->d[j] & (1 << (n = (i & 0x1f)))))) { /* yes */
      if ((n && (m->d[j] << (32 - n))) || lobits(m,j - 1)) goto roundup;
      else {  /* we have exactly a half */
	if (!MTST(m,i+1)) goto roundup; /* mantissa part even */
	else goto noround;
      }
    roundup:
      for (i=j;i<m->nwds;i++) { /* round up */
	a.ul = m->d[j] + k;
	m->d[j] = a.ui.lo;
	if (!(k = a.ui.hi)) break;
      }
      if (k)  m->d[m->nwds++] = k;
      //pbn("rounding",m);
      mwbits = wbits(m);
      if (mwbits != owbits) goto again;
    }
  }

 noround:
  j = m->nwds - 1; /*start from msw */
  t = m->d[j];
  t1 = j ? m->d[--j] : 0;
  
  if ((i = (mwbits & 0x1f))) i = 32 - i;
 /* make room for exponent and sign less suppressed leading bit */
  if (i == 11) t0 =  0xfffff & t; /* perfect fit, drop top bit */
  else 
    if (i < 11) { /* not enuff space so jiggle about */
      t0 = 0xfffff & t >> (11 - i); /* chop off top bit */
      t1 = t << (i + 21) | t1 >> (11 - i); /* move in the rest with t1 */
    } else { /* insufficient bits in t so jiggle about some more */
      t0 = (0xfffff & t << (i - 11)) |  t1 >> (32 - i + 11);
      t  = j ? m->d[--j] : 0;
      t1 = t1 << (i - 11) | t >> (32 - i + 11);
    } 
  u.ieee.negative  = m->sign;
  u.ieee.exponent  = IEEE754_DOUBLE_BIAS + mwbits + m->wexp -1;
  u.ieee.mantissa0 = t0;
  u.ieee.mantissa1 = t1;
  return u.d;
}


static bignum *rsbn(bignum *m, int k) { /* right shift k bits */
  /* Note: wexp not changed ! */
  uint u=0;
  int i,j,n,p,q;
  
  if (k<=0) return m;
  j = k >> 5;
  p = k & 0x1f;
  q = m->nwds - 1;
  m->nwds -= j;
  if (p) { 
    for (i=0,n=j;i<m->nwds;i++,n++) {
      u = (n<q) ? m->d[n+1] : 0;
      m->d[i] = u << (32 - p) | m->d[n] >> p;
    }
  } else for (i=0,n=j;i<m->nwds;i++,n++) m->d[i] = m->d[n];
  if (!m->d[m->nwds-1])m->nwds--;
  return m;
}
    
static bignum *lsbn(bignum *m, int k) { /* left shift k bits */
  /* Note: wexp not changed ! */
  int i,j,n;
  uint r=0;
  ului t;
  if (k<=0) return m;
  if ((j = k >> 5)) { /* shift 32 at a time */
    m->nwds += j;
    n = m->nwds - 1;
    for (i=n;i>=j;i--) {m->d[i] = m->d[i-j];}
    for (i=0;i<j;i++) m->d[i] = 0;
    k &= 0x1f;
  }
  for (i=j;i<m->nwds;i++) {
    t.ui.lo = m->d[i];
    t.ui.hi = 0;
    t.ul  <<= k;
    m->d[i] = t.ui.lo | r;
    r       = t.ui.hi;
  }
  if (r) m->d[m->nwds++] = r;
  return m;
}

static bignum * mknorm(double d, bignum *m) {
  union ieee754_double u;
  uint mask=0;
  u.d = d;
  m->sign = u.ieee.negative;
  m->nwds = 1;
  if (u.ieee.exponent) mask = 0x100000;
  if ((m->d[1]  = u.ieee.mantissa0 | mask)) m->nwds++;
  m->d[0]  = u.ieee.mantissa1;
  m->wexp  = u.ieee.exponent - IEEE754_DOUBLE_BIAS - 52;
  return m;
}

static bignum *addBn(bignum *m, int a) {
  uint c = a;
  ului t;
  int i;

  for (i=0;i<m->nwds;i++) {
    t.ul    = m->d[i]  + c;
    c       = t.ui.hi;
    m->d[i] = t.ui.lo;
  }
  if (c)  m->d[m->nwds++] = c;
  return m;
}

static bignum *subBn(bignum *m, uint s) {
  uint borrow = 0;
  int i;
  ull t;
  for (i=0;i<m->nwds && s;i++) {
    t = m->d[i];
    if ((borrow = (t < s))) t += 0x100000000LL;
    m->d[i]   = t - s;
    s = borrow;
  }
  for (i=m->nwds-1;i>0;i--) if (m->d[i]) { m->nwds = i+1; break; }
  return m;
}

double nextfloat(double d) {
  union ieee754_double u;
  u.d = d;
  if (u.ieee.mantissa0 ^ 0xfffff || u.ieee.mantissa1 ^ 0xffffffff) {
    if (!(++u.ieee.mantissa1)) u.ieee.mantissa0++;
  } else {
    u.ieee.exponent++;
    u.ieee.mantissa0 = 0;
    u.ieee.mantissa1 = 0;
  }
    return u.d;
}

double prevfloat(double d) {
  union ieee754_double u;
  u.d = d;
  if (u.ieee.mantissa0 || u.ieee.mantissa1) {
    if (!(--u.ieee.mantissa1^0xffffffff)) u.ieee.mantissa0--;
  } else {
    u.ieee.exponent--;
    u.ieee.mantissa0 = 0xfffff;
    u.ieee.mantissa1 = 0xffffffff;
  }
  return u.d;
}


/* m1 + m2 -> r ignores signs of m1 and m2 */
static bignum *mksum(bignum * m1,bignum * m2,bignum *r) {
  ull addin=0UL;
  bignum *m;
  ului t;
  int i,j,n;

  n = (m1->nwds < m2->nwds) ? m1->nwds : m2->nwds; // size of shorty
  m = (m1->nwds > m2->nwds) ? m1 : m2;             // m is the taller
  r->nwds = m->nwds; // result at least as tall as m
  for (i=0;i<n;i++) { // deal with shorty
    t.ul    = m1->d[i] + m2->d[i] + addin;
    r->d[i] = t.ui.lo;
    addin   = t.ui.hi;
  }
  for (i=n;i<m->nwds;i++) { // cover the rest
    t.ul    = m->d[i] + addin;
    r->d[i] = t.ui.lo;
    addin   = t.ui.hi;
  }
  if (addin)  r->d[r->nwds++] = addin; // grow if needed
  r->wexp  = 0; /* FIXME */
  r->sign  = 0; /* Note: sum does not care about signs */
  return r;
}

/* compare bignums: return -1 m1<m2; 0 m1=m2; 1 m1>m2 */
static int cmpbn(bignum *m1, bignum *m2) {
  int i,j;

  if (m1->sign != m2->sign) return m2->sign - m1->sign;
  if (m1->nwds > m2->nwds)  return m1->sign ? -1 :  1; /* any sign will do */
  if (m1->nwds < m2->nwds)  return m1->sign ?  1 : -1;
  for (i=m1->nwds-1;i>=0;i--) {
    if (m1->d[i] != m2->d[i]) return (m1->d[i] > m2->d[i]) ? 1 : -1;
  }
  return 0;
}

/* r <- m1 - m 2, Assume same signs for m1 and m2 */
static bignum *mkdiff(bignum *m1, bignum *m2, bignum *r) {
  bignum *m,*p;
  uint borrow = 0;
  ull s;
  int c,i,n;
  r->nwds = 1;
  c = cmpbn(m1,m2);
  if (c == 0) { r->sign=r->d[0]=r->wexp=0; return r; }
  if (c >  0) { m = m1; p = m2; r->sign =  m->sign;  }
  else        { m = m2; p = m1; r->sign = !m->sign;  }
  n = p->nwds;
  for (i=0;i<n;i++) { /* m > p */
    s      = m->d[i] - borrow;
    borrow = borrow && (m->d[i] == 0); /* propagate borrow */
    if (s < p->d[i]) { s += 0x100000000UL; borrow = 1; }
    r->d[i]   = s - p->d[i];
  }
  for (i=n;i<m->nwds;i++) {
    r->d[i] = m->d[i] - borrow;
    if (!(borrow = borrow && (m->d[i]==0))) break;  /* propagate the debt */
  }
  for (i=m->nwds-1;i>0;i--) if (r->d[i]) { r->nwds = i+1; break; }
  ASSERT(!borrow);
  r->wexp  = 0;
  return r;
}

/* m1 + m2 -> r */
static bignum *addbns(bignum * m1,bignum * m2,bignum *r) {
  if (m1->sign != m2->sign) {
    if (m1->sign) {
      m1->sign = 0;
      mkdiff(m2,m1,r); /* mkdiff expects same signs */
      m1->sign = 1;
    } else {
      m2->sign = 0;
      mkdiff(m1,m2,r);
      m2->sign = 1;
    }
  } else {
    mksum(m1,m2,r);
    r->sign = m1->sign;
  }
  return r;
}

/* r <- m1 - m 2 */
static bignum *subbns(bignum *m1, bignum *m2, bignum *r) {
  if (m1->sign != m2->sign) { /* delegate to sum */
    r = mksum(m1,m2,r);
    r->sign = m1->sign;
  } else {
    mkdiff(m1,m2,r);
  }
  return r;
}

static bignum *mulbn(bignum *m, uint b) { 
  ull bb = b;
  ull carry = 0UL;
  ului t;
  int i;
  /* m*b -> m */
  for (i=0;i<m->nwds;i++) {
    t.ul    = m->d[i] * bb + carry;
    m->d[i] = t.ui.lo;
    carry   = t.ui.hi;
 }
  if (carry)  m->d[m->nwds++] = carry;
  return m;
}

static bignum *mulbns(bignum *m1, bignum *m2) { 
  ull b,carry;
  bignum *r,rm;
  ului t;
  int i,j;
  /* m1*m2 -> m2 */
  if (bnzerop(m2)) return m2;
  if (bnzerop(m1)) return cpyBn(m1,m2);
  r = &rm;
  r->nwds = m1->nwds + m2->nwds;
  ASSERT(r->nwds<MAXBNDIGS);
  for (i=0;i<r->nwds;i++) r->d[i] = 0;
  for (i=0;i<m1->nwds;i++) {
    b      = m1->d[i];
    carry  = 0UL;
    for (j=0;j<m2->nwds;j++) {
      t.ul      = m2->d[j] * b + carry + r->d[j+i]; 
      r->d[j+i] = t.ui.lo;
      carry     = t.ui.hi;
    }
    if (carry)  r->d[i+j] = carry;
  }
  for (i=r->nwds-1;i>=0;i--) if (r->d[i]) { r->nwds = i+1; break; }
  if ((r->wexp = m1->wexp +  m2->wexp)) {
    r->wexp = wbits(m1) + wbits(m2) + r->wexp - wbits(r);
  }
  r->sign = (m1->sign == m2->sign) ? 0 : 1;
  cpyBn(r,m2);
  return m2;
}

static void maddBn(bignum *m, int p, int a) { /* multiply m by p and add a */
  ull c = a;
  ull q = p;
  ului t;
  int i;

  for (i=0;i<m->nwds;i++) {
    t.ul    = m->d[i] * q + c;
    c       = t.ui.hi;
    m->d[i] = t.ui.lo;
  }
  if (c)  m->d[m->nwds++] = c;
}

static bignum *mulex(bignum *m, int b, int e) {
  ull delta = b;
  uint pwr;
  ului t;
  int i,j,k;
   /* m*b^e -> m*/
  if (!e) return m;
  ASSERT(e>0);
  t.ul=0;
  t.ui.lo = b;
  for (i=0;(i<e) && !t.ui.hi;i++) {pwr = t.ui.lo; t.ul *= delta;}
  j = e / i;
  k = e % i;
  /* res = m*(pwr^j)*(b^k) */
  for (i=0;i<j;i++) mulbn(m,pwr);
  if (k) {
    pwr = 1;
    for (i=0;i<k;i++) pwr *= b; /* b^k */
    mulbn(m,pwr);
  }
  return m;
}

static  uint idivbn(bignum *m, uint d) { /* m = int(m/d); r = m%d */
  ului t;
  lldiv_t qr;
  ull dll = d;
  int i,n = m->nwds - 1;
  if (!d) error(pgmr_inc,"idiv by 0");
  if (bnzerop(m)) return d;
  t.ui.hi = 0;
  if (m->d[n] < d) t.ui.hi = m->d[n--];
  for (i=n;i>=0;i--) { /* start from the top */
    t.ui.lo = m->d[i];
    if (t.ul < dll) {
      if (i) {
	t.ui.hi   = m->d[i];
	t.ui.lo   = m->d[i-1];
	m->d[i--] = 0;
	qr = lldiv(t.ul,dll);
	m->d[i] = qr.quot;
	t.ui.hi = qr.rem;
      } else {
	m->d[0] = 0;
	t.ui.hi = t.ui.lo; /* tui.hi holds remainder for return */
      }
    } else {
      qr = lldiv(t.ul,dll);
      m->d[i] = qr.quot;
      t.ui.hi = qr.rem;
    }
  }
  if (n < 0) {m->nwds = 1; m->d[0] = 0;}
  else m->nwds  = n + 1;
  return t.ui.hi;
}

static int sdiv_cnt = 0;
static int sdivbn(bignum *m1, bignum *m2) { /* only specical case */
  ull b,c,q;
  ului ys,y;
  int i,cnt = 0;
  int n = m1->nwds;
  if (n < m2->nwds) return 0;
  if (m2->nwds > n) return -15; // divisor too large
  q = m1->d[n-1] / (1 + m2->d[n-1]);
  if (q != 0) {
    b=c=0;
    for (i=0;i<n;i++) {
      ys.ul    = (ull)m2->d[i]*q + c;
      c        = ys.ui.hi;
      y.ul     = (ull)m1->d[i] - (ull)ys.ui.lo -  b;
      b        = y.ui.hi & 1;
      m1->d[i] = y.ui.lo;
    }
  }
  /* suppress msb zero words */
  for (i=n-1;i>0;i--) if (m1->d[i]) break;
  m1->nwds  = i+1;
  if (cmpbn(m1,m2) >= 0) {
    b=c=0;
    q++;
    for (i=0;i<m1->nwds;i++) {
      ys.ul    = (ull)m2->d[i] + c;
      c        = ys.ui.hi;
      y.ul     = (ull)m1->d[i] - (ull)ys.ui.lo -  b;
      b        = y.ui.hi & 1;
      m1->d[i] = y.ui.lo;
    }
    for (i=n-1;i>0;i--) if (m1->d[i]) break;
    m1->nwds  = i+1;
  }
  return q;
}

static inline int isExp2_52(bignum *m) {
  return (m->nwds==2 && m->d[0]==0 && m->d[1]==0x100000);
}

static double  findBest(bignum *f, int d, int e, double z) {
  /* This is basically AlgorithmR from Clinger90 */
  bignum *m,mn,*x,xn,*y,yn;
  bignum *D,md,*D2,md2,*DB,mdb;
  double z1;
  int k,r,i=0;

  if (!(f->nwds==1 && e==0) && (!isinf(z))) {
  for (;i<Max_FB_Loop;i++) {
    m = mknorm(z,&mn);
    k = m->wexp;
#if(RASCAL_DEBUG)
    z1 = bn2r(m);
    ASSERT(z1!=z);
    pieee("fbest z",z);
    pieee("fbchk m",z1);
    pbn("fbest m",m);
    pbn("fbest f",f);
    pdbn("fbest m",m);
    pdbn("fbest f",f);
    printf("fbest e %d, delta %d k %d\n",e,d,k);
#endif
    m->wexp = 0; 
    
    if (e >= 0) 
      if (k >= 0) {x = mulex(cpyBn(f,&xn),d,e);  y = lsbn(cpyBn(m,&yn),k);}
      else        {x = lsbn(mulex(cpyBn(f,&xn),d,e),-k); y = cpyBn(m,&yn);}
    else 
      if (k >= 0) {x = cpyBn(f,&xn);     y = lsbn(mulex(cpyBn(m,&yn),d,-e),k);}
      else        {x = lsbn(cpyBn(f,&xn),-k); y = mulex(cpyBn(m,&yn),d,-e);}
#if(RASCAL_DEBUG)
    pdbn("comp x",x);
    pdbn("comp y",y);
    D = subbns(x,y,&md);        /* D  = x-y  */
    pdbn("diff D",D);
    DB = addbns(D,y,&mdb);
    pdbn("sum D y",DB);
    D2 = lsbn(cpyBn(D,&md2),1); /* D2 = 2*D  */
    D2->sign = 0;               /* D2 = |D2| */
    pdbn("|D2|  ",D2);
    mulbns(m,D2);               /* D2 = m*D2 */
    pdbn("m     ",m);
    pdbn("m|D2| ",D2);
    pdbn("y     ",y);
    r = cmpbn(D2,y);
    printf("cmp(D2,y) = %d\n",r);
#else
    D = subbns(x,y,&md);        /* D  = x-y  */
    D2 = lsbn(cpyBn(D,&md2),1); /* D2 = 2*D  */
    D2->sign = 0;               /* D2 = |D2| */
    mulbns(m,D2);               /* D2 = m*D2 */
    r = cmpbn(D2,y);
#endif
    if (!r) { /* (x-y)*2*m == y */
      if (!(m->d[0] & 1)) { /* m even */
	if (D->sign && isExp2_52(m))  z = prevfloat(z);
	else break;
      } else {z = (D->sign) ? prevfloat(z) : nextfloat(z); break; }
    } else if (r < 0) { /* (x-y)*2*m < y */
      if (D->sign && isExp2_52(m) && (cmpbn(lsbn(D2,1),y)>0)) z = prevfloat(z);
      else break;
    } else { /* (x-y)*2*m > y */
      z = (D->sign) ? prevfloat(z):nextfloat(z);
    }
  }
  }
#if (RASCAL_DEBUG)
  pieee("fbest z",z);
  if (i) printf("nloops %d %18g\n",i,z);
#endif
  bumpRasc(i+1);
  return z;
}

/* Real to AScii Conversion ALgorithm implementation based on
   ((FPP)^2) from Steele & White in ACM SIGPLAN 90 "How to print
   floating-point numbers accurately" with some inspiration (use of k)
   from David M. Gay AT&T Bell Labs Numerical Analysis Manuscript
   90-10 "Correctly Rounded Binary-Decimal and Decimal-Binary
   Conversions" November 30,1990. His optimisation using floating
   point arithmetic and use of Taylor series approximation for k were
   not used since we allow for variable output radix.
   Parameters:
   val  -> ieee double to convert to ascii
   base -> output radix (input radix is always 2)
   sd   -> desired number of significant output digits
   prec -> desired number of digits after point
   sgn  <- 0 for positive else neg
   buf  <- buffer to receive ascii output digits 
           max size in bytes >= 55  = (+ 2 (f (/ 53 (l 2 base)))) for base = 2
   bpnt <- logical index in buf of base point, for example
            0   => first digit follows point
	    -10 => first digit in buf follows 10 zeros after point
            10  => point follows first 10 digits in buf
	    Return: number of valid digits in buf                          
   fexp -> calculate significant digits for exponent notation d.xxx */
int rascal(double val, int    base, int  sd,   int prec,
	   int   *sgn, uchar *buf,  int *bpnt, int fexp) { 
  int    i,k,lz,mxsd,ns,q,ir,is,ss,len;
  static const  int p = 53; /* number of bits in ieee double significand */
  union  ieee754_double u;
  uint    exp;
  double l2b;
  bignum mh,ml,r,s,t;
  
  u.d  = val;
  len  = 0;               // number of digits generated so far
  k    = 0;               // tracks scale
  *sgn = u.ieee.negative; // note sign
  u.ieee.negative = 0;    // stay positive from now on
  exp  = u.ieee.exponent; // raw exponent  
  if (exp == 0x7ff) { // We have a whacko. Caller should check for Inf vs NaN
    memcpy(buf, "NaN", len=3); /* no samoosas */
    goto errout;
  }
  if (u.d == 0.0L) { buf[len++] = '0'; goto done; } // zero
  if (!exp) { memcpy(buf,"denorm",len=6);  goto errout; }
  l2b  = log2(base);
  mxsd = 2 + (p + 1) / l2b; // max number of significant digits in output radix
  k = lrint(log2(u.d)/l2b); // approximation of base exponent
  mkBigNum(u.d,&r);
  mkBigNum(1.0L,&s);
  if (r.wexp < 0) lsbn(&s,-r.wexp); /* adjust scale */
  r.wexp = 0; /* convert bignum to integer (r already scaled) */
  if (k >= 0) mulex(&s,base, k);
  else        mulex(&r,base,-k);
  lz = ceil(l2b); // number of bits needed to represent one base digit
  i  = nzmsb(s.d[s.nwds-1]); // number of leading zero bits
  ns =  (i > lz) ? i - lz : 32 - lz + i; // leave lz zero msb's in divisor
  lsbn(&s,ns);
  lsbn(&r,ns);
  if (cmpbn(&r,&s) < 0) { k--; mulbn(&r,base); } /* adjust botched k */
  if (prec < 0) { // no of digits after point  not specified
    if (sd < 1) sd = mxsd; // no of significant digits not specified, use max
  } else { // modify sd for requested precision for correct rounding
    if ((i  =  (fexp ? 0 : k) + prec) >= 0) { // fexp=> 1 digit before point
      sd = i + 1;
    } else { // special cases
      if (i==-1) { // the only ouput digit is determined by rounding
	bool round = false;
	if (k==-1) round = (fabs(val) >= 0.5L); // easy case 
	else { // do it the long way
	  q = sdivbn(&r,&s);
	  lsbn(&r,1);
	  if (cmpbn(&r,&s) >= 0) q++;
	  round = (q*2 >= base);
	}
	if (round) { buf[len++] = '1'; k++; } 
	else       { buf[len++] = '0'; }
      } else  buf[len++] = '0'; // no digits worth mentioning
      goto done; 
    }
  }
  while (true) {
#if (RASCAL_DEBUG)
    bncheck(&r);
    bncheck(&s);
#endif
    q = sdivbn(&r,&s);
#if (RASCAL_DEBUG)
    if (q < 0 || q > sizeof(numdig) || len > 64)
      printf("rascal: invalid q  %d or len %d detected\n",q,len);
#endif
    buf[len++] = numdig[q];
    if (bnzerop(&r)) goto done;
    if (len == sd) break;
    mulbn(&r,base);
  }
  /* Perform final rounding */
  lsbn(&r,1); // R x 2 for comp
  if (cmpbn(&r,&s) >= 0) { // round up on equal
    for (i = len-1;i>=0;i--) if (buf[i] < numdig[base-1]) break;
    if (i<0) { buf[0] = '1'; k++; len = 1; }  // point moves one to the right
    else { buf[i]++; len = i + 1; } // bump last digit
  } else { // trim trailing zeros
    for (i = len-1;i>=0;i--) if (buf[i] != '0') break;
    len = i + 1;
  }
 done:
  *bpnt = k + 1;
  return len;
 errout:
  *bpnt = 1025; /* invalid point */
  return len;
}

#if 0
bool asctor2(idstr buf, integer len, integer radix, double * dres) {
  char buf1[max_line_len];
  char *se;
  memcpy(buf1,buf,len);
  buf1[len]=0;
  *dres = strtod(buf1,&se);
  return true;
}
#endif

bool asctor(idstr buf, integer len, integer radix, integer start,
	    integer bpdigs, integer apdigs, integer expdigs,
	    bool hadneg, integer hadpoint, bool hadeneg, integer hadexp,
	    double * dres) {
  double pwr, mlt, base;
  double res,bres;
  bignum mb,*m;
  integer exponent,i,ind,digs;
  unsigned char c,*s;
  base     = idadd;
  pwr      = idprod;
  exponent = 0;
  digs     = start;
  if (('+' == (c = buf[digs])) || ('-' == c)) digs++; /* skip leading sign */
  while ('0' == (c = buf[digs])) {bpdigs--; digs++;} /* suppress leading zeros*/
  s = &buf[hadpoint + apdigs - 1];
  while ('0' == *s) {s--; apdigs--;} /* suppress trailing zeros */
#if (RASCAL_DEBUG)
  char mbuf[max_line_len];
  memcpy(mbuf,&buf[start],len-start);
  mbuf[len-start] = 0;
  printf("asctor: <%s> digs %ld, len %ld\n",mbuf,digs,len);
  printf("bpdigs %ld, apdigs %ld, expdigs %ld ", bpdigs, apdigs, expdigs);
  printf("hadneg %d, hadpoint %ld, hadexp %ld, hadeneg %d\n",
	 hadneg, hadpoint, hadexp, hadeneg);
#endif
  if ((bpdigs < 9) && !apdigs && !hadexp) { /* quick and dirty */
    s=&buf[digs];
    for (i=0;i<bpdigs;i++,s++)
      base = base*radix + ((*s <= '9') ? *s - 48 : *s - 55);
    goto asctorout;
  }
  m = mkBigNum(0.L,&mb);
  s=&buf[digs]; /* deal with digits before point */
  for (i=0;i<bpdigs;i++,s++) {
    mlt =  (*s <= '9') ? *s - 48 : *s - 55;
    base = base*radix + mlt;
    maddBn(m,radix,mlt);
  }
  s=&buf[hadpoint]; /* deal with digits after point */
  for (i=0;i<apdigs;i++,s++) {
    mlt   =   (*s <= '9') ? *s - 48 : *s - 55;
    pwr  *= radix;
    base += mlt/pwr;
    maddBn(m,radix,mlt);
  }
  s=&buf[hadexp];  /* deal with exponent digits */
  for (i=0;i<expdigs;i++,s++) exponent = exponent*radix + *s - 48;
  if (hadeneg) {
    for (i = 0; i < exponent; i++) base /= radix;
    exponent = - exponent;
  }  else         for (i = 0; i < exponent; i++) base *= radix;
  exponent -= apdigs;
  base      = findBest(m,radix,exponent,base);
  asctorout:
  bumpRasc(0);
  *dres = (hadneg) ? -base : base;
#if (RASCAL_DEBUG)
  printf("res: %g\n",*dres);
#endif  
  return false;
}  /* asctor */
