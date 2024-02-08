/* ****************** FORMATTERN **************** */
#include "alps.h"

#define ustrlen(s) strlen((char *)s)

/* convert integer to decimal char string */
static uchar itos_buf[16][16]; /* no more that 16 in one call (Outsn) */
static int  itos_ind = 0;
uchar *itos(integer i) {
  register uchar *c = &itos_buf[itos_ind][15];
  if (++itos_ind == 16) itos_ind = 0;
  int s;
  if ((s = (i < 0))) i = labs(i);
  *c = 0;
  do { *--c = 48 + i % 10; } while (i/=10);
  if (s) *--c = '-';
  return c;
}

/* convert integer to decimal char string with width */
uchar *itosn(integer i, integer w) {
  uchar *s = itos(i);
  integer n = ustrlen(s);
  if (w > 15) w = 15; /* silently */
  i = w - n;
  while (i-->0) *--s=' ';
  return s;
}
  
/* convert integer to hexadecimal char string */
static uchar itox_buf[16][32];
static int itox_ind = 0;
uchar *itox(whole k) {
  register uchar *c = &itox_buf[itox_ind][31];
  register int j;
  if (++itox_ind == 16) itox_ind = 0;
  *c = 0;
  for (j=0;j<indlen*2;k >>= 4,j++) *--c = numdig[k & 0xf];
  *--c = 'x'; *--c = '0';
  return c;
}

/* convert string to integer */
bool stoi(char *s, whole *i) {
  whole j=0;
  char t;
  while ((t = *s++)) {
    j *= 10;
    if (isDigit(t)) j += t - '0';
    else return false;
  }
  *i = j;
  return true;
}

/* extract substring from argument */
#define SUBS_BUFLEN 80
static char subs_buf[16][SUBS_BUFLEN + 8];
static int  subs_ind = 0;
char *subs(char *src, integer len) {
  register char *c = subs_buf[subs_ind];
  if (++subs_ind == 16) subs_ind = 0;
  if (len > SUBS_BUFLEN) len = SUBS_BUFLEN; // quietly truncated
  memcpy(c,src,len);
  c[len] = 0; // null terminate
  return c;
}

/* convert character string to ascii hex string len(result) = 2*len(arg) */
#define STOX_BUFLEN 256
#define STOX_STRLEN (STOX_BUFLEN / 2)
static char stox_buf[16][STOX_BUFLEN + 2];
static int stox_ind = 0;
char *stox(char *s, integer l) {
  register char *c;
  register int j,k;
  int ovfl = 0;
  if (++stox_ind == 16) stox_ind = 0;
  if (l > STOX_STRLEN) { l = STOX_STRLEN; ovfl = 1;}
  c = stox_buf[stox_ind];
  for (j=0;j<l;j++) {
    k    = *s++;
    *c++ = numdig[(k >> 4) & 0xf];
    *c++ = numdig[k & 0xf];
  }
  if (ovfl) (*c++ = '?');
  *c = 0;
  return stox_buf[stox_ind];
}

static idstr emes_buf;

char *mkEmes(int n, ...) { /* make an Error message */
  integer i,len,ind=0;
  uchar  *buf;
  char   *c;
  va_list ap;
  buf = emes_buf;
  va_start(ap,n);
  for (i=0;i<n;i++) {
    c = va_arg(ap, char *);
    len = strlen(c);
    if (len+ind > max_line_len) break;
    memcpy(&buf[ind],c,len);
    ind += len;
  }
  buf[ind] = 0;
  va_end(ap);
  return (char *)buf;
}

static idstr fmes_buf;

char *mkFmes(char * mes,fibrec *fex) { /* make file message */
  integer len  = nels(fex->fob.fb->name);
  integer slen = strlen(mes);
  /* (, mes fname) */
  if ((len + slen + 1) > max_line_len) len = max_line_len - slen - 1;
  memcpy(fmes_buf,mes,slen);
  memcpy(&fmes_buf[slen],vadd(fex->fob.fb->name).sp,len);
  fmes_buf[slen+len] = 0; /* null terminate */
  return (char *)fmes_buf;
}

char *asc(char    *Result,    /* ascii representation of val   */
	  integer *len,       /* length of result              */
	  double   val,       /* value to convert              */
	  integer  prec,      /* desired digits after point    */
	  integer  base,      /* radix for conversion          */
	  bool    *manSign,   /* mantissa sign present         */
	  integer *ndigs,     /* number of digits              */
	  integer *ndigap,    /* number of digits after  point */
	  bool    *expSign,   /* exponent sign present         */
	  integer *expLen,    /* number of digits in exponent  */
	  bool     fExpSign,  /* force exponent sign           */
	  integer  fExpLen,   /* force len of exponent         */
	  integer  sd,        /* number of significant digits  */
	  integer  fexp)      /* force e-notation              */
{
  integer pos;
  uchar *s,*e;
  integer i,n,expo,ndig;
  int sign,pnt;
  uchar buf[256];
  *manSign = 0;
  *ndigap  = 0;
  *expSign = 0;
  *expLen  = 0;
  if (alpsIsInf(val)) { memcpy(Result,"Inf",3); *ndigs=*len=3; return Result; }
  if (alpsIsNaN(val)) { memcpy(Result,"Nan",3); *ndigs=*len=3; return Result; }
  if (fExpSign || fExpLen) fexp = 1;
  // all rounding is done by rascal, based on prec and fexp qv
  ndig = rascal(val,base,sd,prec,&sign,buf,&pnt,fexp); 
  buf[ndig] = 0;
  s = buf;
  e = s + ndig;
  pos = 0;
  /*
  alpsOutsn(alpsout,6,"asc:=> ",buf,
	    itosn(ndig,3),itosn(pnt,3),itosn(prec,3),"\n");
  */
  if (sign) {Result[pos++] = neg; *manSign = true;}
  if (pnt <= 0) { /* point to the left of first digit */
    if ((prec >= 0) && !fexp) { /* dude wants prec digits */
      if (prec) {
	Result[pos++] = point;
	for (i=0;i<prec;i++) Result[pos++] = (!*s || pnt++ < 0) ? zero : *s++;
      } else Result[pos++] = '0';
      *ndigap = ndig = prec;
    } else { /* prec undefined */
      if (fexp || ((ndig - pnt) > sd)) { /* too far to the left=>e format */
	Result[pos++] = *s++;
	n = (prec >=0) ? prec : ndig -1;
	*ndigap  = n;
	ndig     = n + 1;
	if (n>0) Result[pos++] = point;
	for (i=0;i<n;i++) Result[pos++] = (e - s) ? *s++ : zero;
	Result[pos++] = dexp;
	Result[pos++] = neg;
	*expSign      = true;
	expo          =  1 - pnt;
	s             = itos(expo);
	*expLen       = ustrlen(s);
	n             = fExpLen - *expLen;
	for (i=0; i< n; i++) Result[pos++] = zero;
	while (*s) Result[pos++] = *s++;
      } else { /* digits after point within field width */
	Result[pos++] = point;
	for (i=pnt;i<0;i++)  Result[pos++] = zero;
	for (i=0;i<ndig;i++) Result[pos++] = *s++;
	*ndigap  = ndig = ndig + abs(pnt);
      }
    } /* no prec */
  } else { /* point to the right of first digit */
    if (prec > 0 && !fexp) { /* user wants prec digs after point */
      n = pnt+prec;
      for (i=0;i<=n;i++) {
	if (i == pnt) Result[pos++] = point;
	else Result[pos++] = (e-s) ? *s++ : zero;
      }
      *ndigap = n - pnt; 
      ndig    = n;  // assert ?
    } else { /* no can do prec or no particular prec */
      n = max(ndig,pnt);
      if (!fexp && ((sd < 0) || (n <= sd))) {/* point within field width */
	*ndigap = n - pnt;
	ndig = n;
	if (pnt==n) n--; /* no trailing point */
	for (i=0;i<=n;i++)
	  Result[pos++] = (i==pnt) ? point : (e-s) ? *s++ : zero;
      } else { /* too far to the right => e format */
	Result[pos++] = *s++;
 	n =(prec < 0) ? ndig-1 : prec; // 1st dig already there
	*ndigap = n;
	ndig    = n + 1;
	if (n>0) Result[pos++] = point;
	for (i=0;i<n;i++) Result[pos++] = (e - s) ? *s++ : zero;
	Result[pos++] = dexp;
	*expSign = false;
	if (fExpSign) Result[pos++] = plus;
	expo    = pnt - 1;
	s       = itos(expo);
	*expLen = ustrlen(s);
	n       = fExpLen - *expLen;
	for (i=0; i< n; i++) Result[pos++] = zero;
	while (*s) Result[pos++] = *s++;
      }
    }
  }
  *len   = pos;
  *ndigs = ndig;
  return Result;
}

/* format number val according to len prec and base */
uchar *fmtnum(uchar   *Result, integer *len,  double val,
	      integer  prec,   integer  base, integer sd, integer fexp,
	      bool     ne,     integer  me,   integer nopad)  {
  integer i, ilen, k, ndigs, ndigap, expLen, mlen;
  bool manSign, expSign;
  idstr r,r1;
  char  fill = space;
  uchar *rr;
  uchar *res = Result;
  uchar *dig = r;
  if (base < 2 || base > max_base) error(inv_funarg,"unsupported radix");
  if (*len < 0) { fill = zero; *len = labs(*len); } /* pad on left with zeros */
  asc((char *)r, &ilen, val, prec, base,           /*convert number to ascii */
      &manSign, &ndigs, &ndigap, &expSign, &expLen, ne, me, sd, fexp);
  /* Prefix non decimal digit constants with 0 if no width specified */
  if (!*len && !isDigit(*dig) && (*dig != point) && (*dig != neg) && (*dig != plus)) {
    memcpy(&r1[1],r, (ilen < max_line_len) ? ilen : ilen-1);
    if (ilen < max_line_len) ilen++;
    r1[0] = zero;
    dig = rr = r1;
  } else {
    rr = r;
  }
  if (*len == 0) *len = ilen;                      /* actual is field width  */
  mlen = *len;
  if (nopad==0) { *res++ = space; (*len)++; }  /* pad with a space on left */
  k = mlen - ilen;           /* justify as per user length requested         */
  if (k < 0) {               /*    does not fit ... so that is the deal      */
    mlen--;
    memcpy(res, rr, mlen);   /*    copy length -1 result chars               */
    res += mlen;
    *res = '?';              /*    and signal perplexity in last char        */
    return Result;
  }
  if (k > 0) { /* first there is some filling to do so get out the drill */
    if (nopad >= 0) { /* pad on left with zeros or spaces */
      if ((*dig == neg) && (fill == zero)) { *res++ = *dig++; mlen--; }
      for (i=0; i < k; i++) *res++ = fill;
      mlen -= k;
    } else { /* pad on right with spaces */
      memcpy(res,dig,mlen-k);
      for (i=mlen-k;i<mlen;i++) res[i] = space;
      return Result;
    }
  }
  memcpy(res,dig,mlen);
  return Result;
}  /*fmt*/

/* Real to String internal routine */
static uchar rtos_buf[16][32]; /* no more that 16 in one call (alpsOutsn) */
static int  rtos_ind = 0;
uchar *rtos(number val,integer len,integer prec) {
  uchar *s = fmtnum(rtos_buf[rtos_ind],&len,val,prec,10,10,0,0,0,1);
  if (++rtos_ind == 16) rtos_ind = 0;
  s[len] = 0;
  return(s);
}

/* numeric time to time-string internal time formatting routine */
static uchar rtot_buf[16][16]; /* no more that 16 in one call (alpsOutsn) */
static int  rtot_ind = 0;
uchar *rtot(number val) { 
  uchar *s,*s1,*s2,*s3;
  number  sec;
  integer min,hour;
  s    = rtot_buf[rtot_ind];
  if (++rtot_ind == 16) rtot_ind = 0;
  min  = trunc(val / 60);
  sec  = 100 + val - min*60;
  hour = min / 60;
  min  = 100 + min % 60;
  s1   = rtos(sec,5,1);
  s2   = itos(min);
  s3   = itosn(hour,4);
  memcpy(s,s3,4);
  s[4] = colon;
  s[5] = s2[1];
  s[6] = s2[2];
  s[7] = colon;
  memcpy(&s[8],++s1,5); 
  return(s);
}

static char specBuf[256];
char * specToString(pointer lex) {
  int n=0,len;
  if (isEnv(lex)) return "#<env>";
  if (isCnt(lex)) return "#<ctl>";
  if (isTcb(lex)) {
    strncpy(specBuf,"#<tcb:",7);
    strncat(specBuf,(char *)itos(getTcbTid(lex)),sizeof(specBuf) - 8);
    strncat(specBuf,"> ",1); // added space for gcc 8.1.1 silly warning
    return specBuf;
  }
  if (isFob(lex)) {
    memcpy(specBuf,"#<fob:",n+=6);
    len = min(nels(lex.fb->name),(sizeof(specBuf) - 8));
    memcpy(&specBuf[6],vadd(lex.fb->name).sp,len);
    memcpy(&specBuf[len+6],">",2);
    return specBuf;
  }
  return NULL;
}

/* process format primitive from user (fmt ...) */
integer format(integer nargs, pointer args, uchar *s)  {
  integer i, j, k, m, nopad, len, flen, prec, fexp;
  int lp,rp; /* number of left/right pad chars  */
  int sd, lsd, dbp, mnp, npt;
  /*  width prec & base: val, index, number of them*/
  int  fw, fwi=0, fwn, pc, pci=0, pcn, ob, obi=0, obn;
  idstr t;
  number val,aval,lval;
  pointer lex,fwx,pcx,obx;
  char *spec;
  bool gotpc,gotfw,gotob;

  j = k = 0;
  sd = (integer)getNumVal(a.v.hsd);
  while (j < nargs && k < max_line_len) {
    gotpc = gotfw = gotob = 0;
    lex = argn(j++,args);
    ASSERT(!isAnon(lex));
    if (isaFil(lex)) lex = lex.fb->name; // files render their names
    if (!isAtom(lex)) s[k++] = '?'; /* forget it mate */
    else {
      if ((j < nargs) && isColon(argn(j,args))) {
	j++;
	if (j>=nargs || (!isColon(argn(j,args)) && !isNum(argn(j,args))))
	  error(inv_funarg,"field width specifier");
	if (isNum(fwx=argn(j,args))) {
	  fwn = nels(fwx); fwi = 0; fw = getInt(fwx); j++; gotfw = true;
	  for (i=0;i<fwn;i++) {
	    m = labs(getIntn(fwx,fwi));
	    if (m <0 || m >= max_line_len)
	      error(inv_funarg,"field width specifier out of range 0-256");
	  }
	}
	if ((j < nargs) && isColon(argn(j,args))) {
	  j++;
	  if (j >= nargs || (!isColon(argn(j,args)) && !isNum(argn(j,args))))
	    error(inv_funarg,"precision specifier");
	  if (isNum(pcx=argn(j,args))) {
	    pcn = nels(pcx); pci=0; pc=getInt(pcx); j++;  gotpc = true;
	  }
	  if ((j < nargs) && isColon(argn(j,args))) {
	    j++;
	    if (j >= nargs || !isNum(obx=argn(j,args)))
	    error(inv_funarg,"output base specifier");
	    obn = nels(obx); obi=0; ob=getInt(obx); j++;  gotob = true;
	  }
	}
      }
      if (isaChr(lex) || (spec = specToString(lex))) {
	if (!isaChr(lex))  flen = strlen(spec);
	else { flen = nels(lex); spec = (char *)vadd(lex).sp; }
	if (!gotfw) fw = flen; // default width is actual length of string
	else        fw = abs(fw);
	if (!gotpc) pc = -1;   // default is left justified
	m = (fw < flen) ? fw : flen; // resulting width is shorter of the two
	if (pc > 0) {          // right justified
	  lp = fw - m; rp = 0; // left pad and right pad
	} else if (pc == 0) {  // center justified
	  rp = (fw - m)/2; lp = fw - m - rp;
	} else {               // must be left justified
	  rp = fw - m; lp = 0;
	}
	for (i=0; (k<max_line_len && i<lp); i++) s[k++] = space;
	for (i=0; (k<max_line_len && i<m) ; i++) s[k++] = spec[i];
	for (i=0; (k<max_line_len && i<rp); i++) s[k++] = space;
      } else if (isNum(lex)) {
	if (!gotfw) fw = (integer)getNumVal(a.v.hfw);
	if (!gotpc) pc = (integer)getNumVal(a.v.hpp);
	if (!gotob) ob = (integer)getNumVal(a.v.hob);
	// if (dims(lex) > 0) s[k++] = '[';
	nopad = true;
	for (m=0; m < nels(lex); m++) {
	  i = 0;
	  // if precision is negative force exponent format
	  if (gotpc && pc <0) {fexp = 1; pc=abs(pc); }
	  else fexp = 0;
	  // adjust significant digits as a funcion  of field width
	  // yes... it is a mess.
	  flen = fw;
	  val = vadd(lex).np[m];
	  len = labs(flen);
	  aval = fabs(val);
	  if (ob==2) lval = log2(aval);
	  else if (ob==10) lval = log10(aval);
	  else lval = log(aval) / log(ob);
	  dbp = trunc(lval) + 1;    // digits before point (TODO rounding!)
	  if (len > 0) { // compute local significant digits
	    lsd = len;   // start with local significant digs == len
	    if (val < 0) lsd--;  // minus sign eats one digit
	    mnp = ((lval - trunc(lval)) > 0);  // mantissa needs point
	    npt = ((aval - trunc(aval)) > 0);  // num needs point
	    if (dbp > lsd) {
	      fexp = 1;  // force exponent format
	      lsd--;     // first digit,   sd = places after point
	    }
	    if (fexp) {
	      lsd--;               // exponent symbol
	      if (aval < 1) lsd--; // exponent sign
	      if ((lsd == 2) && mnp) lsd--;      // mantissa point
	    } else {
	      if ((lsd >= (1 + dbp)) && npt) lsd--; // forget point
	    }
	    if (lsd < 1) lsd = 1;
	  } else {
	    lsd = sd;
	  }
	  fmtnum(t, &flen, val, pc, ob, lsd, fexp, 0, 0, nopad);
	  while (k < max_line_len && i < flen)  s[k++] = t[i++];
	  nopad = (fw != 0);
	  if (gotfw) { fwi++; if (fwi>=fwn) fwi=0; fw = getIntn(fwx,fwi); }
	  if (gotpc) { pci++; if (pci>=pcn) pci=0; pc = getIntn(pcx,pci); }
	  if (gotob) { obi++; if (obi>=obn) obi=0; ob = getIntn(obx,obi); }
	}
	// if ((k < max_line_len) && (dims(lex) > 0)) s[k++] = ']';
      } else s[k++] = '?';
    }
  }
  return k;
}

pointer cvtNum(fibrec *lex, int radix) {
  double base;
  asctor(lex->aline,    lex->alen,     radix,        lex->nstart,
	 lex->ndigs[0], lex->ndigs[1], lex->ndigs[2],
	 lex->hadneg,   lex->hadpoint, lex->hadeneg, lex->hadexp,
	 &base);
  return(mkNum(base)); 
}  /*cvt num*/


pointer cvtCpx(fibrec *lex, int radix, pointer res) {
  double base;
  integer lnd,convOk;
  pointer rex;

  asctor(lex->aline,    lex->alen,     radix,        lex->nstart,
	 lex->ndigs[0], lex->ndigs[1], lex->ndigs[2],
	 lex->hadneg,   lex->hadpoint, lex->hadeneg, lex->hadexp,
	 &base);
  if (isNil(res)) {
#if (COMPLEX!=true)
    rex = mkNum(base);  /* don't let cpx's creep in */
#else
    rex = mkCpx(base,0.0);
#endif
  } else {
    rex = res;
    vadd(rex).cp[0] = vadd(rex).np[0] + (base*I);
  }
  return(rex);                     /* send answer for protection too  */
}
