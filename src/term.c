#include "alps.h"
#include <errno.h>
#include <sys/stat.h>
#include <sys/sysmacros.h>
/********* Terminal Reader Stuff **************/

enum escmodes {EM_NORMAL = 0, EM_ESCAPE = 1, EM_ESC1 = 2,
	       EM_ESC2 = 3, EM_DO_CQ = 4, EM_DO_TR = 5};

static bool  isRawMode(fibrec *fp) { return ((fp->ttyMode & 1) != 0); }
void setRawMode(fibrec *fp) { fp->ttyMode |=  1; }
void clrRawMode(fibrec *fp) { fp->ttyMode &= ~1; }

static bool  isHistMode(fibrec *fp) { return ((fp->ttyMode & 2) != 0); }
static void setHistMode(fibrec *fp) { fp->ttyMode |=  2; }
static void clrHistMode(fibrec *fp) { fp->ttyMode &= ~2; }

static bool  isSearchUpMode(fibrec *fp) { return ((fp->ttyMode & 4) != 0); }
static void setSearchUpMode(fibrec *fp) { fp->ttyMode |=  4; }
static void clrSearchUpMode(fibrec *fp) { fp->ttyMode &= ~4; }

static bool  isSearchDnMode(fibrec *fp) { return ((fp->ttyMode & 8) != 0); }
static void setSearchDnMode(fibrec *fp) { fp->ttyMode |=  8; }
static void clrSearchDnMode(fibrec *fp) { fp->ttyMode &= ~8; }

static bool    searchMode(fibrec *fp) { return ((fp->ttyMode & 12) != 0); }
static void clrSearchMode(fibrec *fp) { fp->ttyMode &= ~12; }

static bool    needReset(fibrec *fp)  { return ((fp->ttyMode & 48) != 0); }
       void setNeedReset(fibrec *fp)  { fp->ttyMode |=  16; }
static bool needHardReset(fibrec *fp) { return ((fp->ttyMode & 32) != 0); }
    void setNeedHardReset(fibrec *fp) { fp->ttyMode |=  32; }
static void clrNeedReset(fibrec *fp)  { fp->ttyMode &= ~48; }

static bool    havePrompt(fibrec *fp) { return ((fp->ttyMode & 64) != 0); }
       void setHavePrompt(fibrec *fp) { fp->ttyMode |=  64; }
static void clrHavePrompt(fibrec *fp) { fp->ttyMode &= ~64; }

static bool  isModified(fibrec *fp) { return ((fp->ttyMode & 128) != 0); }
static void setModified(fibrec *fp) { fp->ttyMode |=  128; }
       void clrModified(fibrec *fp) { fp->ttyMode &= ~128; }

static void clrFP() { alpsOuts(alpsout,"[J"); }/* clear from point */
static void cursRet() { alpsOutc(alpsout,'\r'); }

static inline void cursMov(int n, const char *c) {
  if (n > 0)  alpsOutsn(alpsout,3,"[",itos(n),c);
}

static inline void cursUp(int n)   { cursMov(n,"A"); }

static inline void cursDown(int n) { cursMov(n,"B"); }

static inline void cursFwd(fibrec *fb, int n)  {
  int p = fb->ilpos + fb->ipos;
  int d,r = fb->twidth - (p % fb->twidth);
  if (n < r)  cursMov(n,"C");
  else {
    d = 1 + ((n - r) / fb->twidth);
    cursDown(d);
    cursRet();
    cursMov((n - r) % fb->twidth,"C");
  }
  fb->ilpos += n;
 }
  
static inline void cursBack(fibrec *fb, int n) { 
  int p = fb->ilpos + fb->ipos;
  int u,r = p % fb->twidth;
  if (n <= r) cursMov(n,"D"); 
  else {
    u = 1 + ((n - r) / fb->twidth);
    cursUp(u);
    cursRet();
    cursMov((p - n) % fb->twidth,"C");
  }
  fb->ilpos -= n;
}

static inline void cursBol(fibrec *fb) {
  cursBack(fb,fb->ilpos);
  ASSERT(fb->ilpos == 0);
}

static inline void cursEol(fibrec *fb) {
  cursFwd(fb,fb->ecci-fb->ilpos); 
}

static inline void cursBolClr(fibrec *fb) {  cursBol(fb);  clrFP(); }

static void showLine(fibrec *fb, uchar *buf, int len) {
  if (len <= 0) return;
  alpsOutb(alpsout,buf,len);
  fb->ilpos += len;
  if (0 == ((fb->ipos + fb->ilpos) % fb->twidth))
    /* stoopid coisa sticks at right edge of screen even with wrap on */
    alpsOutln(alpsout);
}

static void updateLine(fibrec *fb) { // update line from point
  uchar *lbuf = vadd(fb->ibuf).sp;
  int   len  = fb->ecci - fb->ilpos;
  ASSERT(len >= 0)
  showLine(fb,&lbuf[fb->ilpos],len);
  cursBack(fb,len);
}

static void setLine(fibrec *fb, pointer lex) {
  int len = nels(lex);
  uchar *lbuf = vadd(fb->ibuf).sp;
  cursBolClr(fb);
  memcpy(lbuf,vadd(lex).sp,len);
  fb->ecci  = len; 
  fb->mlpos = -1;
  showLine(fb, lbuf, len);
}

static bool cmpLine(fibrec *fb, pointer lex) {
  integer len = nels(lex);
  uchar *bs   = vadd(fb->ibuf).sp;
  uchar *lbuf = vadd(lex).sp;
  int i,j=1+len-fb->ecci;
  for (i=0;i<j;i++) if (memcmp(bs,&lbuf[i],fb->ecci) == 0) return true;
  return false;
}

static void setPos(fibrec *fb, integer pos) {
  int d,p=fb->ilpos;
  if (0 == (d = fb->ipos - pos)) return;
  if (d > 0) cursBack(fb,d);
  else       cursFwd(fb,d);
  fb->ipos  = pos;
  fb->ilpos = p;
}

static void showHistCandidate(fibrec *fb) {
  pointer rex = fb->cp;
  cursBolClr(fb);
  if (isNil(rex)) return;
  rex = mcar(rex);
  if (!searchMode(fb)) setLine(fb,rex);
  else showLine(fb,vadd(rex).sp,nels(rex));
}

static void searchHist(fibrec *fb) {
  pointer lex,rex,tex; 
  if (fb->ecci == 0) return; /* nothing to search for */
  if (isSearchDnMode(fb)) tex = nrevList(lookup(a.v.hhs));
  for (lex = fb->hp;!isNil(lex);lex = mcdr(lex)) {
    rex = mcar(lex);
    if (cmpLine(fb,rex)) {
      fb->cp = lex;  // this is our candidate  
      setHistMode(fb);
      break;
    }
  }
  showHistCandidate(fb);
  if (isSearchDnMode(fb)) rex = nrevList(tex);
}

static void showSearch(fibrec *fb) {
  uchar *lbuf = vadd(fb->ibuf).sp;
  setPos(fb,0);
  cursBolClr(fb);
  if (isSearchUpMode(fb)) alpsOuts(alpsout,"^r\"");
  else                    alpsOuts(alpsout,"^s\"");
  alpsOutb(alpsout,lbuf,fb->ecci);
  alpsOuts(alpsout,"\": ");
  fb->ipos   = fb->ecci+6;
}

static void lineReset(fibrec *fb) {
  int j,ip=fb->ilpos;
  uchar *lbuf = vadd(fb->ibuf).sp;
  if (searchMode(fb)) showSearch(fb);
  else {
    if (havePrompt(fb)) {
      if (!needHardReset(fb)) cursBol(fb);
      cursRet(); clrFP();
      alpsout->olpos = 0;
      alpsPrompt(fb,alpsout);
    }
    if (isHistMode(fb)) {
      showHistCandidate(fb);
      j = fb->ecci-ip;
      cursBack(fb,j);
    } else {
      if (!needHardReset(fb)) cursBol(fb);
      else cursRet();
      clrFP();
      fb->ilpos = 0;
      if (fb->ecci > 0) {
	j = fb->ecci-ip;
	showLine(fb,lbuf,fb->ecci);
	cursBack(fb,j);
      }
    }
  }
  clrNeedReset(fb);
  alpsFlush(alpsout);
}

static void rstSearchMode(fibrec *fb) {
  if (searchMode(fb)) {
      clrSearchMode(fb);
      fb->ecci = 0;
      lineReset(fb);
      fb->hp = (isNil(fb->cp)) ? lookup(a.v.hhs) : fb->cp;
  } 
  fb->cp = pnil; // always kill candidate 
}

static void chkHistCandidate(fibrec *fb) {
  if (!isNil(fb->cp)) {
    if (searchMode(fb)) {
      clrSearchMode(fb);
      lineReset(fb);
    }
    setLine(fb,mcar(fb->cp)); /* ilpos is at the end here */
    if ((fb->cp.pt == lookup(a.v.hhs).pt) && (fb->ulpos > 0))
      cursBack(fb,fb->ecci-fb->ulpos);
    fb->hp = fb->cp;  /* history search continues from candidate */
    fb->cp = pnil;    /* no candidate no more */
  }
  clrSearchMode(fb);
  clrHistMode(fb);
}

static void saveUndo(fibrec *fb) {
  fb->ulpos = fb->ilpos; /* save undo buf */
  fb->ulen  = fb->ecci;
  memcpy(vadd(fb->ubuf).sp,vadd(fb->ibuf).sp,fb->ecci);
}

static void saveYank(fibrec *fb, uchar *buf, int len) {
  fb->ylen  = len;
  memcpy(vadd(fb->ybuf).sp,buf,len);
}

static pointer findCompletion(uchar *buf, integer len) {
  pointer lex,rex,tex,res;
  lex  = mcar(oblis_head(buf,len));
  res  = pnil;
  while (!isNil(lex)) {
    tex = mcar(lex);
    if ((len <= nels(tex)) && !memcmp(vadd(tex).sp,buf,len)) { 
      rex = getfs(1); 
      setCar(rex,tex);
      if (isNil(res)) { res = rex;       protect(res); } // save head
      else            { setCdr(res,rex); res = rex; }
    }
    lex  = mcdr(lex);
  }
  if (!isNil(res)) res = unprotect();
  return res;
}

static pointer findFileCompletion(uchar *buf, integer len) {
  pointer lex,rex,orex,res;
  uchar obuf[max_line_len],ebuf[max_line_len],*fn,*mi;
  char *dn,*mEnv;
  int i,j,dl,fl,el,l2=0;
  DIR *dp;  
  struct dirent *ep;
  protect(res = pnil); // protect res from the boogeyMan
  mi  = buf;
  if ((len >= 2) && (0 == memcmp(buf,"~/",2))) {
    if (NULL != (mEnv = getenv("HOME"))) {
      l2 = strlen(mEnv);
      memcpy(obuf,mEnv,l2);
      mi++; // skip tilde
      len--;
    }
  }
  memcpy(&obuf[l2],mi,len); 
  len += l2;
  for (i=len-1; i>=0 ; i--) if (obuf[i]=='/') break;
  if (i<0)   { dn = ".";  dl = 1; fn = obuf;       fl = len;  }
  else
    if (i>0) { 
      dn = (char *)obuf; dl = i; fn = &obuf[i+1]; fl = len-i-1; dn[dl] = 0;
    } else   { dn = "/";  dl = 1; fn = &obuf[dl];  fl = len-1;  }
  dp = opendir(dn);
  if (dp == NULL) return(pnil);
  while ((ep = readdir(dp))) {
    el = strlen(ep->d_name);  // names too long are silently dropped
    if ((el>=fl) && (el<max_line_len-1) && (0==memcmp(fn,ep->d_name,fl))) {
      memcpy(ebuf,ep->d_name,el); // found one with matching prefix
      for (j=1,rex=orex=res;!isNil(rex);orex=rex,rex=mcdr(rex)) {
	if ((j=strcompare((char *)ebuf,el,mcar(rex))) <= 0) break; // out of for
      }
      if ((j < 0) || isNil(rex)) { // add in sorted order
	lex = getfs(1);
	if (isEqq(rex,res)) { // insert at head (how many times again ?!)
	  unprotect();
	  setCdr(lex,res);
	  protect(res=lex);
	} else {
	  setCdr(lex,rex);
	  setCdr(orex,lex);
	}
      } else error(pgmr_inc,"dup file in complist");
#if (HOST!=SOLARIS32) && (HOST!=SOLARIS64)
      if (ep->d_type == DT_DIR) ebuf[el++] = slash;
#endif
      setCar(lex,buildChr((char *)ebuf,el));
    }
  } // while ep
  closedir(dp);
  lex = unprotect();
  ASSERT(isEqq(res,lex));
  return res;
}

static integer findLCPrefix(pointer lex) { // find Longest Common Prefix
  pointer rex = mcar(lex);
  integer i,rlen,plen=nels(rex);
  uchar *pref = vadd(rex).sp;
  if (isNil(lex)) return plen;
  lex  = mcdr(lex);
  while (plen && !isNil(lex)) {
    rex  = mcar(lex);
    rlen = nels(rex);
    plen = (rlen > plen) ? rlen : plen;
    lex  = mcdr(lex);
    for (i=0;i<plen;i++) if (pref[i] != vadd(rex).sp[i]) break;
    plen = i;
  }
  return plen;
}

static void showCompletion(fibrec *fb, pointer lex) {
  pointer mex,rex;
  integer j,l;
  uchar *lbuf = vadd(fb->ibuf).sp;
  l = 0;
  for (rex=lex;!isNil(rex);rex=mcdr(rex)) /* find longest candidate */
    if (l < nels(mcar(rex))) l =  nels(mcar(rex));
  alpsOuts(alpsout,"\r\n[J");
  l++;
  for (rex=lex;!isNil(rex);rex=mcdr(rex)) {
    mex = mcar(rex);
    alpsOutb(alpsout,vadd(mex).sp, nels(mex));
    if (alpsout->olpos + 2*l - nels(mex) > fb->twidth)  alpsOutln(alpsout);
    else alpsOutn(alpsout,space,l-nels(mex));
  }
  alpsOutln(alpsout);
  clrFP();
  if (havePrompt(fb)) alpsPrompt(fb,alpsout);
  j = fb->ecci-fb->ilpos;
  fb->ilpos = 0;
  if (fb->ecci > 0) {
    showLine(fb,lbuf,fb->ecci);
    cursBack(fb,j);
  }
}

static integer translate(uchar key, uchar *buf, pointer alist) {
  pointer keycode = chartab[key];
  pointer lex     = assoc(keycode,alist);
  integer i,len;
  if (isNil(lex) || !isaChr(mcadr(lex))) return 1;  // no change baby
  lex = mcadr(lex);
  len = nels(lex);
  memcpy(buf, vadd(lex).sp, len);
  return len;
} 

static const char *oPars = "{[(";
static bool isOPar(char c, int *p) {
  const char *s = oPars;
  while (*s) { if (*s == c) {*p = s - oPars; return true; }  s++; }
  return false;
}

static const char *cPars = "}])";
static bool isCPar(char c, int *p) {
  const char *s = cPars;
  while (*s) { if (*s == c) {*p = s - cPars; return true; }  s++; }
  return false;
}
  
static bool findPars(fibrec *fb, integer p, integer *p1, integer *p2) {
  int i,n,s,q;
  char cp, op;
  uchar *lbuf = vadd(fb->ibuf).sp;
  n=q=0; /* nesting level zero , not in quotes */
  if ((p < fb->ecci) && isOPar(lbuf[p],&s)) {
    cp  = cPars[s];
    op  = oPars[s];
    *p1 = p;
    for (i=p+1;i<fb->ecci;i++) {
      if (!q && lbuf[i] == cp) {
	if (n==0) { *p2 = i; return true; }
	else n--;
      }
      if (q &&  lbuf[i] == excape) i++; // skip next
      if (      lbuf[i] == dquot)  q = ~q;
      if (!q && lbuf[i] == op)     n++;
    }
  } else if (isCPar(lbuf[p-1],&s)) {
    cp  = cPars[s];
    op  = oPars[s];
    *p1 = p-1;
    n = 0;
    for (i=p-2;i>=0;i--) {
      if (!q && lbuf[i] == op) {
	if (n==0) { *p2 = i; return true; }
	else n--;
      }
      if (q && (lbuf[i] == dquot) && lbuf[i-1] == excape) i--; // skip next
      if (lbuf[i] == dquot) q = ~q;
      if (!q && lbuf[i] == cp) n++;
    }
  }
  return false;
}

static void cursGo(fibrec *fb, int p) {
  int d;
  if (0 == (d = fb->ilpos - p)) return;
  if (d < 0) cursFwd(fb,-d);
  else       cursBack(fb,d);
}       

static void highLight(fibrec *fb, int p1, int len) { 
  uchar *lbuf = vadd(fb->ibuf).sp;
  int oi = fb->ilpos;
  cursGo(fb,p1);
  alpsOuts(alpsout,"[44;37m");
  showLine(fb,&lbuf[p1],len);
  alpsOuts(alpsout,"[m");
  cursGo(fb,oi);
}

static void goFwd(fibrec *fb)  { 
  chkHistCandidate(fb);
  saveUndo(fb);
  if (fb->ilpos < fb->ecci) cursFwd(fb,1);
}

static void goBack(fibrec *fb) { 
  chkHistCandidate(fb);
  saveUndo(fb);
  if (fb->ilpos > 0)  cursBack(fb,1);
}

static void goUp(fibrec *fb) {
  int r = -1;
  rstSearchMode(fb);
  if (isModified(fb)) alpsAddHistory(fb);
  /* go back in history if we can */
  if (!isHistMode(fb)) {
    setHistMode(fb);
    r = fb->ulpos; 
  } else if (!isNil(mcdr(fb->hp))) fb->hp = mcdr(fb->hp);
  if (!isNil(fb->hp)) {
    fb->cp = fb->hp;
    showHistCandidate(fb);
    if (r >= 0) cursGo(fb,r);
  }
}

static void goDown(fibrec *fb) { 
  pointer rex,hist = lookup(a.v.hhs);
  rstSearchMode(fb);
  if (isModified(fb)) alpsAddHistory(fb);
  if (!isHistMode(fb)) return; /* got to go up before you can come down */
  if (!isEqq(fb->hp,hist)) {
    rex    = nrevList(hist);
    fb->hp = (isNil(fb->hp)) ? mcdr(rex) : mcdr(fb->hp);
    //    lex    = mcar(fb->hp);
    nrevList(rex);
    fb->cp = fb->hp;
    showHistCandidate(fb);
  } else { 
    clrHistMode(fb); /* hit the bottom */
    cursBolClr(fb);
    fb->ecci = 0;
  }
}

static int fwdWrd(fibrec *fb) {
  int p;
  uchar *lbuf = vadd(fb->ibuf).sp;
  for (p=fb->ilpos;p<fb->ecci;p++)
    if ((lbuf[p] == point) || (isWord(lbuf[p]))) break;
  for (;p<fb->ecci;p++)
      if ((lbuf[p] != point) && (!isWord(lbuf[p]))) break;
  return p;
}

static int backWrd(fibrec *fb) {
  int p;
  uchar *lbuf = vadd(fb->ibuf).sp;
  if (fb->ilpos == 0) return 0;
  for (p=fb->ilpos-1;p>=0 && (lbuf[p] != point) && !isWord(lbuf[p]);p--);
  for (;p>=0 && ((lbuf[p] == point) || isWord(lbuf[p]));p--);
  return p+1;
}

static void goFwdWrd(fibrec *fb)  { 
  int p;
  chkHistCandidate(fb);
  saveUndo(fb);
  p = fwdWrd(fb);
  cursFwd(fb,p-fb->ilpos);
}

static void goBackWrd(fibrec *fb)  { 
  int p;
  chkHistCandidate(fb);
  saveUndo(fb);
  p = backWrd(fb);
  cursBack(fb,fb->ilpos-p);
}

static int fwdPar(fibrec *fb) {
  int p,q;
  uchar *lbuf = vadd(fb->ibuf).sp;
  for (p=fb->ilpos;p<fb->ecci;p++) if (isOPar(lbuf[p],&q)) break;
  return p;
}

static int backPar(fibrec *fb) {
  int p,q;
  uchar *lbuf = vadd(fb->ibuf).sp;
  if (fb->ilpos == 0) return 0;
  for (p=fb->ilpos-1;p>=0;p--) if (isCPar(lbuf[p],&q)) break;
  return p+1;
}

static void goFwdPar(fibrec *fb)  { 
  integer p,p1,p2;
  chkHistCandidate(fb);
  saveUndo(fb);
  p = fwdPar(fb);
  if (fb->ilpos != p) cursGo(fb,p);
  else  if ((p<fb->ecci) && findPars(fb,p,&p1,&p2))  cursGo(fb,p2+1);
}

static void goBackPar(fibrec *fb)  { 
  integer p,p1,p2;
  chkHistCandidate(fb);
  saveUndo(fb);
  p = backPar(fb);
  if (fb->ilpos != p) cursGo(fb,p);
  else if ((p > 0) && findPars(fb,p,&p1,&p2))  cursGo(fb,p2);
}

static void transpWrd(fibrec *fb) {
  int p1,pb1,p2,pb2,ip,d1,d2,di;  
  uchar *lbuf = vadd(fb->ibuf).sp;
  uchar tbuf[max_line_len];
  setModified(fb);
  chkHistCandidate(fb);
  ip = fb->ilpos;     /* save in case we bail */
  p2 = fwdWrd(fb);    /* end of wd 2 */
  if (p2 == ip) return;
  fb->ilpos = p2;
  pb2 = backWrd(fb);  /* beginning of wd 2 */
  d2 = p2 - pb2;      /* length of wd 2    */
  fb->ilpos = pb2;
  if (p2 != fwdWrd(fb)) { fb->ilpos = ip; return; }
  pb1 = backWrd(fb);  /* beginning of wd 1 */
  if (pb1 == fb->ilpos) { fb->ilpos = ip; return; }
  fb->ilpos = pb1;
  p1 = fwdWrd(fb);    /* end of wd 1              */
  if (p1 >= pb2)        { fb->ilpos = ip; return; }
  saveUndo(fb);                       /* point of no return                */
  d1 = p1 - pb1;                      /* len of wd 1                       */
  di = pb2 - p1;                      /* no of intersticial chars          */
  memcpy(tbuf,&lbuf[pb1],d1+di);      /* save wd 1 and intersticial chars  */
  memcpy(&lbuf[pb1],&lbuf[pb2],d2);   /* put wd 2 in its place             */
  memcpy(&lbuf[pb1+d2],&tbuf[d1],di); /* squish in separator               */ 
  memcpy(&lbuf[pb1+d2+di],tbuf,d1);   /* put wd 1 at the end               */
  fb->ilpos = ip;                     /* restore ilpos                     */
  cursBack(fb,ip-pb1);                /* cursor to beginning of orig wd 1  */
  showLine(fb,&lbuf[pb1],d1+di+d2);   /* replay the whole lot onto display */
}

static void insChars(fibrec *fb, uchar *buf, int len) { /*insert buf at point */
  int ilen,j;
  uchar *lbuf = vadd(fb->ibuf).sp;
  setModified(fb);
  if ((fb->ecci+len) >= max_line_len) len = max_line_len - fb->ecci;
  if (fb->ilpos==fb->ecci) { /* insert buf at EOL */
    memcpy(&lbuf[fb->ecci],buf,len);
    fb->ecci += len;
    showLine(fb,&lbuf[fb->ilpos],len);
  } else { /* insert buf at point */
    ilen = fb->ecci+len-1;
    for (j=ilen;j>fb->ilpos;j--) lbuf[j] = lbuf[j-len];
    memcpy(&lbuf[fb->ilpos],buf,len);
    fb->ecci += len;
    if (fb->mlpos >= fb->ilpos) fb->mlpos += len;
    updateLine(fb);
    cursFwd(fb,len);
  }
}

static void delChars(fibrec *fb, integer p) { /* from p to point */
  integer j,p1,p2,len;  
  uchar *lbuf = vadd(fb->ibuf).sp;
  setModified(fb);
  chkHistCandidate(fb);
  if (p > fb->ecci) p = fb->ecci;
  p1  = min(p,fb->ilpos);
  p2  = max(p,fb->ilpos);
  if ((0 == (len = p2 - p1)) || (p < 0)) return;
  saveUndo(fb);
  saveYank(fb,&lbuf[p1],len);
  for (j=p1;j<fb->ecci-len;j++) lbuf[j] = lbuf[j+len];	
  if (fb->mlpos >= p2) fb->mlpos -= len;
  else if (fb->mlpos > p1) fb->mlpos = p1;
  if (p < fb->ilpos) cursBack(fb,len);
  clrFP();
  fb->ecci -=len;    
  updateLine(fb); 
}	

static void yankChars(fibrec *fb, integer p) { /* from p to point */ 
  integer p1,p2,len;  
  uchar *lbuf = vadd(fb->ibuf).sp;
  chkHistCandidate(fb);
  p1  = min(p,fb->ilpos);
  p2  = max(p,fb->ilpos);
  if ((0 == (len = p2 - p1)) || (p < 0)) return;
  saveYank(fb,&lbuf[p1],len);
}

static void insertLastLine(fibrec *fb) {
  pointer lex= mcdr(fb->hp);
  if (!isNil(lex)) {
    chkHistCandidate(fb);
    saveUndo(fb);
    lex = mcar(lex);
    insChars(fb,vadd(lex).sp,nels(lex));
  }
}

static void rstTermReader(fibrec *fb) { /* reset basic state */
  fb->ilpos = 0; 
  fb->ulen  = 0;         /* no more undo */
  fb->mlpos = -1;
  fb->hp    = lookup(a.v.hhs);
  clrModified(fb);
}

void alpsTermReader(int fd) {
  pointer lex;
  integer len,ilen,j,p1,p2,ibind,iblen; 
  fibrec *fb = fparr[fd];
  uchar *lbuf = vadd(fb->ibuf).sp;
  uchar c,ibuf[max_line_len],mbuf[max_line_len];
  if (fb->isatty != 1) error(pgmr_inc,"Term reader invoked on non tty");
  iblen  = read(fd,ibuf,1);
  if (iblen < 0) {
    alpsMsg(3,"Reader error",strerror(errno),"\n");
    return;
  }
  if (iblen == 0) {
      fb->bufstat = ldvs_eof;
      fb->ecci    = -1;
      fb->ilpos   = 0;
      return;
  }
  if (isRawMode(fb)) {
    memcpy(lbuf,ibuf,iblen);
    fb->bufstat = ldvs_ok;
    fb->ecci    = iblen;
    fb->ilpos   = 0;
    return;
  }
  ibind = 0;
 readsumore:
  c    = ibuf[ibind++];
  if (needReset(fb)) lineReset(fb);
  if (fb->escMode==EM_ESCAPE) {
      switch(c) {
      case '[': fb->escMode = EM_ESC1;    return;
      case 002: goBackPar(fb);            break;
      case 006: goFwdPar(fb);             break;
      case 010: delChars(fb,backWrd(fb)); break;
      case 'b': goBackWrd(fb);            break;
      case 'd': delChars(fb,fwdWrd(fb));  break;
      case 'f': goFwdWrd(fb);             break;
      case 't': transpWrd(fb);            break;
      case 'w': yankChars(fb,fb->mlpos);  break;
      default: alpsOutc(alpsout,7); 
      }
      fb->escMode = EM_NORMAL;
  } else if (fb->escMode==EM_ESC1) {
      switch(c) {
      case 'A': goUp(fb);     break;
      case 'B': goDown(fb);   break;
      case 'C': goFwd(fb);    break;
      case 'D': goBack(fb);   break;
      case 'F': cursEol(fb);  break;
      case 'H': cursBol(fb);  break;
      case '2': //Ins
      case '3': //Del
      case '4': //???
      case '5': //PgUp
      case '6': //PgDn
	fb->escMode = EM_ESC2;    return;
      default: alpsOutc(alpsout,7);
      }
      fb->escMode = EM_NORMAL;
  } else if (fb->escMode==EM_ESC2) { /* trailing escape sequence */
    /* Fling trailing tilde */
    ASSERT(c==126);
    alpsOutc(alpsout,7); 
    fb->escMode = EM_NORMAL;
  } else if (fb->escMode==EM_DO_CQ) { /* control q */
    if (c < space) {
      mbuf[0] = excape;
      if (c == eol) mbuf[1] = 'n';
      else if (c == 28) mbuf[1] = 'q'; // will only work if sigquit disabled
      else mbuf[1] = c+64;
      insChars(fb,mbuf,2);
    } else   insChars(fb,mbuf,1);
    fb->escMode = EM_NORMAL; 
  } else if (fb->escMode == EM_DO_TR) { /* control o */
    len = translate(c, &ibuf[ibind], lookup(a.v.hkb));
    iblen += len;
    fb->escMode = EM_NORMAL;
  } else {
    switch (c) {
    case 0: /* C-SPC set mark */
      if (!isHistMode(fb) && !searchMode(fb)) fb->mlpos = fb->ilpos;
      break;
    case 1: /* C-a goto BOL */
      chkHistCandidate(fb);
      saveUndo(fb);
      if (fb->ilpos == 0) {
	cursEol(fb);
	insChars(fb,(uchar *)")",1);
	cursBol(fb);
	insChars(fb,(uchar *)"(",1);
      } else {
	cursBol(fb);
      }     
      break;
    case 2: /* C-b back one char */
      goBack(fb);    
      break;
    case 4: /* C-d */
      chkHistCandidate(fb);
      if (fb->ecci==0) { /* EOF */
	fb->bufstat = ldvs_eof;
	fb->lchar   = c;
	fb->ulpos   = 0; // = ilpos was in rstTermReader
	rstTermReader(fb);
	return;
      } else {
	delChars(fb,fb->ilpos+1);
      }
      break;
    case 5: /* C-e goto EOL */
      chkHistCandidate(fb);
      saveUndo(fb);
      cursEol(fb);
      break;
    case 6: /* C-f forward one char */
      goFwd(fb);      
      break;
    case 8: /* C-h backspace delete char before point */
    case 127:
      if (searchMode(fb)) {
	if (fb->ecci > 0) { /* only del/bs and inCaP for search */
	  fb->ecci--;
	  showSearch(fb); 
	  searchHist(fb);
	}
      } else {
   	delChars(fb,fb->ilpos-1);
      }
      break;
    case 9: /* C-i (Tab) show completion */
      if (!isHistMode(fb) && !searchMode(fb)) {
	bool fileFlag;
	int p  = backWrd(fb);
	if (0 == (len = (fb->ilpos - p))) break; // gotta gimme a clue
	if ((fileFlag = ((p > 0) && (lbuf[p-1] == dquot)))) {
	  for (j=fb->ilpos-1; j>=p ; j--) if (lbuf[j] == slash) break;
	  lex = findFileCompletion(&lbuf[p],len);
	} else {
	  j   = p - 1; // start index - 1 of suffix to match in lbuf
	  lex = findCompletion(&lbuf[p],len);
	}
	if (isNil(lex)) break; /* no go */
	if (fb->lchar == c) { // user is insisting
	  showCompletion(fb,lex);
	  break;
	}
	ilen = listLen(lex); // number of matching entries
	p2   = findLCPrefix(lex);
	p1   = p2 - fb->ilpos + j + 1; // number of additional common chars
	if (p1 < 0) break; // no joy
	if (p1 == 0) {// first is exact match (no addnl chars matched)
	  if ((ilen > 1) || (fb->ilpos < fb->ecci)) break; //others or no room 
	  else  insChars(fb,(uchar *)(fileFlag?"\"":" "),1); // only match
	} else {
	  memcpy(mbuf,&vadd(mcar(lex)).sp[p2-p1],p1);
	  if (ilen == 1) { // only match
	    if (fileFlag) {
	      if (mbuf[p1-1] != slash) mbuf[p1++] = dquot; // no trailing slash
	    } else mbuf[p1++] = space;
	  }
	  saveUndo(fb);
	  insChars(fb,mbuf,p1);
	} 
      }
      break;
    case 10: /* C-j Newline */
      chkHistCandidate(fb);
      alpsAddHistory(fb);
      fb->ulpos = fb->ilpos; /* needed for hist */
      cursFwd(fb,fb->ecci-fb->ilpos); 
      lbuf[fb->ecci++] = eol; 
      clrHavePrompt(fb);
      fb->bufstat      = ldvs_ok;
      alpsOutc(alpsout,c); 
      alpsFlush(alpsout);
      rstTermReader(fb);
      return;
    case 11: /* C-k kill line from point to eol*/
      delChars(fb,fb->ecci);
      break;
    case 12: /* C-l refresh line */
      lineReset(fb);
      break;
    case 14: /* C-n next history line */
      goDown(fb);
      break;
    case 15: /* C-o exchange mark and point */
      chkHistCandidate(fb);
      if (fb->mlpos < 0) alpsOutc(alpsout,7);
      else {
	j = fb->mlpos;
	fb->mlpos = fb->ilpos;
	cursGo(fb,j);
      }
      break;
    case 16: /* C-p previous history line */
      goUp(fb);   
      break;
    case 17: /* C-q control char escape */ 
      fb->escMode = EM_DO_CQ;
      break;
    case 18: /* C-r reverse search (up) */
      if (isModified(fb)) alpsAddHistory(fb);
      if (!isSearchUpMode(fb)) {
	setSearchUpMode(fb);
	if (isSearchDnMode(fb)) {
	  clrSearchDnMode(fb);
	}  else {
	  fb->ecci = 0;  // start off with a clean slate
	  fb->cp   = pnil;
	}
	showSearch(fb);
      } else {
	if (isNil(fb->cp) || (fb->ecci == 0)) break;
	fb->hp = mcdr(fb->cp); /* go back in history from current candidate */
      }
      searchHist(fb);
      break;
    case 19: /* C-s search (down) */
      if (isModified(fb)) alpsAddHistory(fb);
      if (!isSearchDnMode(fb)) {
	setSearchDnMode(fb);
	if (isSearchUpMode(fb)) {
	  clrSearchUpMode(fb);
	} else {
	  fb->ecci = 0;
	  fb->cp   = pnil;
	}
	showSearch(fb);
      } else {
	if (isNil(fb->cp) || (fb->ecci == 0)) break;
	lex    = nrevList(lookup(a.v.hhs));
	fb->hp = mcdr(fb->cp); /* go back in history from current candidate */
	lex    = nrevList(lex);     /* return to normal order */
      }
      searchHist(fb);
      break;
    case 20: /* C-t transpose chars */
      setModified(fb);
      chkHistCandidate(fb);
      saveUndo(fb);
      if ((fb->ecci > 1) && (fb->ilpos > 0)) {
	if (fb->ilpos == fb->ecci) cursBack(fb,1);
	c                   = lbuf[fb->ilpos - 1];
	lbuf[fb->ilpos - 1] = lbuf[fb->ilpos];
	lbuf[fb->ilpos]     = c;
	cursBack(fb,1);
	showLine(fb,&lbuf[fb->ilpos],2);
      } 
      break;
    case 21: /* C-u kill line from bol to point */
      delChars(fb,0);
      break;
    case 22: /* C-v join prev line from history */
      insertLastLine(fb);
      break;
    case 23: /* C-w kill from mark to point */
      delChars(fb,fb->mlpos);
      break;
    case 24: /* C-x keycode replacement */
      fb->escMode = EM_DO_TR;
      break;
    case 25: /* C-y yank ybuf to point */
      chkHistCandidate(fb);
      insChars(fb,vadd(fb->ybuf).sp,fb->ylen);
      break;
    case 27:
      fb->escMode = (fb->escMode) ? EM_NORMAL : EM_ESCAPE;
      break;
    case 31: /* C-_ undo (not really that useful methinks) */ 
      if (!isHistMode(fb) && !searchMode(fb)) {
	memcpy(mbuf,lbuf,j=fb->ecci); /* temp for flip to ubuf */
	p1 = fb->ilpos;
	cursBolClr(fb);
	fb->ecci  = fb->ulen;
	memcpy(lbuf,vadd(fb->ubuf).sp,fb->ecci);    
	memcpy(vadd(fb->ubuf).sp,mbuf,fb->ulen=j); /*old line is undo*/    
	showLine(fb,lbuf,fb->ecci);
	cursBack(fb,fb->ecci-fb->ulpos);
	fb->ulpos = p1;
      }
      break;
    default: 
      if (c >= space) {
	if (searchMode(fb)) {
	  lbuf[fb->ecci++] = c;
	  setModified(fb);
	  showSearch(fb); 
	  searchHist(fb);
	} else {
	  chkHistCandidate(fb);
	  mbuf[0] = c;
	  insChars(fb,mbuf,1);
	}
      } else { 
	j = fb->ilpos;
	cursFwd(fb,fb->ecci-fb->ilpos); 
	alpsOutsn(alpsout,3,"DM7\n\r[JGot Char: ",itos((int)c),"8");
	cursBack(fb,fb->ecci-j);
	setNeedReset(fb);
      }
    } /* switch on input char */
  }
  if (!searchMode(fb)) {
    if (findPars(fb,fb->ilpos,&p1,&p2)) {
      highLight(fb,p1,1);
      highLight(fb,p2,1);
      setNeedReset(fb);
    }
  }
  fb->lchar = c;
  if (ibind < iblen) goto readsumore;
  alpsFlush(alpsout);
}

void alpsCloseTerm() {
  if (alpsin->isatty == 1)  tcsetattr(0,TCSADRAIN,&alpsin->ios);
}

/********* Prompt stuff ****************/

pointer alpsSetPrompt(pointer p) {
  if (!isaChr(p)) {
    alpsOuts(alpserr,"Encountered invalid prompt, prompt unchanged\n");
    return pnil;
  }
  setSym(a.v.hpr,p);
  return p;
}

void alpsPrompt(fibrec *in, fibrec *out) {
  pointer lex = lookup(a.v.hpr);
  integer len;
  ASSERT(isaChr(lex));
  len = nels(lex);
  if (len > 16) len = 15;
  alpsFlush(alpserr);  /* in case user should know stuff */
  alpsTab(out,0);      /* start on a new line            */
  alpsOutb(out,vadd(lex).sp,len);
  alpsFlush(out);
  in->ipos = out->olpos; /* set initial pos on terminal line */
  setHavePrompt(in);
}

/******************** History Section ****************/

 void alpsAddHistory(fibrec *fb) {
  pointer lex,olex,rex;
  integer len = fb->ecci;
  uchar  *buf = vadd(fb->ibuf).sp;
  clrModified(fb);
  if ((len == 0) || (*buf == ' ')) return; 
  lex = olex = lookup(a.v.hhs);
  while (!isNil(lex)) {
    rex = mcar(lex);
    if ((len == nels(rex)) && (0 == memcmp(buf,vadd(rex).sp,len))) {
      if (isEqq(lex,olex)) return;     // repeat of last input 
      setCdr(olex,mcdr(lex));          // unlink 
      setCdr(lex,lookup(a.v.hhs));     // old input becomes new history head
      setSym(a.v.hhs,lex);
      return;
    }
    olex = lex;
    lex  = mcdr(lex);
  }
  /* no precedent found...*/
  lex = mkChr(buf,len);
  protect(lex);
  rex = getfs(1);
  unprotect();
  setCar(rex,lex);
  setCdr(rex,lookup(a.v.hhs));
  setSym(a.v.hhs,rex);
}

void alpsReadHistory() {
  pointer lex,rex;
  pointer hist;
  int     fd,len,clen=0;
  fibrec *alpshist;
  pointer filhist,parm,nhist;
  char    buf[max_line_len];

  setSym(a.v.hhs,pnil); /* start out with no history in total innocence */
  setSym(a.v.hhf,pnil); 
  
  if (*mHist != '/') { /* prepend cwd to relative path */
    len = strlen(mHist)+2;
    if (!getcwd(buf,max_line_len-len)) alpsError("getcwd for history file\n");
    clen        = strlen(buf);
    buf[clen++] = '/';
  }
  strcpy(&buf[clen],mHist);

  nhist            = buildChr(buf,strlen(buf));
  parm             = mkParmBlock(2,nhist,buildChr("rw",2));
  filhist          = alpsOpenF(parm);
  alpshist         = getFil(filhist);
  protect(filhist);
  protect(hist = pnil);  /* not protecting anything, just reserving slot */
  while (!isNil(lex = alpsReadLine(alpshist))) {
    protect(lex);
    rex = getfs(1);
    unprotn(2);
    setCar(rex,lex);
    setCdr(rex,hist);
    hist = rex;
    protect(hist);
  }
  unprotn(2);  // hist & filhist
  ASSERT(protind==0);  /* no one should be calling me with stuff on protstk */
  setSym(a.v.hhs,hist);
  setSym(a.v.hhf,nhist); /* remember history file name */
  alpsCloseF(filhist);
}

void alpsWriteHistory() {
  pointer lex,olex,tex;
  int fd;
  struct stat sb;
  fibrec *alpshist;
  pointer filhist,nhist,parm;
  uchar buf[max_line_len];
  nhist = lookup(a.v.hhf);
  if (isNil(nhist)) return; /* fame is fleeting, oblivion lasts for ever */
  parm     = mkParmBlock(2,nhist,buildChr("w",1));
  filhist  = alpsOpenF(parm);
  if (isNil(filhist)) {
    alpsOutsln(alpserr,"Write of history file failed");
    return;
  }
  alpshist = getFil(filhist);
  fd       = alpshist->fd;
  fstat(fd,&sb);  /* Check for null device before truncate */
  if (sb.st_rdev == makedev(1,3)) {
    alpsCloseF(filhist);
    return;
  }
  if (0>ftruncate(fd,0L)) { /* in case of exegesis */
    alpsError("truncate failed on history file\n");
    alpsCloseF(filhist);
    return;
  }
  tex = olex = nrevList(lookup(a.v.hhs)); /* a savage reversal of history */
  while (!isNil(olex)) { /* reversed list headed by olex */
    lex = mcar(olex);
    alpsOutb(alpshist,vadd(lex).sp,nels(lex));
    alpsOutln(alpshist);
    olex = mcdr(olex);
  }
  nrevList(tex); /* restore history order */
  alpsCloseF(filhist);
}

void alpsInitTerm(fibrec *term, int no_alloc) {
  struct termios mTermio;
  struct winsize mWinsize;
  ioctl(term->fd, TIOCGWINSZ, &mWinsize);
  term->twidth  = mWinsize.ws_col;
  term->theight = mWinsize.ws_row;
  if (term->twidth <= 0) term->twidth=80;
  setNumval(a.v.hcw,term->twidth);  // Achtung: Patching workspaceInit values
  setNumval(a.v.hch,term->theight);
  term->isatty  = 1;
  term->escMode = EM_NORMAL;   
  term->ttyMode = 0;   
  term->ipos    = 0;   
  term->cp      = pnil;            /* history candidate */
  if (!no_alloc) term->ybuf = allocCV(max_line_len);    /* yank buffer  */
  term->ylen    = 0;
  term->mlpos   = -1;                       /* mark spot         */
  if (!no_alloc)term->ubuf = allocCV(max_line_len);   /* undo buffer   */
  term->ulen    = 0;
  term->ulpos   = -1;                       /* last ilpos        */
  tcgetattr(0,&mTermio);
  memcpy(&term->ios,&mTermio,sizeof(struct termios));
  mTermio.c_lflag    &= ~(ECHO | ICANON);
  //  mTermio.c_iflag    &= ~IXON;
  mTermio.c_cc[VMIN]  = 0;
  mTermio.c_cc[VTIME] = 0;
  tcsetattr(term->fd,TCSANOW,&mTermio);
}

void alpsWinchHandler(int sig) {
 struct winsize mWinsize;
 ioctl(0, TIOCGWINSZ, &mWinsize);
 alpsin->twidth  = mWinsize.ws_col;
 alpsin->theight = mWinsize.ws_row;
 setSym(a.v.hcw,mkNum(alpsin->twidth));
 setSym(a.v.hch,mkNum(alpsin->theight));
 setNeedHardReset(alpsin);
}
