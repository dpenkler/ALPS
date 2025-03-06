/* alps.h */
//#include <limits.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <dirent.h>
#include <termios.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <unistd.h>
#include <stdarg.h>

/* defines for HOST */
#define ANDROID   1
#define CYGWIN32  2
#define CYGWIN64  3
#define LINUX32   4
#define LINUX64   5
#define SOLARIS32 6
#define SOLARIS64 7
#define WEBOS     8
#define MACOS     9
#define OANDROID 10
#define ANDROID64 11
#define S390X     12
/* ascii strings for HOST defined in hosts[] in alps.c */

#ifndef HOST
#define HOST LINUX64
#endif

#if (HOST==ANDROID) || (HOST==CYGWIN32) || (HOST==LINUX32) || (HOST==WEBOS) ||\
    (HOST==OANDROID)
#define ALIGN              4
#define PROC              32    /* number of bits in an int or pointer  */
#elif (HOST==LINUX64) || (HOST==SOLARIS64) || (HOST==MACOS) || (HOST==CYGWIN64) || (HOST==ANDROID64) || (HOST==S390X)
#define ALIGN              8    /* memory alignment boundary in octets  */
#define PROC              64    /* number of bits in an int or pointer  */
#elif (HOST==SOLARIS32)
#define ALIGN              8    /* memory alignment boundary in octets  */
#define PROC              32   /* number of bits in an int or pointer  */
#endif
#if (HOST==MACOS)
#define MAP_ANONYMOUS MAP_ANON
#define NO_RT
#endif
#if (HOST==SOLARIS32) || (HOST==SOLARIS64) || (HOST==S390X)
#define ALPS_BIG_ENDIAN
#else
#undef ALPS_BIG_ENDIAN
#endif
#ifndef INT_MAX
#define INT_MAX 0x7ffffff
#endif

/*data sizes in computer addressable units bytes or words*/
#define numlen          sizeof(number)       /* a real number:double precision*/
#define cpxlen          numlen*2             /* a complex number              */
#define chrlen          1                    /* a string character:ASCII byte */
#define indlen          (PROC / 8)           /* INTEGER and pointer ILP model */
#define rg_len          (indlen * 2)         /* list node in fs space         */
#define env_rgohd       ((sizeof(envrecT) + rg_len - 1)/rg_len) /*in rg units */

/*  interpreter limitations */
/* Workspace size = (BIG*wsp_block*PROC)/1024 Megabytes.
   BIG=2 => 64MB on a 32 bit machine or 128MB on a 64 bit proc     */
#define BIG             2       /* Default value,  argv overrides   */   
#define max_line_len    1024    /* line length for input processing  */
#define max_files       32      /* maximum number of file descriptors */
#define max_base        16      /* largest printable numeric base      */
#define max_err_len     32      /* longest error message length         */
#define wsp_block       1024    /* workspace block size ints             */
#define wsp_bsize       2048L   /* workspace allocated in blocks          */
#define maxstacks       32      /* not a limit but used to calc wsp_size   */
#define hash_size       256     /* hash table or OBLIST (0 origin!)         */
#define num_env_bkts    7       /* number of buckets for returned envblks  */
#define PROF            10      /* profiler   timer ticks in microseconds */
#define TICKS           10      /* preemption timer ticks in milliseconds*/
#define PRINT_LENGTH    32767   /* default print length                 */
#define PRINT_DEPTH     1024    /* default print depth                 */
#define PRINT_LIST_LEN  1024    /* default max elts in list to print  */
#define MAX_PROT        16      /* maximum depth of protection stack */
#define BB_PCDEPTH      32      /* black box pc ring depth          */
#define BB_TCBDEPTH     8       /* depth black box tcp disp ring   */
#define NUM_ROOT_VARS   36      /* size of roottable in integers  */
#define WSP_BKSIZE      2048    /* size of wspc file buffer ints */
#define MAX_CS          256     /* nuber of character symbols   */
#define MAX_CC          257     /* number of char constants+1  */
/*  the constants false, true, zero, one and pi */
#define false 0
#define true  1
#define idadd           0e0
#define idprod          1e0
#define pi     (double) 3.141592653589793238462643383279
/* Max findBest loop count */
#define Max_FB_Loop 8

/* number of apl contants of different types */
#define apl_ncon        45  /* numerics   */
#define apl_ccon        2   /* characters */
#define apl_wcon        21 /* words these are pointers */
#define apl_consts      (apl_ncon + apl_ccon + apl_wcon)

/* These would be static const char but stupid gcc considers constants in
   switch statement case labels as not being in the spirit of 'C' */
#define shriek    '!'
#define alps_quad '#'
#define quote     '\''
#define zero      '0'
#define space     ' '
#define excape    '\\'
#define comment   ';'
#define colon     ':'
#define comma     ','
#define dollar    '$'
#define percent   '%'
#define ampersand '&'
#define lcbrc     '{' /* left  curly  brace */
#define rcbrc     '}' /* right curly  brace */
#define lsbrc     '[' /* left  square brace */
#define rsbrc     ']' /* right square brace */
#define lpar      '('
#define rpar      ')'
#define point     '.'
#define dexp      'e' /* exponent */
#define plus      '+'
#define neg       '-'
#define qquot     '`' /* back quote */
#define dquot     '"'
#define eol       '\n'
#define star      '*'
#define slash     '/'
#define less      '<'
#define eqval     '='
#define great     '>'
#define quest     '?'
#define spiral    '@'
#define hat       '^'
#define uscore    '_'
#define bar       '|'
#define tilde     '~'
#define complex _Complex

/* Basic type abstractions */
typedef double          number;
typedef complex double  cnum;
typedef long            integer;
typedef unsigned long   whole;
typedef unsigned int    uint;
typedef unsigned char   uchar;
typedef int             bool;
typedef uchar           idstr[max_line_len];

#if (HOST==OANDROID)
inline static double log3(double n) {return(log(n)/log(2.0));}
#endif

#if  (HOST==OANDROID) || (HOST==SOLARIS32) || (HOST==SOLARIS64)
#define COMPLEX         false  
#define COMPCAP ""
typedef double complex;
#define I 0
#define cabs  abs
#define csqrt sqrt
#define cimag(X) X 
#define creal(X) X 
#define cpow pow
#define clog log
#else
#define COMPLEX         true    /* Support for complex numbers          */
#include <complex.h>
#define COMPCAP "j"
#if (HOST==ANDROID) || (HOST==ANDROID64)
cnum clog(cnum);
cnum cpow(cnum,cnum);
cnum catan(cnum);
cnum cacos(cnum);
cnum casin(cnum);
cnum catanh(cnum);
cnum cacosh(cnum);
cnum casinh(cnum);
#endif
#endif

/* Types of basic objects */
#define FBits 8 /* number of flag bits */
/* Flags in 1st word of allocated memory of type for disposition */
typedef enum {mrk = 1,  /* MM marking & parm reservations                */
              act = 2,  /* used for active env chain                     */
              rvd = 4,  /* used for captured env chain                   */
              hld = 8   /* used for parm block held by mapfn             */
} vflags;


/* type code shifted right FBits to reveal type, 1st class citizens */
typedef enum {unk  =  0, /* unknown soldier aka anon                */
	      num  =  1, /* numeric   constant  scalar or array     */
	      cpx  =  2, /* complex   constant  scalar or array     */
	      chr  =  3, /* character constant  scalar or array     */
	      ref  =  4, /* pointer   constant  scalar or array     */
              env  =  5, /* env block                               */
              stk  =  6, /* a stack                                 */
	      cnt  =  7, /* continuation block                      */
              tcb  =  8, /* task control block                      */
              fob  =  9, /* foreign object                          */
	      cym  = 10, /* constant symbol                         */
	      sym  = 11, /* a symbol                                */
	      /* type codes below do not appear in identity tag     */
              lst  = 12, /* list, not a type tag just a marker      */
              sxp  = 13, /* list, not a type tag just a marker      */
	      cnm  = 14, /* num or cpx scalar or array              */
	      nc   = 15, /* num or chr scalar or array              */
	      ncr  = 16, /* num, chr, ref or sym scalar or array    */
	      ncra = 17, /* num, chr, ref or sym array              */
	      sclr = 18, /* numeric scalar                          */
	      nsgl = 19, /* numeric singleton                       */
	      axn  = 20, /* numeric singleton or nil                */
	      nsv  = 21, /* numeric singleton or vector             */
	      nsa  = 22, /* Not Used                                */
	      nv   = 23, /* numeric vector                          */
	      na   = 24, /* numeric array                           */
	      csgl = 25, /* character singleton                     */
	      cv   = 26, /* character vector                        */
	      nscv = 27, /* Num single or char vector               */
	      pdn  = 28, /* primitive dyadic numop                  */
	      pdo  = 29, /* primitive dyadic op                     */
	      fun  = 30, /* not constant                            */
	      cns  = 31, /* cons cell                               */
	      flk  = 32, /* file like object   fil or buf           */
	      fil  = 33, /* file object                             */
              cvf  = 34, /* character vector or file object         */
	      buf  = 35, /* buffer object                           */
	      ins  = 36, /* instrument object                       */
	      wgt  = 37, /* widget                                  */
	      any  = 38  /* this at the end                         */
} vtype; /* never ever re-order! assert(lst > fob); */
         /* assumes address of cons elements >= lst. See isList()   */

typedef union pointerT {
  integer          it;
  integer         *ip;
  whole            wh;
  whole           *wp;
  uchar           *sp;
  number          *np;
  cnum            *cp;
  struct blk_hd   *bk;
  struct reg_blk  *rg;
  struct symrec   *at;
  struct vrec     *vr;
  struct envrec   *ev;
  struct cnt      *ct;
  struct fobrec   *fb;
  struct fibrec   *fi;
  union  pointerT *pt;
  struct pcb      *bb;
  struct tcb      *tb;
  void            *vd;
} pointer;

/* the cons block only resident of FS space */
typedef struct reg_blk {
  pointer ar;   /* Contains Address   Register */
  pointer dr;   /* Contains Decrement Register */
} reg_blk;

typedef struct envrec {  /* environment binding block */
  whole   idntt;   /* type is env                                       */
  integer nargs;   /* number of args; (size(envrec) == (nargs+2)*rg_len)*/
#if (DEBUG==true)
  integer   owner; /* debug         */
  whole     serno; /* serial number */
#endif
  pointer nenv;    /* next env; used for env chain and free list        */
  pointer argp;    /* pointer into args; aka dest                       */
  /* Must be aligned to a reg_blk boundary => even num words above here!*/
  reg_blk args[];  /* array of length nargs; symbol in car, val in cdr  */
} envrecT;

/* alps' generic callback taking a universal pointer argument */
typedef void     (*procTp)(pointer);
typedef void     (*fcbT)(int);
/* file handling callback array */
typedef void     (*fcbs[FD_SETSIZE])(int);

/* types of foreign objects (yes these are second class citizens) */
typedef enum {filt = 1, buft = 2, inst = 3, wdgt = 4} fobtp;

typedef struct fobrec {  /******* foreign object record structure *******/
  whole   idntt;   /* type is fob                                       */
  pointer next;    /* next  fobrec on fob_busy list                     */
  integer type;    /* type of foreign object as per fobtp               */
  pointer name;    /* Print string of foreign object                    */
  pointer fobpvt;  /* pointer to fob's private space                    */
  pointer data1;   /* pointer to data or one word for storage           */
  pointer data2;   /* an extra pointer for private purposes             */
  integer fobsize; /* Size of fob's private space in bytes              */
  /* start of fob's private space aligned to next byte after fobsize    */
} fobrec;

/* Foreign Object types and friends go here */

/* no struct for inst since all his data lives in fobrec */
 
 /* no struct widget  since all her data lives in fobrec */


/* fob for files */
typedef enum {finp,  /* read only file  */
	      fout,  /* write only file */
	      fio,   /* read-write file */
	      fpass, /* passive socket  */
	      fclos  /* closed file     */
} fstatT;

typedef enum {ldvs_ok, ldvs_eof, ldvs_nodata} bufstatT;

  /* this is a fob of type fil pointed to by its fobpvt */ 

typedef struct fibrec {/* file information block record ;-) */
  pointer   fob;       /* pointer to parent fobrec          */
  integer   fd;        /* file descriptor                   */
  fcbT      readcb;    /* input callback                    */
  fcbT      exceptcb;  /* oob callback                      */
  fstatT    fdstat;    /* file status                       */
  bufstatT  bufstat;   /* buffer status                     */
  pointer   ibuf;      /* input buffer vrec valid:finp fio  */
  integer   ifpos;     /* current input position            */
  integer   ilpos;     /* current line position in inp line */
  integer   ecci;      /* end of current input index        */
  pointer   obuf;      /* output buffer vrec valid: fout fio*/
  integer   olpos;     /* current line position in out line */
  integer   ecco;      /* end of current output index       */
  idstr     aline;     /* token char accumulation buffer    */
  char      pbb[8];    /* putback buffer                    */
  integer   pbbi;      /* pbb index                         */
  integer   alen;      /* input element accumulated length  */
  integer   qqdepth;   /* quasiquote depth                  */
  integer   isatty;    /* flag true => itsatty              */
  struct termios ios;  /* terminal control settings         */
  pointer   hp;        /* current history position in list  */
  pointer   cp;        /* search candidate position in list */
  integer   escMode;   /* escape sequence state variable    */
  integer   ttyMode;   /* used for raw and history mode     */
  integer   ipos;      /* initial position on terminal line */
  integer   lchar;     /* last char (for double semantics)  */
  pointer   ybuf;      /* yank buffer                       */
  integer   ylen;      /* length of yank string             */
  integer   mlpos;     /* mark position (-1 if unmarked)    */
  pointer   ubuf;      /* undo buffer                       */
  integer   ulen;      /* length of undo buf                */
  integer   ulpos;     /* last line position for undo       */
  integer   twidth;    /* terminal width                    */
  integer   theight;   /* terminal height                   */
  /* number parsing state */
  integer   nstart;    /* start index of number in aline    */
  integer   nind;      /* index into ndigs                  */
  integer   ndigs[3];  /* digits before, after point, exp   */
  int       hadpoint;  /* index of first digit after point  */
  int       hadexp;    /* index of first digit after exp    */ 
  int       radix;     /* radix for conversion              */
  bool      hadneg;    /* negative mantissa                 */
  bool      hadeneg;   /* negative exponent                 */
  bool      hadcpx;    /* seen complex prefix (j)           */
} fibrec;

typedef union ctable {
  pointer h[apl_consts];
  struct {
    pointer
    /*-------1----2----3----4----5----6----7----8----9---10---------*/
    /*num*/ hib, hob, hpp, hpw, hfw, hsd, hep, hvp, hwn, hnf, /* 00 */
    /*num*/ hcs, hcr, hpm, hpn, hls, hrl, hio, hct, htr, hai, /* 10 */
    /*num*/ hmi, hmn, hmx, hnn, hra, hdm, hcc, hop, hof, hvn, /* 20 */
    /*num*/ hpl, hpd, hll, hdp, hdd, hdl, hgc, hfm, hfs, hcw, /* 30 */
    /*num*/ hch, hgw, hgh, hfn, hwd,                          /* 40 */
    /*chr*/                          hgs, hcp,                /* 40 */
    /*ref*/                                    hdx, hrf, hpf, /* 40 */
    /*ref*/ hlf, hhf, hrs, hex, hlx, hnm, hws, hsw, hhs, hpr, /* 50 */
    /*ref*/ hlp, hlh, hqq, hkb, hpc, hgb, hdb, hal;           /* 60 */
    /*-------1----2----3----4----5----6----7----8----9---10---------*/
  } v;
} ctable;

/* The error list */
typedef enum {
  no_err,     no_room,    stk_ovfl,  unb_sym,    inv_sym,  inv_exp,  inv_fun,
  inv_numarg, inv_funarg, dom_range, sys_cor,    con_ass,  not_imp,  bad_num,
  bad_str,    bad_vec,    no_files,  inv_funame, ios_err,  bad_file, sys_par,
  pgmr_inc,   user_err,   user_int,  sys_int,    last_err
} errors;

/* alps.c =*/
extern ctable a;
extern fibrec *alpsin, *alpsout, *alpserr;
extern uchar specCS[],whiteCS[],digitCS[],alphaCS[],symStCS[],wordCS[];
extern const char *mHist;
extern pointer chartab[];
extern fibrec *fparr[];
extern pointer pnil,ptru;
extern integer protind;
extern pointer protstk[];
extern number  iround;
extern pointer allocCV(integer);
extern pointer allocFil(pointer);
extern pointer allocFob(fobtp,   pointer, pointer, pointer, integer);
extern void    alpsAddHistory(fibrec *);
extern void    alpsBlockSignals();
extern pointer alpsCloseF(pointer);
extern void    alpsCloseTerm();
extern void    alpsCrash(const char *, const char *, uint, const char *);
extern void    alpsError(const char *);
extern void    alpsFlush(fibrec *);
extern void    alpsMsg(int n, ...);
extern pointer alpsOpenF(pointer);
extern void    alpsOutb(fibrec *, uchar *, integer);
extern void    alpsOutc(fibrec *,int);
extern void    alpsOutln(fibrec *);
extern void    alpsOutn(fibrec *,int, int);
extern void    alpsOuts(fibrec *, const char *);
extern void    alpsOutsln(fibrec *, char *);
extern void    alpsOutsn(fibrec *,int , ...);
extern void    alpsPrompt(fibrec *, fibrec *);
extern void    alpsReadHistory();
extern pointer alpsReadLine(fibrec *);
extern pointer alpsSetPrompt(pointer);                           /* term.c */
extern void    alpsTab(fibrec *, integer);
extern void    alpsUnblockSignals();
extern void    alpsWriteHistory();
extern char   *asc(char*, integer *, double, integer, integer,   /* format.c */
		   bool *, integer *, integer *, bool *,
		   integer *, bool, integer, integer, integer);
extern pointer assoc(pointer, pointer); 
extern pointer buildChr(const char *, integer);
extern void    bumpRasc(int);
extern pointer cons(pointer, pointer);
extern pointer cvtNum(fibrec *, int);
extern pointer cvtCpx(fibrec *, int, pointer);
extern integer dims(pointer);
extern void    error(errors ,char *);
extern uchar  *fmtnum(uchar *, integer *, double, integer,       /* format.c */
		      integer, integer, integer, bool,
		      integer, integer);
extern integer format(integer, pointer, uchar *);                /* format.c */
extern integer getDim(pointer, integer);
extern fibrec *getFil(pointer);
extern pointer getFname(pointer);
extern pointer getfs(integer);
extern number  getNumVal(pointer);
extern number *getNumVec(pointer);
extern integer getTcbTid(pointer);
extern pointer getVal(pointer);
extern uchar  *itos(integer);                                    /* format.c */
extern uchar  *itosn(integer,integer);                           /* format.c */
extern uchar  *itox(whole);                                      /* format.c */
extern integer listLen(pointer);
extern pointer lookup(pointer);
extern pointer mcar(pointer);
extern pointer mcdr(pointer);
extern pointer mcadr(pointer);
extern pointer mkChr(const uchar *, integer);
extern pointer mkCpx(const number, const number);
extern char   *mkEmes(int n, ...);                               /* format.c */
extern char   *mkFmes(char *, fibrec *);                         /* format.c */
extern pointer mkNum(const number);
extern pointer mkParmBlock(int n, ...);
extern uchar  *mkString(pointer, idstr, char *);
extern integer nels(pointer);
extern pointer nrevList(pointer);
extern pointer oblis_head(const uchar *, integer);
extern void    protect(pointer);
extern uchar  *rtos(number, integer, integer);                   /* format.c */
extern uchar  *rtot(number);                                     /* format.c */
extern void    scheduleIOCB(pointer, pointer, int);
extern pointer setCar(pointer,pointer);
extern pointer setCdr(pointer,pointer);
extern void    setNumval(pointer, number);
extern void    setSym(pointer, pointer);
extern void    setVal(pointer, pointer);
extern char   *specToString(pointer);
extern bool    stoi(char *, whole *);                            /* format.c */
extern char   *stox(char *, integer);                            /* format.c */
extern char   *subs(char *, integer);                            /* format.c */
extern integer strcompare(char *, integer, pointer);
extern pointer unprotect();
extern void    unprotn(int n);
extern void    unprotall(void);
extern pointer vadd(pointer);
extern int     widMarker(pointer);
/* term.c */
extern void    alpsTermReader(int);
extern void    clrModified(fibrec *);
extern void    clrRawMode(fibrec *);
extern void    setRawMode(fibrec *);
extern void    setHavePrompt(fibrec *);
extern void    setNeedReset(fibrec *);
extern void    setNeedHardReset(fibrec *);

/* rascal.c */
extern int rascal(double  d,   int   base, int  sd,   int prec,
		  int    *sgn, uchar *buf,  int *bpnt, int fexp);
extern bool asctor(idstr buf, integer len, integer radix, integer start,
		   integer bpdigs, integer apdigs, integer expdigs,
		   bool hadneg, integer hadpoint, bool hadeneg, integer hadexp,
		   double * dres);

/* sound.c */
extern pointer alloc_sb(int, int);

/* common */
#define BIT_MAP(var,size)  uchar var[(size + 7) / 8 + 1]
#define BMZERO(var,size)   memset(var,0,(size + 7) / 8 + 1)
#define BMSETP(var,index)  (var[index >> 3]  &  (1 << (index & 7)))
#define BMCLRP(var,index) !(var[index >> 3]  &  (1 << (index & 7)))
#define BMSET(var,index)   (var[index >> 3] |=  (1 << (index & 7)))
#define BMCLR(var,index)   (var[index >> 3] &= ~(1 << (index & 7)))

#if (DEBUG==true)
static bool CKIND(int c) {
  if ((c >=0) && (c < 256)) return 1;
  alpsOutsn(alpserr,3,"Warning: bad char index ",itos(c),"\n");
  return 0;
}
#else
#define CKIND(c) (((c) >=0) && ((c) < 256))
#endif

#if (RANGE==true)
#define ASSERT(X) ((X)?(void)0 : alpsCrash(#X, __FILE__, __LINE__, __func__))
#else
#define ASSERT(X)  /* Note: no side effects when turned off! */
#endif

#if (HOST==SOLARIS32) || (HOST==SOLARIS64)
#include <ieeefp.h>
int alpsIsNaN  (double x) { return isnand(x); }
int alpsIsInf  (double x) { return !finite(x); }
#else
#define alpsIsNaN(x) isnan(x)
#define alpsIsInf(x) isinf(x)
#endif

inline static int min(integer a, integer b) {return a < b ?a : b;}
inline static int max(integer a, integer b) {return a < b ?b : a;}

/* CharSet predicates */
inline static bool isSpec (int c) {return CKIND(c) ? BMSETP(specCS,c)  : 0;}
inline static bool isWhite(int c) {return CKIND(c) ? BMSETP(whiteCS,c) : 0;}
inline static bool isDigit(int c) {return (c >= '0' && c <= '9');}
inline static bool isAlpha(int c) {return CKIND(c) ? BMSETP(alphaCS,c) : 0;}
inline static bool isSymSt(int c) {return CKIND(c) ? BMSETP(symStCS,c) : 0;}
inline static bool isWord (int c) {return CKIND(c) ? BMSETP(wordCS,c)  : 0;}
 
inline static bool isEqq  (pointer i, pointer j) {return(i.pt == j.pt);}
inline static bool isNil  (pointer i) { return(i.pt == pnil.pt); }
inline static bool isTru  (pointer i) { return(i.pt == ptru.pt); }

static const unsigned char numdig[16] = {"0123456789ABCDEF"};

/* Environment block field handling: getters & setters */
inline static pointer nenv(pointer lex) { return lex.ev->nenv; }
inline static pointer snenv(pointer tex, pointer lex) { /* set next env */
  return (tex.ev->nenv = lex); }
inline static integer nargs(pointer lex) { return lex.ev->nargs; }
inline static integer warg(pointer lex) { /* which arg 0->n-1 */
  return (((lex.ev->argp.wh - indlen) -  lex.wh)/rg_len - env_rgohd); }
inline static pointer arg1(pointer lex) {return lex.ev->args[0].dr;}
inline static pointer arg2(pointer lex) {return lex.ev->args[1].dr;}
inline static pointer arg3(pointer lex) {return lex.ev->args[2].dr;}
inline static pointer arg4(pointer lex) {return lex.ev->args[3].dr;}
/* watch out argn has origin at ZERO */
static pointer argn(integer i, pointer lex) {return lex.ev->args[i].dr;}
static pointer cargn(integer i, pointer lex) {
  return (i < nargs(lex)) ? lex.ev->args[i].dr : pnil;}
static pointer symn(integer i, pointer lex) {return lex.ev->args[i].ar;}
inline static void ssym1(pointer lex, pointer rex) {lex.ev->args[0].ar = rex;}
inline static void ssymn(pointer lex, integer n, pointer rex) {
  lex.ev->args[n].ar = rex;}
inline static void sarg1(pointer lex, pointer rex) {lex.ev->args[0].dr = rex;}
inline static void sarg2(pointer lex, pointer rex) {lex.ev->args[1].dr = rex;}
inline static void sargn(pointer lex, integer n, pointer rex) {
  lex.ev->args[n].dr = rex;}

/* basic type predicates */
inline static bool isNum  (pointer i) {return((i.pt->wh>>FBits) == num);}
inline static bool isCpx  (pointer i) {return((i.pt->wh>>FBits) == cpx);}
inline static bool isChr  (pointer i) {return((i.pt->wh>>FBits) == chr);}
inline static bool isRef  (pointer i) {return((i.pt->wh>>FBits) == ref);}
inline static bool isNC   (pointer i) {return((i.pt->wh>>FBits) < ref);}
/*                   NC => num,cpx, or char */
inline static bool isNCR  (pointer i) {return((i.pt->wh>>FBits) <= ref);}
/*                   NCR => num,cpx, char or ref */
inline static bool isEnv  (pointer i) {return((i.pt->wh>>FBits) == env);}
inline static bool isStk  (pointer i) {return((i.pt->wh>>FBits) == stk);}
inline static bool isCnt  (pointer i) {return((i.pt->wh>>FBits) == cnt);}
inline static bool isTcb  (pointer i) {return((i.pt->wh>>FBits) == tcb);}
inline static bool isFob  (pointer i) {return((i.pt->wh>>FBits) == fob);}
inline static bool isConst(pointer i) {return((i.pt->wh>>FBits) <= cym);}
inline static bool isAtom (pointer i) {return((i.pt->wh>>FBits) <= sym);}
inline static bool isCym  (pointer i) {return((i.pt->wh>>FBits) == cym);}
inline static bool isSym  (pointer i) {return((i.pt->wh>>FBits) == sym);}
inline static bool isCons (pointer i) {return((i.pt->wh>>FBits) >= lst);}
inline static bool isList (pointer i) {return(isCons(i) || isNil(i));}

/* Foreign object type predicates */
inline static bool isFil(pointer i)    {return (isFob(i)&&i.fb->type == filt);}
inline static bool isBuf(pointer i)    {return (isFob(i)&&i.fb->type == buft);}
inline static bool isInst(pointer i)   {return (isFob(i)&&i.fb->type == inst);}
inline static bool isWidget(pointer i) {return (isFob(i)&&i.fb->type == wdgt);}
inline static bool isaFil(pointer i)   {return (isFil(i) || isBuf(i));}

inline static bool isAnon (pointer i) {return(*i.wp == 0);}
inline static bool isaChr(pointer i) {return(isChr(i) || isSym(i) || isCym(i));}
inline static bool isaNum(pointer i) {return(isNum(i) || isCpx(i));}

/* basic token  predicates */
inline static bool isColon(pointer i) {return(i.pt == chartab[colon].pt);}
inline static bool isComma(pointer i) {return(i.pt == chartab[comma].pt);}
inline static bool isDash (pointer i) {return(i.pt == chartab[neg  ].pt);}
inline static bool isDot  (pointer i) {return(i.pt == chartab[point].pt);}
inline static bool isLpar (pointer i) {return(i.pt == chartab[lpar ].pt);}
inline static bool isRcbrc(pointer i) {return(i.pt == chartab[rcbrc].pt);}
inline static bool isRpar (pointer i) {return(i.pt == chartab[rpar ].pt);}
inline static bool isRsbrc(pointer i) {return(i.pt == chartab[rsbrc].pt);}

inline static whole getWhole(pointer lex) {
  ASSERT(isNum(lex));
  return ((whole)fabs(vadd(lex).np[0]));
}
inline static integer getInt(pointer lex) {
  ASSERT(isNum(lex));
  return ((integer)(vadd(lex).np[0]));
}
inline static integer getIntn(pointer lex, integer n) {
  ASSERT(isNum(lex));
  return ((integer)(vadd(lex).np[n]));
}
inline static number getNum(pointer lex) {
  ASSERT(isNum(lex));
  return (vadd(lex).np[0]);
}
/* FS field handling */
/* Alle gute Dinge sind Drei */
inline static pointer car(pointer arg)   { return (arg.rg->ar); }
inline static pointer cdr(pointer arg)   { return (arg.rg->dr); }

inline static pointer caar(pointer arg)  { return (arg.rg->ar.rg->ar); }
inline static pointer cadr(pointer arg)  { return (arg.rg->dr.rg->ar); }
inline static pointer cdar(pointer arg)  { return (arg.rg->ar.rg->dr); }
inline static pointer cddr(pointer arg)  { return (arg.rg->dr.rg->dr); }

inline static pointer caaar(pointer arg) { return (arg.rg->ar.rg->ar.rg->ar); }
inline static pointer caadr(pointer arg) { return (arg.rg->dr.rg->ar.rg->ar); }
inline static pointer cadar(pointer arg) { return (arg.rg->ar.rg->dr.rg->ar); }
inline static pointer caddr(pointer arg) { return (arg.rg->dr.rg->dr.rg->ar); }
inline static pointer cdaar(pointer arg) { return (arg.rg->ar.rg->ar.rg->dr); }
inline static pointer cdadr(pointer arg) { return (arg.rg->dr.rg->ar.rg->dr); }
inline static pointer cddar(pointer arg) { return (arg.rg->ar.rg->dr.rg->dr); }
inline static pointer cdddr(pointer arg) { return (arg.rg->dr.rg->dr.rg->dr); }

