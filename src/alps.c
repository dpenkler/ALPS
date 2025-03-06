/*                           VERSION  -8.36-                                */
#define REV          "Time-stamp: <2024-10-08 18:19:32 dave>"
#include <signal.h>
#include <time.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <sys/mman.h>
#include <errno.h>
#include <float.h>
#include <getopt.h>
#include <sys/stat.h>
#include <pwd.h>
#include <grp.h>
#include <setjmp.h>

/* Features supported */
#define EXP             true    /* include experimental code if any         */
/*  define DEBUG   in environment    internal debug output                  */
/*  define GRAF    in environment    enable graphics integration with EZWGL */
/*  define INSTCON in environment    enable gpib instrument control         */
/*  define SOUND   in environment    support for play & record              */
/*  define STATS   in environment    internal profiling support             */
/*  define SOCKET  in environment    enable networking support              */
#define RANGE           !FAST   /* range checking                           */
#define WSPSAV          !DEBUG  /* generate save_wspc code                  */
#define PREEMPT         !(FAST || DEBUG)   /* pre-emptive dispatcher        */
#define TIMER           true    /* timer interrupt for preempt and watchdog */
#define CKARGS          !FAST   /* check subr args                          */
#define TDEBUG          !FAST   /* for trace and debugr                     */
#define TAIL            FAST    /* experimental tail recursion removal      */
/* MMDBUG levels */
#define None        0
#define Circumspect 1
#define Low         2
#define High        3
#define MMDBUG      None
 
#define FORWARD      /* We have forward declarations here my dear Blaise */
#define ENVONLY  1
#define BINDONLY 2
#define ENVNBIND 3
#define TASKONLY 4
#define SYSTRACE 0
#define QND        /* Random number generator quick normal distribution */

#define SHALLOW 1
#define DEEP    2
#define DEEPC   3
#define BINDING SHALLOW

#include "alps.h"

/* The stage is set. Enter the poor player who struts and frets his hour.*/

#if (HOST==SOLARIS32) || (HOST==SOLARIS64)
#define MYCLOCK_ID CLOCK_REALTIME
#else
#define MYCLOCK_ID CLOCK_PROCESS_CPUTIME_ID
#endif

#if (EXP)
#define  EXPCAP "E"
#else
#define  EXPCAP ""
#endif

#if (TAIL)
#define  TAILCAP "t"
#else
#define  TAILCAP ""
#endif

#if (DEBUG)
#define DEBUGCAP "B"
#else
#define DEBUGCAP ""
#endif

#if (GRAF)
#define GRAFCAP "G"
#include "graf.h"
#else
#define GRAFCAP ""
static int gflag=0;
#endif

#if (SOUND)
#define SNDCAP "S"
#include "sound.h"
#else
#define SNDCAP ""
#endif

#if (INSTCON)
#include "inst.h"
#define USBTMC true
#include <sys/sysmacros.h>
#define INSTCAP "I"
#else
#define INSTCAP ""
#endif

#if (WSPSAV)
#define WSAVCAP "W"
#else
#define WSAVCAP ""
#endif

#if (STATS)
#define STATCAP "T"
#define INCCT(A,B) incCounter(A,B)
#else
#define STATCAP ""
#define INCCT(A,B)
#endif

#if (PREEMPT)
#define PRECAP "P"
#else
#define PRECAP ""
#endif

#if (TIMER)
#define TIMCAP "M"
#else
#define TIMCAP ""
#endif

#if (SOCKET)
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <netdb.h>
#define NETCAP "N"
#else
#define NETCAP ""
#endif

#if (TDEBUG)
#define TDBGCAP "D"
#else
#define TDBGCAP ""
#endif

#if (CKARGS)
#define CKARGSCAP "C"
#else
#define CKARGSCAP ""
#endif

#if (RANGE)
#define RANGECAP "R"
#else
#define RANGECAP ""
#endif

#if (BINDING==SHALLOW)
#define BINDCAP "s"
#elif (BINDING==DEEP)
#define BINDCAP "d"
#elif (BINDING==DEEPC)
#define BINDCAP "c"
#else 
#define BINDCAP ""
#endif

#define CAPSTRING EXPCAP GRAFCAP SNDCAP INSTCAP WSAVCAP STATCAP PRECAP TIMCAP \
  NETCAP TDBGCAP CKARGSCAP DEBUGCAP RANGECAP BINDCAP TAILCAP COMPCAP 

/* USER INTERFACE TO INTERPRETER */

const char *save_file   = "APLSAV";
const char *in_file     = "/dev/tty";
const char *out_file    = "/dev/tty";
const char *snd_file    = "/dev/dsp";
const char *hist_file   = ".alps_history";
const char *load_file   = ".alps";
const char *alps_suffix = ".al";
char *def_expr = "(let()(prompt)(a #EX(read))(rclr)(a #RS(eval #EX))"
  "(cond((or (nlistp #EX)(ne(car #EX)'a))(print #RS))))";

/* Internal implementation specific constants */

/*data sizes in computer addressable units bytes or words*/
#define alcbk_ohd       (indlen * 2)         /* allocated block sys ohd       */
#define vrec_ohd        (indlen * 4)         /* vrec block length no padding! */
#define symrec_ohd      sizeof(struct symrec)/* atom record length            */
#define pcbsize         (indlen * 7)         /* partition control block size  */
#define init_bsize      8                    /* initial block len in words    */

#define prim_dyadics    23 /* from s_dist (incl) to s_roll (not incl) */

/***** TYPE DECLARATION SECTION ******/

typedef enum { /* primitive and auxiliary case entries in bigEval */
  s_nop,   s_cond,    s_fun,
  s_defun, s_defexpr, s_defmacro, s_defclos,  s_defconst,
  s_apply, s_eval,    s_lambda,   s_label,    s_cont,     s_fnarg, 
  s_fexpr, s_macro,
  s_let,   s_letstar, s_prog,     s_progstar, s_return,   s_go,
  s_if,    s_when,    s_unless,   s_while,    s_repeat,  
  s_catch, s_throw,   s_uwdprot,  s_condcase, s_error,
  s_cwcc,  s_peval,   s_getpri,   s_setpri,   s_send,
  /* Mapping functions */
  s_mapc, s_map,   s_mapcan, s_mapn,  s_mapcar, s_maplist,
  s_gen,  s_genc,  s_ngen,   s_ngenc,   // s_*gen* must be in this order
  s_find, s_findc, s_sort,
  /* Interpreter Control */
  s_break,   s_wait,    s_thaw,    s_freeze,
  s_dbg,     s_trace,   s_kill,
  s_load,    s_require, s_save,    s_restore,
  s_gc,      s_quit,    s_bye,
  /* Lisp monadic predicates */
  s_atom,    s_consp,   s_listp,   s_nlistp,  s_null,    s_chrp,    s_nump, 
  s_cpxp,    s_refp,    s_symp,    s_primp,   s_boundp,  s_zerop,   s_onep,
  s_oddp,    s_evenp,   
  /* Lisp dyadic predicates */
  s_lt,      s_le,      s_eq,      s_ne,      s_gt,      s_ge,      s_eql,
  s_neql,    s_equal,   s_nequal,  s_memq,    s_member,  s_subset,
  /* Lisp logical connectives */
  s_not,     s_and,     s_or,      s_nand,    s_nor,     
  /* nasssty list manipulators */
  s_delete,  s_rplaca,  s_rplacd,  s_nconc,   s_nrev,  
  /* List accessors */
  s_car,     s_cdr,
  s_caar,    s_cadr,    s_cdar,    s_cddr,  
  s_caaar,   s_caadr,   s_cadar,   s_caddr,   s_cdaar,   s_cdadr,   s_cddar,
  s_cdddr,   s_nth,     s_nthcdr,  s_last, 
  /* List constructors */
  s_cons,    s_acons,   s_list,    s_liststar,s_append,  s_pairlis,
  /* Symbol manipulation and generation */
  s_qquot,   s_quote,   s_gensym,  s_oblis,   s_clear,
  s_spec,    s_setq ,   s_setqq,
  s_set,     s_pop,     s_push,    s_put,     s_get,    s_remprop,  s_intern,
  /* List processing */
  s_subst,   s_revl,    s_rotl,
  /* List query primitives */
  s_len,     s_pos,     s_assoc,  
  /* Data conversion */
  s_num,     s_chr,     s_implod,  s_explod,
  /* arithmetic convenience */
  s_incr  ,  s_decr  ,  s_sqrt,   s_modexp, s_int,      s_frc,     s_isint, 
  /* Here beginneth the primitive dyadic numOps, dist beeth the first */
  s_dist ,  s_plus  ,  s_minus ,  s_times ,  s_divid ,  s_ceil ,   s_floor ,
  /* APL logical connectives */
  s_andd  ,  s_xor   ,  s_vor   ,  s_nandd ,  s_nord ,
  /* non varyadic numops */
  s_resid ,  s_power ,  s_log   ,  s_trig  ,  s_binom ,
  /* Here endeth the NumOps, binom beeth the last */
  /* APL dyadic predicates, assume 0 when invoked monadically */
  s_great ,   s_greatq,  s_lessn ,  s_lessq , s_eqapl ,  s_neapl ,
  /* Other APL primitives */
  s_roll  ,  s_nott  ,
  s_shape ,  s_ravel ,  s_cat   ,  s_gup   ,  s_gdn   ,  s_iota  ,  s_rev   ,
  s_transp,  s_minv  ,  s_exec  ,  s_fmt   ,  s_komp  ,  s_expnd ,  s_take  ,
  s_drop  ,  s_rot   ,  s_shft  ,  s_decod ,  s_encod ,  s_elmnt ,  s_index ,
  s_reduc ,  s_scan  ,  s_inrp  ,  s_outp  ,  s_iset  ,  s_aset  ,  s_aref, 
  /* APL specials */
  s_rank  ,  s_tally,   s_search,  s_four  ,  s_ifour , 
  /* Input functions */
  s_ratom,   s_read,    s_getc,    s_readl,   s_getl,   s_getb   ,  s_rclr,  
  /* Printing */
  s_print,   s_prin0,   s_prinl,   s_princ,   s_pp,     s_putb   , 
  s_plen,    s_tab,     s_lpos,    s_prat,    s_terpri, 
  s_prompt,  s_beep,  
  /* File operations */
  s_open,    s_ioctl,   s_close,   s_rewind,  s_seek,    s_buf,     s_eof,
  s_dump,
  /* Time related functions */
  s_clock,   s_date ,   s_time ,  
  /* Sound */
  s_sopen,   s_play ,   s_rec  ,     s_sclose,
  /* Instrument IO */
  s_oio,     s_rio  ,   s_wio  ,     s_cio, 
  /* Graphics */
  s_gclear,  s_gupdate, s_glinit,  s_genable, s_gdisable,
  s_graf,    s_loc,     s_proj,    s_gmat,    s_light,   s_glmodel,
  s_gsmodel, s_mkwdgt,  s_cfgwdgt, s_dispwdgt,s_hidewdgt,s_wdgtdata,
  s_gsldrval,s_gdialval,s_gfsval,  s_gtxtval, s_gtxtclr, s_gtxtset, 
  s_gcanvas, s_gsave,   s_gload,   s_gwar,    s_gresize,
  /* Host operating system and file system functions */
  s_sys,     s_getenv,  s_setenv,    s_dl,      s_wd,      s_pu,
  s_rn,      s_st,
  /* System information */
  s_pfree ,  s_pv   ,   s_stats ,  s_whzat, 
  /* Compiler specials */
  s_faddr,   s_mint,    s_mkprc,   s_rdda,   s_peep,  
  s_mget,    s_mput,    s_sysp,
  /* argument handling specials */
  s_arg,    s_args,
/*insert new items here ^^^ dont reorder*/
/*keep this lot here    VVV at the end  bounded by s_var and s_trailer */
  s_var,       s_comat,     s_con,
  s_sys_eval,  s_sys_eval1, s_done,      
  s_sys_apply, s_app_loop,  s_app_ret0,  s_app_ret1,  s_app_ret2,
  s_eval0, 
  /* auxiliary housekeeping functions */
  s_rs_iexp,  s_ret_iexp, s_rp_giexp,  s_fxend,
  s_rs_env,   s_dpret,   
  s_rsrt,     s_rsrt1,    s_merge,    s_merge1,    s_merge2,    s_merge3,
  s_setqq0,   s_defcn0,   s_evargs,   s_eva_do,    s_eva_end,   s_cond_cont,
  s_unbind,   s_ev_res,   s_fnarg1,   s_evlis,     s_evlis1,
  s_teval,    s_tevalx,   s_teval1,   s_ratchetd,  s_ret1,      s_go1,
  s_throw1,   s_bail,     s_bailout,  s_condcase1, s_condcase2,
  s_or1,      s_and1,     s_and2,     s_negres,
  s_map1,     s_mapn1,    s_mapn2,    s_mapc1,     s_mapcar1,   s_maplist1, 
  s_mapcan1,  s_mapcan2,  s_find1,    s_findc1,
  s_gen1,     s_genc1,    s_ngen1,    s_ngenc1, // must be in this order
  s_while1,   s_while2,   s_repeat1,  s_repeat2,   
  s_apply1,   s_eval1a,   s_eval1e,   s_rhead,      s_rhaux,    s_rtail,
  s_rtailx,    s_rrpar,   s_rend,
  s_ratx,     s_ratx0,    s_ratx1,    s_rataux,    s_numaux,    s_cpxaux,
  s_eofx,     s_arend,    s_dqaux,    s_rqaux,     s_chuckle,
  s_rqqexp,   s_rqqend,   s_if1,      s_when1,     s_unless1,
  s_rbqcnt1,  s_rbqcnt2,  s_rbqcnt3,  s_rbqcnt4,   s_rbqcnt5,   s_rbqcnt6,
  s_rvhead,   s_rvcont,   s_revhd,    s_revcnt,    s_dumpx,     s_rdot,
  s_getc1,    s_readla,   s_readlx,   s_sys_read,  s_rendres,   
  s_load1,    s_load2,    s_load3,    s_rpaux,     s_dqexc,     s_exec1,
  s_getl1,    s_getb1,    s_brkc,     s_brke,      s_brkx,      s_brkout,
  s_saveaux,  s_rstraux,  s_prog_end, s_let0,      s_let1,      s_let_end,
  s_letstar0, s_letstar1, s_catch1,   s_catch2,    s_uwdp1,     s_uwdp2,
  s_cwcc1,    s_fun1,     s_dbgout,   s_exit,      s_bootstrap, s_evaldx,
  s_trailer
} lispTypes;

typedef enum {econ, evar, subr, fsubr, expr, fexpr} eval_types;

/* Dispatch state for tasks */

typedef enum { done  = 0,   /* don't reschedule          */
	       runbl = 1,   /* running/runnable          */
	       winp  = 2,   /* waiting for input         */
	       swait = 3,   /* waiting for timer / tcb   */
	       tyld  = 4,   /* come back to me later     */
	       bench = 5,   /* on the bench ready to go  */
	       mxdsp = 6    /* last state + 1            */
} dispstate;

/* Runtime task flags */
typedef enum { ctlFlag =  1, /* Interrupt signalled   */
	       dbgFlag =  2, /* Debug mode enabled    */
	       trcFlag =  4, /* Trace mode enabled    */
	       stpFlag =  8, /* Stepping in debugger  */
	       resFlag = 16, /* Show result of eval   */
	       pmtFlag = 32, /* Pre-emption requested */
	       wdgFlag = 64  /* Watchdog expired      */
} tflags;

/* memory management accounting type */
typedef enum {
  anon = 0, avrc = 1, aenv = 2, astk = 3, 
  acnt = 4, atcb = 5, afob = 6, alst = 7 
} acctp; /* alst must be last! */


/* table with length of basic types indexed by vtype enums: num,chr & ref  */
static const integer lentab [] = {0,numlen,cpxlen,chrlen,indlen,0};
static const char    flagtab[] = {"marh????"};
static const char    typetab[] = {"UNXCWEScTFys?"};

/* memory allocator block descriptor */
typedef struct blk_hd {
  integer bsize;     /* size of whole block (neg size if busy)              */
  pointer bnext;     /* pointer to next block free or busy                  */
  pointer bprev;     /* previous free block / overwritten by busy block     */
  /* bsize - (4 * indlen) free / or -bzise - alc_adjust busy octets         */
  /* last indlen octets contain size == bsize (also negative for busy block */
} blk_hd;

/*partition control block*/
typedef struct pcb {
  integer dtag_sz;
  pointer bnext, bprev, groover, bbase, broof;
  integer bbsz;
} pcb;

/* variable record */
typedef struct vrec {
  whole   idntt;    /* type of data: num or chr       */
  integer varels;   /* number of elements             */
  pointer varadd;   /* address of data at end of vard */
  integer vardim;   /* number of dimensions           */
  integer vard[];   /* array with size of of each dims*/
} vrec;

/* symbol record */
struct symrec { /* this animal must look just like a vrec with the
                         symbol stuff hidden between *varadd and end of vard */
  whole   idntt;    /* type is sym for symbol         */
  integer varels;   /* length of symbol name          */
  pointer varadd;   /* points to end of structure     */
  integer vardim;   /* number of dimensions (always 1)*/
  integer dimlen;   /* size of dim  (vard[0] = varels)*/
  pointer value;    /* value cell           \         */
  pointer plist;    /* property list header  \  SYMBOL*/
  pointer sysp;     /* system pointer        /  STUFF */
  lispTypes spl;    /* system property      /         */
#if (BINDING==DEEPC)
  whole   cenv;
  pointer cval;
#endif
};

typedef struct cnt {  /* continuation control block */
  whole   idntt;   /* type is cnt                                       */
  pointer next;    /* next  may be used for cnt chain and free list     */
  pointer ctlsp;   /* stack pointer into control stack                  */
  pointer stklim;  /* pointer to last slot of stack                     */
  pointer stack;   /* this continuation's control stack                 */
  whole   stksiz;  /* stack size without extension                      */
  whole   stkext;  /* size of stack extension for use on stack overflow */
  bool    extflg;  /* true if stack extension in use                    */
  bool    mprotf;  /* true if memory protect fence active on stack      */
  whole   evaldb;  /* backup of eval depth                              */
} cntT;

typedef cntT *cntTp;

typedef struct tcb {        /* task control block */
  whole       idntt;        /* type is tcb                       */
  pointer     next;         /* pnil terminated linkage in disptab*/
  /*            lisp run time context                            */ 
  lispTypes   opcd;         /* current opcode                    */
  pointer     iexp;         /* slot for general iexp register    */
  pointer     parm;         /* slot for general parm register    */
  pointer     dest;         /* saved dest,res is buried here too */
  pointer     envp;         /* binding pointer                   */
  pointer     defd;         /* everyone has their own defd       */
  pointer     base;         /* base of control stack (tcb copy)  */
  pointer     slim;         /* top  of control stack (tcb copy)  */
  pointer     cntptr;       /* continuation control block        */
  pointer     envbase;      /* bottom of the binding chain       */
  pointer     recstkp;      /* error handling recovery stack ptr */
  whole       evald;        /* eval recursion depth              */
  whole       gccnt;        /* gc count for this tcb             */
  /*               scheduler & dispatcher fields                 */ 
  integer     tid;          /* task id for easy identification   */
  integer     priority;     /* 0 <= priority <= 15 (0 highest)   */
  whole       tflags;       /* task flags: ctl dbg trc stp etc   */
  number      stime;        /* schedule time                     */
  pointer     iexpq;        /* umbilical cord: q of iexps to eval*/
  pointer     tcbwq;        /* queue of tcb's we are waiting on  */
  pointer     filwq;        /* queue of waited on fil fobs       */
  pointer     fres;         /* final res set on going to done    */
  dispstate   state;        /* dispatch state (q)                */
  pointer     errexp;       /* error expression for showError()  */
  errors      errflg;       /* tcb error state used by dispatcher*/
  char       *errinf;       /* tcb error additional text info    */
  int         serrno;       /* sys errno associated with errflg  */
  int         inpfd;        /* current input  file descriptor    */
  int         outfd;        /* current output file descriptor    */
  number      utime;        /* usage time in TICKS               */
#if (DEBUG)
  whole       iexctr;       /* instructions executed counter     */
  lispTypes   pcring[BB_PCDEPTH];   /* the black box             */
  integer     pcrind;       /* current black box index           */
  lispTypes   lpope;        /* last point of pre-emption         */
  integer     lpictr;       /* iexctr at last pope               */
#endif
  lispTypes   eopcd;        /* error opcode                      */
} tcbT;

typedef tcbT *tcbTp;

/* Structure of table entries for  LISP builtins */
struct funent {
  lispTypes  stype;
  char      *lispName;
  eval_types lispEval;
  int        fparmTemplate;
};

/* Structures for argument checking constants */
enum reqtp {man, opt, optn, rst}; /* Mandatory, optional ... rest */

struct parment {
  enum reqtp req;
  vtype typ;
};
  
struct fparms {
  uchar          fman,fopt,frst;
  struct parment plist[5];
};

typedef union rootable {
  pointer a[NUM_ROOT_VARS]; /* a for array */
  struct { /* redefines a */
    /* only ever add new elements to the end and don't exceed NUM_ROOT_VARS */
    pointer wspc;       /* 0 workspace address                      */
    pointer ospc;       /* 1 oblist                                 */
    pointer fspc;       /* 2 fs space                               */
    pointer fspc_end;   /* 3 fs space end                           */
    pointer bbp;        /* 4 base partition pointer                 */
    pointer wspc_end;   /* 5 address of last word in ws             */
    pointer envp;       /* 6 current binding environment            */
    whole   fffsi;      /* 7 index of first free  fs entry          */
    pointer snil;       /* 8 address of the symbol nil              */
    pointer strue;      /* 9 address of the symbol true             */
    pointer lds;        /*10 pointer to dimension sizes             */
    pointer ldx;        /*11 pointer to dimension sizes extra       */
    pointer indad;      /*12 pointer to subscript values            */
    pointer repl;	/*13 read eval print code                   */
    pointer vreched;    /*14 head of allocated vrec list            */
    integer valign;     /*15 address alignment                      */
    pointer sysprmpt;   /*16 system prompt                          */
    pointer filler;     /*17 Filler - was debug prompt              */
    pointer filler1;    /*18 Filler - was old current prompt        */ 
    pointer brkprmpt;   /*19 initial break prompt                   */
    pointer bprompt;    /*20 current break prompt                   */  
    whole   nfsfree;    /*21 number of free fs entries              */
    pointer fs_bm;      /*22 fs bitmap in octets                    */
    pointer envbase;    /*23 first address of binding environment   */
    pointer envlim;     /*24 last address of binding environment    */
    integer env_blks;   /*25 total number of env blocks             */
    pointer cnt_free;   /*26 Continuation block free list           */
    pointer cnt_busy;   /*27 Busy continuation block entries        */
    whole   cnt_alloc;  /*28 Number of stacks currently allocated   */ 
    pointer fob_busy;   /*29 Busy fob control block entries         */
    pointer nonum;      /*30 Null numeric vector                    */
    pointer nostr;      /*31 Null string                            */
    pointer noref;      /*32 Null reference vector                  */
  } s; /* s for struct */
} rootable;

/* Structures for APL system constants and variables #.. */
typedef struct conent { /* constant creation data record */
  char         name[3];
  vtype        vtp;
  lispTypes    ltp;
  integer      numdim;
  integer      numels;
} conentT;

/* Structure of table entries for error symbols */
typedef struct {
  errors  errind;
  char   *errname;
  char   *errmes;
} errentT;

typedef number      hnval[apl_ncon + 18]; 
typedef const char *hcval[apl_ccon];
typedef integer     hwval[apl_wcon];


/******* CONSTANT DECLARATION SECTION ******/

static const char *typeDesc[] = {
  "Unknown        "                        ,  //  0 
  "Number         "                        ,  //  1
  "Complex        "                        ,  //  2
  "Character      "                        ,  //  3
  "Reference      "                        ,  //  4
  "Environment    "                        ,  //  5
  "Stack          "                        ,  //  6
  "Continuation   "                        ,  //  7
  "Task           "                        ,  //  8
  "Foreign Object "                        ,  //  9
  "Constant Symbol"                        ,  // 10
  "Symbol         "                        ,  // 11
  "List"                                   ,  // 12
  "S-expression"                           ,  // 13
  "Number or Complex"                      ,  // 14
  "Numeric or Character"                   ,  // 15
  "Numeric or Character or Reference"      ,  // 16
  "Numeric or Character or Reference Array",  // 17
  "Numeric Scalar"                         ,  // 18
  "Numeric Singleton"                      ,  // 19
  "Numeric Singleton or nil"               ,  // 20
  "Numeric Singleton or Vector"            ,  // 21
  "Not Used"                               ,  // 22
  "Numeric Vector"                         ,  // 23
  "Numeric Array"                          ,  // 24
  "Character Singleton"                    ,  // 25
  "Character Vector"                       ,  // 26
  "Num singleton or Character vector"      ,  // 27
  "Primitive dyadic numeric operator"      ,  // 28
  "Primitive dyadic operator"              ,  // 29
  "Function or symbol"                     ,  // 30 
  "Cons cell"                              ,  // 31
  "File like object"                       ,  // 32
  "File object"                            ,  // 33
  "Character vector or file object"        ,  // 34
  "Buffer object"                          ,  // 35 
  "Instrument object"                      ,  // 36
  "Widget"                                 ,  // 37
  "Any"                                       // 38
};

/**** Lisp function entry table *****/

static const struct funent evtab[] = {
  {s_nop, 	"nop",          evar,  0},
  {s_cond, 	"cond", 	fsubr, 0},
  {s_fun, 	"fun",  	fsubr, 0},
  {s_defun, 	"de", 	        fsubr, 0},
  {s_defexpr, 	"df", 	        fsubr, 0},
  {s_defmacro, 	"dm", 	        fsubr, 0},
  {s_defclos, 	"dc", 	        fsubr, 0},
  {s_defconst, 	"defconst",     fsubr, 0},
  /* Application, evaluation, binding and prog */
  {s_apply, 	"apply", 	subr,  7},
  {s_eval, 	"eval", 	subr,  8},
  /* These three are the only expr's   */
  {s_lambda, 	"lambda", 	expr,  0},
  {s_label, 	"label", 	expr,  0},
  {s_cont, 	"cont", 	expr,  0},
  /* These three are the only fexpr's  */
  {s_fnarg, 	"fnarg", 	fexpr, 0},
  {s_fexpr, 	"fexpr", 	fexpr, 0},
  {s_macro, 	"macro", 	fexpr, 0},
  /*   Lisp control flow primitives    */
  {s_let,       "let",          fsubr, 0},
  {s_letstar,   "let*",         fsubr, 0},
  {s_prog, 	"prog", 	fsubr, 0},
  {s_progstar,  "prog*",        fsubr, 0},
  {s_return, 	"return", 	subr, 18},
  {s_go, 	"go", 	        subr,  2}, /* checked by go */
  {s_if, 	"if",  	        fsubr, 0},
  {s_when,  	"when",         fsubr, 0},
  {s_unless, 	"unless",       fsubr, 0},
  {s_while, 	"while", 	fsubr, 0},
  {s_repeat, 	"repeat", 	fsubr, 0},
  {s_catch, 	"catch", 	fsubr, 0},
  {s_throw, 	"throw", 	subr,  4},
  {s_uwdprot, "unwind-protect", fsubr, 0},
  {s_condcase,"condition-case", fsubr, 0},
  {s_error,	"error",        subr,  9},
  {s_cwcc, 	"cwcc", 	subr, 10},
  /*     Task handling                 */
  {s_peval, 	"peval", 	subr,  8},
  {s_getpri, 	"getpri", 	subr, 11},
  {s_setpri, 	"setpri", 	subr, 12},
  {s_send, 	"send", 	subr, 13},
  /*         map functions             */
  {s_mapc, 	"mapc", 	subr, 14},
  {s_map, 	"map",  	subr, 14},
  {s_mapcan, 	"mapcan", 	subr, 14},
  {s_mapn, 	"mapn", 	subr, 14},
  {s_mapcar, 	"mapcar", 	subr, 14},
  {s_maplist, 	"maplist", 	subr, 14},
  {s_gen, 	"gen",          subr, 14},
  {s_genc, 	"genc", 	subr, 14},
  {s_ngen, 	"ngen",         subr, 14},
  {s_ngenc, 	"ngenc", 	subr, 14},
  {s_find, 	"find", 	subr, 15},
  {s_findc, 	"findc", 	subr, 15},
  {s_sort, 	"sort", 	subr, 16},
 /*         Interpreter control        */
  {s_break, 	"break", 	fsubr, 0},
  {s_wait, 	"wait", 	subr, 17},/*n,tcb list ,fil list*/
  {s_thaw,  	"thaw",         fsubr, 0},
  {s_freeze,  	"freeze",       fsubr, 0},
  {s_dbg, 	"dbg", 	        fsubr, 0},
  {s_trace, 	"trc",  	fsubr, 0},
  {s_kill, 	"kill",  	subr, 102},
  {s_load, 	"load", 	subr,  9},
  {s_require, 	"require", 	subr, 19},
  {s_save, 	"save", 	subr, 20},
  {s_restore, 	"restore", 	subr, 20},
  {s_gc, 	"gc",   	fsubr, 0},
  {s_quit, 	"quit",  	subr, 21},
  {s_bye, 	"bye",  	fsubr, 0},
  /******* no primitive from here on may call eval if trr is to work */
  /* monadic predicates */
  {s_atom, 	"atom", 	subr,  2},
  {s_consp, 	"consp", 	subr,  2},
  {s_listp, 	"listp", 	subr,  2},
  {s_nlistp,    "nlistp",       subr,  2},
  {s_null, 	"null", 	subr,  2},
  {s_chrp, 	"chrp", 	subr,  2},
  {s_nump, 	"nump", 	subr,  2},
  {s_cpxp, 	"cpxp", 	subr,  2},
  {s_refp, 	"refp", 	subr,  2},
  {s_symp,      "symbolp",      subr,  2},
  {s_primp,     "primp",        subr,  2},
  {s_boundp, 	"boundp", 	subr,  3},
  {s_zerop, 	"zerop", 	subr, 32},
  {s_onep, 	"onep", 	subr, 32},
  {s_oddp, 	"oddp", 	subr, 32},
  {s_evenp, 	"evenp", 	subr, 32},
  /* dyadic predicates */
  {s_lt, 	"lt",   	subr, 22},
  {s_le, 	"le",   	subr, 22},
  {s_eq, 	"eq",   	subr, 23},
  {s_ne, 	"ne", 	        subr, 23},
  {s_gt, 	"gt", 	        subr, 22},
  {s_ge, 	"ge",      	subr, 22},
  {s_eql, 	"eql", 	        subr, 23},
  {s_neql, 	"neql", 	subr, 23},
  {s_equal, 	"equal", 	subr, 23},
  {s_nequal, 	"nequal", 	subr, 23},
  {s_memq, 	"memq",  	subr, 24},
  {s_member, 	"member", 	subr, 24},
  {s_subset, 	"subset", 	subr, 25},
  /*     Lisp logical connectives      */
  {s_not, 	"not",  	subr, 26},
  {s_and, 	"and",  	fsubr, 0},
  {s_or,	"or",   	fsubr, 0},
  {s_nand, 	"nand",  	fsubr, 0},
  {s_nor,	"nor",   	fsubr, 0},
  /* Dudes who attack lists with knives */
  {s_delete, 	"delete", 	subr, 24},
  {s_rplaca, 	"rplaca", 	subr, 27},
  {s_rplacd, 	"rplacd", 	subr, 27},
  {s_nconc, 	"nconc", 	subr, 28},
  {s_nrev, 	"nrev", 	subr,  1},
  /*          List Accessors           */
  {s_car, 	"car", 	        subr,  1},
  {s_cdr, 	"cdr", 	        subr,  1},
  {s_caar, 	"caar", 	subr,  1},
  {s_cadr, 	"cadr", 	subr,  1},
  {s_cdar, 	"cdar", 	subr,  1},
  {s_cddr, 	"cddr", 	subr,  1},
  {s_caaar, 	"caaar", 	subr,  1},
  {s_caadr, 	"caadr", 	subr,  1},
  {s_cadar, 	"cadar", 	subr,  1},
  {s_caddr, 	"caddr", 	subr,  1},
  {s_cdaar, 	"cdaar", 	subr,  1},
  {s_cdadr, 	"cdadr", 	subr,  1},
  {s_cddar, 	"cddar", 	subr,  1},
  {s_cdddr, 	"cdddr", 	subr,  1},
  {s_nth, 	"nth", 	        subr, 29},
  {s_nthcdr, 	"nthcdr",       subr, 29},
  {s_last, 	"last", 	subr, 30},
  /* List constructors */
  {s_cons, 	"cons", 	subr, 23},
  {s_acons, 	"acons", 	subr, 31},
  {s_list, 	"list", 	subr, 41},
  {s_liststar, 	"list*", 	subr, 94},
  {s_append, 	"append", 	subr, 42},
  {s_pairlis, 	"pairlis", 	subr, 44},
  /* Symbol stuff */
  {s_qquot, 	"qquote",       evar,  0},
  {s_quote, 	"quote", 	fsubr, 0},
  {s_gensym, 	"gensym", 	fsubr, 0},
  {s_oblis, 	"oblis", 	fsubr, 0},
  {s_clear, 	"clear", 	fsubr, 0},
  {s_spec,      "a",            fsubr, 0},  /* specification ass */
  {s_setq, 	"setq", 	fsubr, 0},
  {s_setqq, 	"setqq", 	fsubr, 0},
  {s_set, 	"set",  	subr,  4},
  {s_pop, 	"pop",  	subr,  3},
  {s_push, 	"push", 	subr,  5},
  {s_put, 	"put",  	subr,  6},
  {s_get, 	"get", 	        subr,  4},
  {s_remprop, 	"remprop", 	subr,  4},
  {s_intern, 	"intern", 	subr,  9},
  /* List processing */
  {s_subst, 	"subst", 	subr, 31},
  {s_revl, 	"revl", 	subr,  1},
  {s_rotl, 	"rotl", 	subr, 93},
  /* List queries */
  {s_len, 	"len",  	subr,  1},
  {s_pos,  	"pos",          subr, 24},
  {s_assoc, 	"assoc", 	subr, 24},
  /* Data conversion */
  {s_num, 	"num",  	subr, 33},
  {s_chr, 	"chr",  	subr, 32},
  //(implod [[axs] cym] lst) self check
  {s_implod, 	"implod", 	subr, 34},
  {s_explod, 	"explod", 	subr, 35},
  /* Arithmetic convenience */
  {s_incr,      "incr",         fsubr, 0},
  {s_decr,      "decr",         fsubr, 0},
  {s_sqrt,      "sqrt",         subr, 36},
  {s_modexp,    "modexp",       subr, 90},
  {s_int,       "int",          subr, 36},
  {s_frc,       "frc",          subr, 36},
  {s_isint,     "intp",         subr, 36},
  /* APL functions. Start of prim_dyadics */
  {s_dist,      "dist",         subr, 37},
  {s_plus,      "+",            subr, 37},
  {s_minus,     "-",            subr, 37},
  {s_times,     "*",            subr, 37},
  {s_divid,     "/",            subr, 37},
  {s_ceil,      "c",            subr, 37},
  {s_floor,     "f",            subr, 37},
  {s_andd,      "^",            subr, 38},
  {s_xor,       "x",            subr, 38},
  {s_vor,       "v",            subr, 38},
  {s_nandd,     "~^",           subr, 38},
  {s_nord,      "~v",           subr, 38},
  {s_resid,     "|",            subr, 43},
  {s_power,     "exp",          subr, 43},
  {s_log,       "l",            subr, 43},
  {s_trig,      "%",            subr, 43},
  {s_binom,     "!",            subr, 43},
   /* End of primitive dyadic numeric operators */
  {s_great,     ">",            subr, 39},
  {s_greatq,    ">=",           subr, 39},
  {s_lessn,     "<",            subr, 39},
  {s_lessq,     "<=",           subr, 39},
  {s_eqapl,     "=",            subr, 40},
  {s_neapl,     "<>",           subr, 40},
  /* end prim dyadics: sanity check requires s_roll be next <<<<<            */
  {s_roll,      "?",            subr, 51},
  {s_nott,      "~",            subr, 32},
  {s_shape,     "p",            subr, 45},
  {s_ravel,     "rav",          subr, 47},
  {s_cat,       "cat",          subr, 48},
  {s_gup,       "gup",          subr, 49},
  {s_gdn,       "gdn",          subr, 49},
  {s_iota,      "i",            subr, 46}, 
  {s_rev, 	"rev",  	subr, 35},  
   /* reverse (rev [axis] <any>) axis only valid for ncr  */
  {s_transp,    "tr",           subr, 45},
  {s_minv,      "m/",           subr, 51},  // self check
  {s_exec,      "exec",         subr, 33},  /* execute string arg*/
  {s_fmt, 	"fmt",  	subr, 41},
  {s_komp,      "k",            subr, 52},
  {s_expnd,     "ex",           subr, 52},
  {s_take,      "tk",           subr, 53},
  {s_drop,      "dp",           subr, 53}, 
  {s_rot, 	"rot",  	subr, 91},
  {s_shft, 	"shft",  	subr, 91},
  /* (rot/shft arg [amountt [axis]]) */
  {s_decod,     "dec",          subr, 54},
  {s_encod,     "enc",          subr, 54},
  {s_elmnt,     "elt",          subr, 55},
  {s_index,     "ind",          subr, 55},
  /* Composite functions */
  {s_reduc,     "r",            subr, 56},
  {s_scan,      "s",            subr, 56},
  {s_inrp,      ".",            subr, 57},
  {s_outp,      "o",            subr, 58},
  /* Array indexing */ 
  {s_iset, 	"iset", 	subr, 59},
  {s_aset, 	"aset", 	subr, 60},
  //  "(aset <array> <val> <index>)"
  {s_aref, 	"aref",  	subr, 61},
 /* APL extensions */
  {s_rank,      "rank",         subr, 47},
  {s_tally,     "tally",        subr, 47},
  {s_search,    "ss",           subr, 62},
  /* string search "(ss <pattern:C> <target:C> [<axis:N> [<start:N>]])"   */
  {s_four,      "w",            subr, 36}, 
  /* "discrete fourier transform (w <mat:N>)" */
  {s_ifour,     "y",            subr, 36},
  /* "inverse discrete fourier transform (w <mat:N>)" */
  /* Input */
  {s_ratom, 	"ratom", 	subr, 63},
  {s_read, 	"read", 	subr, 63},
  {s_getc, 	"getc", 	subr, 63},
  {s_readl, 	"readl", 	subr, 63},
  {s_getl, 	"getl", 	subr, 98},
  {s_getb, 	"getb", 	subr, 64},
  {s_rclr, 	"rclr", 	subr, 65},
  /* Printing */
  {s_print, 	"print", 	subr, 66},
  {s_prin0, 	"prin0", 	subr, 66},
  {s_prinl, 	"prinl", 	subr, 66},
  {s_princ, 	"princ", 	subr, 66},
  {s_pp, 	"pp",   	subr, 92},
  {s_putb, 	"putb",   	subr, 68},
  {s_plen, 	"plen",   	subr, 26},
  {s_tab, 	"tab",  	subr, 67},
  {s_lpos, 	"lpos", 	subr, 65},
  {s_prat, 	"prat", 	subr, 70},
  {s_terpri, 	"terpri", 	subr, 69},
  {s_prompt, 	"prompt", 	subr,  0},
  {s_beep, 	"beep", 	subr, 71},
  /* File */
  {s_open,  	"open",  	subr, 95},
  {s_ioctl,  	"ioctl",  	subr, 72},
  {s_close, 	"close", 	subr, 73},
  {s_rewind,	"rewind",	subr, 73},
  {s_seek,      "seek",         subr, 64},
  {s_buf, 	"buf",  	subr, 74},
  {s_eof, 	"eof",   	subr, 73},
  {s_dump, 	"du",   	subr,  9},
  /* Time related functions */
  {s_clock, 	"clock", 	fsubr, 0},
  {s_date, 	"date", 	subr, 21},
  {s_time, 	"time", 	fsubr, 0},
  /* Sound */
  {s_sopen, 	"sopen",   	fsubr, 0},
  {s_play, 	"play",   	subr, 99},
  {s_rec, 	"rec",   	subr, 67},
  {s_sclose, 	"sclose",   	fsubr, 0},
  /* Instrument IO */
  {s_oio, 	"oio", 	        subr,101},
  {s_rio, 	"rio", 	        subr, 75},
  {s_wio, 	"wio",  	subr, 76},
  {s_cio, 	"cio", 	        subr, 75},
  /* Graphics */
  {s_gclear, 	"gclear", 	fsubr, 0},
  {s_gupdate, 	"gupdate",      subr, 97},
  {s_glinit, 	"glinit",       fsubr, 0},
  {s_genable, 	"genable",      subr, 77},
  {s_gdisable, 	"gdisable",     subr, 77},
  {s_graf, 	"graf", 	subr,100},
  {s_loc, 	"loc",  	subr,  0},
  {s_proj, 	"proj",         subr, 26},
  {s_gmat, 	"gmat", 	subr, 78},
  /* Material propgmat (gmat <face> <propname> <prop>)*/
  {s_light, 	"light", 	subr, 78},
  /* Lighting src props "(light <lnum> <propname> <prop>");*/
  {s_glmodel, 	"glmodel", 	subr, 79},
  /* Lighting model     */
  {s_gsmodel, 	"gsmodel", 	subr, 67},
  /* Shading model   "(gsmodel [EZ_FLAT | EZ_SHADE])"   */
  {s_mkwdgt,    "mkwdgt",       subr, 80},
  /* Create Widget "mkwdgt type parent <list>)"     */
  {s_cfgwdgt,   "cfgwdgt",      subr, 81},
  /* Configure Widget "cfgwdgt widget <list>");  */
  {s_dispwdgt,  "dispwdgt",     subr, 82},
  {s_hidewdgt,  "hidewdgt",     subr, 82},
  {s_wdgtdata,  "wdgtdata",     subr, 82},
  {s_gsldrval,  "gsldrval",     subr, 82},
  {s_gdialval,  "gdialval",     subr, 82},
  {s_gfsval,    "gfsval",       subr, 82},
  {s_gtxtval,   "gtxtval",      subr, 82},
  {s_gtxtclr,   "gtxtclr",      subr, 82},
  {s_gtxtset,   "gtxtset",      subr, 83},
  {s_gcanvas,   "gcanvas",      subr, 82},
  {s_gsave,     "gsave",        subr, 83},
  {s_gload,     "gload",        subr,103},
  {s_gwar,      "gwar",         subr, 82},
  {s_gresize,   "gresize",      subr,104},
  /* Host System Functions */
  {s_sys, 	"sys",  	subr, 84},
  {s_getenv, 	"getenv",  	subr,  9},
  {s_setenv, 	"setenv",  	subr, 85},
  {s_dl, 	"dl",   	subr, 20},
  {s_wd, 	"wd" ,  	subr, 20},
  {s_pu, 	"pu",   	subr,  9},
  {s_rn, 	"rn",   	subr, 85},
  {s_st, 	"st",   	subr,  9},
  /* System information */
  {s_pfree, 	"pf", 	        subr, 21},
  {s_pv,  	"pv",           fsubr, 0},
  {s_stats, 	"stats", 	subr, 18},
  {s_whzat,  	"wh",  	        fsubr, 0},
   /* Compiler specials */
  {s_faddr, 	"addr", 	subr, 26},
  /* make num of address of obj (addr <obj>)  */
  {s_mint, 	"mint", 	subr, 86},
  {s_mkprc, 	"mkprc", 	subr, 87},
  /* make fun a compiled procedure with code (mkprc <fun:S> <code:C>) */
  {s_rdda, 	"rdda", 	subr, 67},
  /* make address of num (rdda <addr:N>) */
  {s_peep, 	"peep", 	subr, 88},
  /* make num(s) from *addr for len words" (peep <addr> [len])" */
  {s_mget, 	"mget", 	subr, 88},
  /* get len bytes from addr (mget <addr:N> [<len:N>])*/
  {s_mput, 	"mput", 	subr, 89},
  /* put object at addr   */
  {s_sysp, 	"sysp", 	subr,  3},
  /* return system prop (sysp <sym:S>) */
  {s_arg, 	"arg",  	subr, 26},/* return first arg */
  {s_args, 	"args",  	fsubr, 0}, /* return args for macro/fexpr */
   /*insert new user primitives above this line only */
  {s_var, 	"var",  	evar,  0},
  {s_comat,     ",@",           econ,  0},
 /* all entries up to here are builtins, s_con used to dimension add table */
 /* From here on down no atoms are actually built in oblis                 */
  {s_con, 	"con",  	econ,  0},/* constant placeholder */
  {s_sys_eval, 	"sys_eval", 	evar,  0},
  {s_sys_eval1,	"sys_eval1", 	evar,  0},
  {s_done, 	"done", 	evar,  0},
  {s_sys_apply,	"sys_apply", 	evar,  0},
  {s_app_loop,	"app_loop", 	evar,  0},
  {s_app_ret0,	"app_ret0", 	evar,  0},
  {s_app_ret1,	"app_ret1", 	evar,  0},
  {s_app_ret2,	"app_ret2", 	evar,  0},
  {s_eval0, 	"eval0", 	evar,  0},
  {s_rs_iexp, 	"rs_iexp", 	evar,  0},
  {s_ret_iexp, 	"ret_iexp", 	evar,  0},
  {s_rp_giexp, 	"rp_giexp", 	evar,  0},
  {s_fxend,  	"fxend",        evar,  0},
  {s_rs_env, 	"rs_env", 	evar,  0},
  {s_dpret,  	"dpret",  	evar,  0},
  {s_rsrt,  	"rsrt",  	evar,  0},
  {s_rsrt1, 	"rsrt1", 	evar,  0},
  {s_merge, 	"merge", 	evar,  0},
  {s_merge1,	"merge1",	evar,  0},
  {s_merge2,	"merge2",	evar,  0},
  {s_merge3,	"merge3",	evar,  0},
  {s_setqq0, 	"setqq0", 	evar,  0},
  {s_defcn0, 	"defcn0", 	evar,  0},
  {s_evargs,	"evargs", 	evar,  0},
  {s_eva_do,    "eva_do", 	evar,  0},
  {s_eva_end, 	"eva_end", 	evar,  0},
  {s_cond_cont, "cond_cont", 	evar,  0},
  {s_unbind, 	"unbind", 	evar,  0},
  {s_ev_res, 	"ev_res", 	evar,  0},
  {s_fnarg1, 	"fnarg1", 	evar,  0},
  {s_evlis, 	"evlis", 	evar,  0},
  {s_evlis1, 	"evlis1", 	evar,  0},
  {s_teval, 	"teval", 	evar,  0},
  {s_tevalx, 	"tevalx", 	evar,  0},
  {s_teval1, 	"teval1", 	evar,  0},
  {s_ratchetd, 	"ratchetd", 	evar,  0},
  {s_ret1,	"ret1", 	evar,  0},
  {s_go1,	"go1",  	evar,  0},
  {s_throw1,	"throw1", 	evar,  0},
  {s_bail,	"bail", 	evar,  0},
  {s_bailout,	"bailout", 	evar,  0},
  {s_condcase1,	"condcase1", 	evar,  0},
  {s_condcase2,	"condcase2", 	evar,  0},
  {s_or1, 	"or1", 	        evar,  0},
  {s_and1, 	"and1",	        evar,  0},
  {s_and2, 	"and2",	        evar,  0},
  {s_negres, 	"negres",       evar,  0},
  {s_map1, 	"map1", 	evar,  0},
  {s_mapn1, 	"mapn1", 	evar,  0},
  {s_mapn2, 	"mapn2", 	evar,  0},
  {s_mapc1, 	"mapc1", 	evar,  0},
  {s_mapcar1, 	"mapcar1", 	evar,  0},
  {s_maplist1, 	"maplist1", 	evar,  0},
  {s_mapcan1, 	"mapcan1", 	evar,  0},
  {s_mapcan2, 	"mapcan2", 	evar,  0},
  {s_find1, 	"find1", 	evar,  0},
  {s_findc1, 	"findc1", 	evar,  0},
  {s_gen1, 	"gen1", 	evar,  0},
  {s_genc1, 	"genc1", 	evar,  0},
  {s_ngen1, 	"ngen1", 	evar,  0},
  {s_ngenc1, 	"ngenc1", 	evar,  0},
  {s_while1, 	"while1", 	evar,  0},
  {s_while2, 	"while2", 	evar,  0},
  {s_repeat1, 	"repeat1", 	evar,  0},
  {s_repeat2, 	"repeat2", 	evar,  0},
  {s_apply1, 	"apply1", 	evar,  0},
  {s_eval1a, 	"eval1a", 	evar,  0},
  {s_eval1e, 	"eval1e", 	evar,  0},
  {s_rhead, 	"rhead", 	evar,  0},
  {s_rhaux, 	"rhaux", 	evar,  0},
  {s_rtail, 	"rtail", 	evar,  0},
  {s_rtailx, 	"rtailx", 	evar,  0},
  {s_rrpar, 	"rrpar", 	evar,  0},
  {s_rend, 	"rend", 	evar,  0},
  {s_ratx, 	"ratx", 	evar,  0},
  {s_ratx0, 	"ratx0", 	evar,  0},
  {s_ratx1, 	"ratx1", 	evar,  0},
  {s_rataux, 	"rataux", 	evar,  0},
  {s_numaux, 	"numaux", 	evar,  0},
  {s_cpxaux, 	"cpxaux", 	evar,  0},
  {s_eofx, 	"eofx", 	evar,  0},
  {s_arend, 	"arend", 	evar,  0},
  {s_dqaux,     "dqaux", 	evar,  0},
  {s_rqaux,     "rqaux", 	evar,  0},
  {s_chuckle,   "chuckle",	evar,  0},
  {s_rqqexp,	"rqqexp",	evar,  0},
  {s_rqqend,	"rqqend",	evar,  0},
  {s_if1,	"if1",  	evar,  0},
  {s_when1,	"when1",  	evar,  0},
  {s_unless1,	"unless1",  	evar,  0},
  {s_rbqcnt1,	"rbqcnt1",	evar,  0},
  {s_rbqcnt2,	"rbqcnt2",	evar,  0},
  {s_rbqcnt3,	"rbqcnt3",	evar,  0},
  {s_rbqcnt4,	"rbqcnt4",	evar,  0},
  {s_rbqcnt5,	"rbqcnt5",	evar,  0},
  {s_rbqcnt6,	"rbqcnt6",	evar,  0},
  {s_rvhead,	"rvhead", 	evar,  0},
  {s_rvcont,	"rvcont", 	evar,  0},
  {s_revhd,	"revhd", 	evar,  0},
  {s_revcnt,	"revcnt", 	evar,  0},
  {s_dumpx, 	"dumpx", 	evar,  0},
  {s_rdot, 	"rdot", 	evar,  0},
  {s_getc1, 	"getc1", 	evar,  0},
  {s_readla, 	"readla", 	evar,  0},
  {s_readlx, 	"readlx", 	evar,  0},
  {s_sys_read, 	"sys_read", 	evar,  0},
  {s_rendres, 	"rendres", 	evar,  0},
  {s_load1, 	"load1", 	evar,  0},
  {s_load2, 	"load2", 	evar,  0},
  {s_load3, 	"load3", 	evar,  0},
  {s_rpaux, 	"rpaux", 	evar,  0},
  {s_dqexc, 	"dqexc", 	evar,  0},
  {s_exec1, 	"exec1", 	evar,  0},
  {s_getl1, 	"getl1", 	evar,  0},
  {s_getb1, 	"getb1", 	evar,  0},
  {s_brkc, 	"brkc", 	evar,  0},
  {s_brke, 	"brke", 	evar,  0},
  {s_brkx, 	"brkx", 	evar,  0},
  {s_brkout, 	"brkout", 	evar,  0},
  {s_saveaux, 	"saveaux",  	evar,  0},
  {s_rstraux, 	"rstraux",  	evar,  0},
  {s_prog_end,  "prog_end",     evar,  0},
  {s_let0,      "let0",         evar,  0},
  {s_let1,      "let1",         evar,  0},
  {s_let_end,   "let_end",      evar,  0},
  {s_letstar0,  "letstar0",     evar,  0},
  {s_letstar1,  "letstar1",     evar,  0},
  {s_catch1,    "catch1",       evar,  0},
  {s_catch2,    "catch2",       evar,  0},
  {s_uwdp1,     "uwdp1",        evar,  0},
  {s_uwdp2,     "uwdp2",        evar,  0},
  {s_cwcc1,     "cwcc1",        evar,  0},
  {s_fun1,      "fun1",         evar,  0},
  {s_dbgout,    "dbgout",       evar,  0},
  {s_exit,      "exit",         evar,  0},
  {s_bootstrap,	"bootstrap", 	evar,  0},
  {s_evaldx,	"evaldx", 	evar,  0},
  {s_trailer, 	"trailer", 	evar,  0}
};

/* formal parameter template table */
struct fparms ftt[] = {
  /* 0 no chck*/  {0,0,0,{}},
  /* 1 lst    */  {1,0,0,{{man,lst}}},
  /* 2 sxp   */   {1,0,0,{{man,sxp}}},
  /* 3 sym    */  {1,0,0,{{man,sym}}},
  /* 4 sym,any*/  {2,0,0,{{man,sym},{man,any}}},
  /* 5 any,sym*/  {2,0,0,{{man,any},{man,sym}}},
  /* 6 sym,any2*/ {3,0,0,{{man,sym},{man,any},{man,any}}},
  /* 7 apply  */  {2,0,1,{{man,fun},{optn,any},{man,lst}}},
  /* 8 eval   */  {1,1,0,{{man,sxp},{opt,env}}},
  /* 9 cv     */  {1,0,0,{{man,cv}}},
  /*10 fun    */  {1,0,0,{{man,fun}}},
  /*11 tcb    */  {1,0,0,{{man,tcb}}},
  /*12 num tcb*/  {2,0,0,{{man,nsgl},{man,tcb}}},
  /*13 send   */  {2,1,0,{{man,sxp},{man,tcb},{opt,env}}},
  /*14 mappers*/  {2,0,1,{{man,fun},{man,lst},{rst,lst}}},
  /*15 finders*/  {3,0,0,{{man,fun},{man,lst},{man,any}}},
  /*16 fun lst*/  {2,0,0,{{man,fun},{man,lst}}},
  /*17 wait   */  {1,2,0,{{man,nsgl},{opt,lst},{opt,lst}}},
  /*18 opt sxp*/  {0,1,0,{{opt,sxp}}},
  /*19 cv ocv */  {1,1,0,{{man,cv},{opt,cv}}},
  /*20 opt cv */  {0,1,0,{{opt,cv}}},
  /*21 onsgl  */  {0,1,0,{{opt,nsgl}}},
  /*22 2 m nc */  {2,0,0,{{man,nc},{man,nc}}},
  /*23 2 m any*/  {2,0,0,{{man,any},{man,any}}},
  /*24 2 m a l*/  {2,0,0,{{man,any},{man,lst}}},
  /*25 2 m lst*/  {2,0,0,{{man,lst},{man,lst}}},
  /*26 1 m any*/  {1,0,0,{{man,any}}},
  /*27 rplacx */  {2,0,0,{{man,cns},{man,any}}},
  /*28 nconc  */  {0,1,1,{{optn,lst},{opt,any}}},
  /*29 2m n l */  {2,0,0,{{man,nsgl},{man,lst}}},
  /*30 l on   */  {1,1,0,{{man,lst},{opt,nsgl}}},
  /*31 3 m any*/  {3,0,0,{{man,any},{man,any},{man,any}}},
  /*32 1 m num*/  {1,0,0,{{man,num}}},
  /*33 1 m chr*/  {1,0,0,{{man,chr}}},
  /*34 implod */  {1,2,0,{{opt,axn},{opt,cym},{man,lst}}},
  /*35 explod */  {1,1,0,{{opt,nsgl},{man,ncr}}},
  /*36 1 m cnm*/  {1,0,0,{{man,cnm}}},
  /*37 1 r cnm*/  {0,0,1,{{rst,cnm}}},
  /*38 1 r num*/  {0,0,1,{{rst,num}}},
  /*39 1 r nc */  {0,0,1,{{rst,nc}}},
  /*40 1 r ncr*/  {0,0,1,{{rst,ncr}}},
  /*41 1 r any*/  {0,0,1,{{rst,any}}},
  /*42 1 r lst*/  {0,0,1,{{rst,lst}}},
  /*43 2 o cnm*/  {0,2,0,{{opt,cnm},{opt,cnm}}},
  /*44 3 m lst*/  {2,1,0,{{man,lst},{man,lst},{opt,lst}}},
  /*45 onv ncr*/  {1,1,0,{{opt,nsv},{man,ncr}}},
  /*46 1m nsv */  {1,0,0,{{man,nsv}}},
  /*47 1m ncr */  {1,0,0,{{man,ncr}}},
  /*48 cat    */  {2,1,0,{{opt,nsgl},{man,ncr},{man,ncr}}},
  /*49 1 m nc */  {1,0,0,{{man,nc}}},
  /*50 m o any*/  {1,1,0,{{opt,any},{man,any}}},
  /*51 m o num*/  {1,1,0,{{opt,num},{man,num}}},
  /*52 k/ex   */  {2,1,0,{{opt,nsgl},{man,nsv},{man,ncr}}},
  /*53 tk/dp  */  {2,0,0,{{man,nsv},{man,ncr}}},
  /*54 2m num */  {2,0,0,{{man,num},{man,num}}},
  /*55 2m ncr */  {2,0,0,{{man,ncr},{man,ncr}}},
  /*56 redscan*/  {2,2,0,{{man,pdo},{opt,nsgl},{opt,nsgl},{man,cnm}}},
  /*57 innerp */  {4,0,0,{{man,pdo},{man,pdo},{man,ncr},{man,nc}}},
  /*58 outerp */  {3,0,0,{{man,pdo},{man,ncr},{man,nc}}},
  /*59 iset   */  {3,0,0,{{man,ncr},{man,ncr},{man,num}}},
  /*60 aset   */  {2,0,1,{{man,ncr},{man,any},{rst,unk}}}, // Check me
  /*61 aref   */  {2,0,1,{{man,ncr},{rst,unk}}}, // Check me
  /*62 search */  {2,2,0,{{man,cv},{man,chr},{opt,nsgl},{opt,nsgl}}},
  /*63 1o flk */  {0,1,0,{{opt,flk}}},
  /*64 2m f n */  {2,0,0,{{man,flk},{man,nsgl}}},
  /*65 1o fil */  {0,1,0,{{opt,fil}}},
  /*66 a o flk*/  {1,1,0,{{man,any},{opt,flk}}},
  /*67 1m nsgl*/  {1,0,0,{{man,nsgl}}},
  /*68 c  fil */  {2,0,0,{{man,chr},{man,fil}}},
  /*69 osn ofl*/  {0,2,0,{{opt,nsgl},{opt,fil}}},
  /*70 n n oa */  {2,1,0,{{man,nsgl},{man,nsgl},{opt,cv}}},
  /*71 o ns ns*/  {0,2,0,{{opt,nsgl},{opt,nsgl}}},
  /*72 ioctl  */  {2,1,0,{{man,fil},{man,nsgl},{opt,nsgl}}},
  /*73 1m flk */  {1,0,0,{{man,flk}}},
  /*74 buf    */  {1,1,0,{{man,chr},{opt,buf}}},
  /*75 1m ins */  {1,0,0,{{man,ins}}},
  /*76 ins cv */  {2,0,0,{{man,ins},{man,cv}}},
  /*77 m r nsg*/  {1,0,1,{{man,nsgl},{rst,nsgl}}},
  /*78 n s s v*/  {3,0,0,{{man,nsgl},{man,nsgl},{man,nsv}}},
  /*79 n s v  */  {2,0,0,{{man,nsgl},{man,nsv}}},
  /*80 ns s l */  {3,0,0,{{man,nsgl},{man,any},{man,lst}}},
  /*81 wgt lst*/  {2,0,0,{{man,wgt},{man,lst}}},
  /*82 1m wgt */  {1,0,0,{{man,wgt}}},
  /*83 wgt cv */  {2,0,0,{{man,wgt},{man,cv}}},
  /*84 mr ncr */  {1,0,1,{{man,ncr},{rst,ncr}}},
  /*85 2m cv  */  {2,0,0,{{man,cv},{man,cv}}},
  /*86 2m nsgl*/  {2,0,0,{{man,nsgl},{man,nsgl}}},
  /*87 sym chr*/  {2,0,0,{{man,sym},{man,cv}}},
  /*88 m r nsg*/  {1,1,0,{{man,nsgl},{opt,nsgl}}},
  /*89 nsg cv */  {1,1,0,{{man,nsgl},{man,cv}}},
  /*90 ns n ns*/  {3,0,0,{{man,nsgl},{man,num},{man,nsgl}}},
  /*91 ns n a */  {2,1,0,{{opt,nsgl},{man,num},{man,ncr}}}, // rot
  /*92 2nsg a */  {1,2,0,{{man,any},{opt,nsgl},{opt,nsgl}}},
  /*93 onsg l */  {1,1,0,{{opt,nsgl},{opt,lst}}},           // rotl
  /*94 list*  */  {1,0,1,{{optn,any},{man,any}}},
  /*95 c oc os*/  {1,2,0,{{man,cvf},{opt,cv},{opt,nsgl}}},
  /*96 n ns   */  {2,0,0,{{man,num},{man,nsgl}}},
  /*97 1o wgt */  {0,1,0,{{opt,wgt}}},
  /*98  flk c */  {0,2,0,{{opt,flk},{opt,chr}}},
  /*99 n o ns */  {1,1,0,{{man,num},{opt,nsgl}}},
  /*100n o ns */  {1,1,0,{{opt,nsgl},{man,any}}},
  /*101 m nscv*/  {1,0,0,{{man,nscv}}},
  /*102 m unk */  {1,0,0,{{man,unk}}},
  /*103 mc on2*/  {1,2,0,{{man,cv},{opt,nsgl},{opt,num}}},
  /*104 wd ns2*/  {3,0,0,{{man,wgt},{man,nsgl},{man,nsgl}}},
};

/* apl index of identity elements of primitive dyadic functions */
/* 0=AI, 1=MI, 2=MN, 3=MX, 4=non existant */
static const integer idindx[prim_dyadics]=
   // dist + - * / c f ^ x v ~^ ~v | ` l % !  >  >= < <=  = <> 
  {     0, 0,0,1,1,2,3,1,0,0, 0, 1,0,1,4,4,1, 0, 1, 0, 1, 1, 0  };

/*apl const names*/

static conentT  h_consts[apl_consts] =   {
  {"#IB", num, s_var,   0,  0},	/* 0 #IB input base                         */
  {"#OB", num, s_var,   0,  0},	/* 1 #OB output base                        */
  {"#PP", num, s_var,   0,  0},	/* 2 #PP print precision                    */
  {"#PW", num, s_var,   0,  0},	/* 3 #PW print width                        */
  {"#FW", num, s_var,   0,  0},	/* 4 #FW field width                        */
  {"#SD", num, s_var,   0,  0},	/* 5 #SD significant digits                 */
  {"#EP", num, s_var,   0,  0},	/* 6 #EP e format mantissa digs after point */
  {"#VP", num, s_var,   1,  4},	/* 7 #VP view port                          */
  {"#WN", num, s_var,   1,  4},	/* 8 #WN window limits                      */
  {"#NF", num, s_var,   1,  2},	/* 9 #NF near far planes                    */
  {"#CS", num, s_var,   1,  2},	/*10 #CS character size                     */
  {"#CR", num, s_var,   1,  2},	/*11 #CR character rotation                 */
  {"#PM", num, s_var,   0,  1},	/*12 #PM polygon mode 0-2                   */
  {"#PN", num, s_var,   0,  0},	/*13 #PN pen number                         */
  {"#LS", num, s_var,   0,  0},	/*14 #LS line style =  width<<2 + style     */
  {"#RL", num, s_var,   0,  0},	/*15 #RL random link                        */
  {"#IO", num, s_var,   0,  0},	/*16 #IO index origin                       */
  {"#CT", num, s_var,   0,  0},	/*17 #CT comparison tolerance               */
  {"#TR", num, s_var,   0,  0},	/*18 #TR trace depth                        */
  {"#AI", num, s_con,   0,  0},	/*19 #AI additive identity = 0              */
  {"#MI", num, s_con,   0,  0},	/*20 #MI multiplicative identity = 1        */
  {"#MN", num, s_con,   0,  0},	/*21 #MN minimum number                     */
  {"#MX", num, s_con,   0,  0},	/*22 #MX maximum number                     */
  {"#NN", num, s_con,   0,  0},	/*23 #NN not a number                       */
  {"#RA", num, s_con,   0,  0},	/*24 #RA result address pointer             */
  {"#DM", num, s_var,   0,  0},	/*25 #DM drawing mode 1 - 12 (see EZ_BEGIN) */
  {"#CC", num, s_var,   1,  4},	/*26 #CC Canvas Clear Colour  R,G,B,A       */
  {"#OP", num, s_var,   1,  3},	/*27 #OP Observer position                  */
  {"#OF", num, s_var,   1,  3},	/*28 #OF Observer focus                     */
  {"#VN", num, s_con,   0,  0},	/*29 #VN Version Number major.minor         */
  {"#PL", num, s_var,   0,  0}, /*30 #PL Print Length                       */
  {"#PD", num, s_var,   0,  0}, /*31 #PD Print Depth                        */
  {"#LL", num, s_var,   0,  0}, /*32 #LL print List Length                  */
  {"#DP", num, s_var,   0,  0}, /*33 #DP Debug print Precision              */
  {"#DD", num, s_var,   0,  0}, /*34 #DD Debug print Length                 */
  {"#DL", num, s_var,   0,  0}, /*35 #DL Debug print list Length            */
  {"#GC", num, s_con,   0,  0}, /*36 #GC number of garbage collections      */
  {"#FM", num, s_con,   1,  3}, /*37 #FM Free Memory vector                 */
  {"#FS", num, s_var,   0,  0}, /*38 #FS Sampling frequency for dsp         */
  {"#CW", num, s_var,   0,  0}, /*39 #CW Console Width number of characters */
  {"#CH", num, s_var,   0,  0}, /*40 #CH Console Height  "    "     "       */
  {"#GW", num, s_con,   0,  0}, /*41 #GW Graphics Width number of pixels    */
  {"#GH", num, s_con,   0,  0}, /*42 #GH Graphics Height  "    "     "      */
  {"#FN", num, s_var,   0,  0}, /*43 #FN Graphics Font (num or chr)         */
  {"#WD", num, s_var,   0,  0}, /*44 #WD Watchdog timer interval in seconds */
  {"#GS", chr, s_var,   1,  0}, /*45 #GS Gensym base string end with digits */
  {"#CP", chr, s_con,   1,  0}, /*46 #CP Capability string                  */
  {"#DX", ref, s_var,   0,  0}, /*47 #DX Default eXp Read-Eval-Print Loop   */
  {"#RF", ref, s_var,   0,  0}, /*48 #RF read file                          */
  {"#PF", ref, s_var,   0,  0}, /*49 #PF print file                         */
  {"#LF", ref, s_var,   0,  0}, /*50 #LF log file                           */
  {"#HF", ref, s_var,   0,  0}, /*51 #HF history file                       */
  {"#RS", ref, s_var,   0,  0}, /*52 #RS last result                        */
  {"#EX", ref, s_var,   0,  0},	/*53 #EX last expression                    */
  {"#LX", ref, s_var,   0,  0},	/*54 #LX latent expression                  */
  {"#NM", ref, s_con,   0,  0},	/*55 #NM Name of alps executable (argv[0])  */
  {"#WS", ref, s_con,   0,  0},	/*56 #WS workspace name                     */
  {"#SW", ref, s_var,   0,  0}, /*57 #SW t if wsc is to be saved on exit    */
  {"#HS", ref, s_var,   0,  0}, /*58 #HS History Stack (most recent first)  */
  {"#PR", ref, s_var,   0,  0}, /*59 #PR Default prompt string              */
  {"#LP", ref, s_var,   0,  0}, /*60 #LP Load Path                          */
  {"#LH", ref, s_var,   0,  0}, /*61 #LH Load History                       */
  {"#QQ", ref, s_var,   0,  0}, /*62 #QQ Quasiquote simplify hook           */
  {"#KB", ref, s_var,   0,  0}, /*63 #KB KeyBoard translation assoc list    */
  {"#PC", ref, s_var,   0,  0}, /*64 #PC performance counter list           */
  {"#GB", ref, s_var,   0,  0}, /*65 #GB Graphics Buffer nil=Front t=Back   */
  {"#DB", ref, s_var,   0,  0}, /*66 #DB Debug flag for mreptcb             */
  {"#AL", ref, s_var,   0,  0}  /*67 #AL optional arguments list            */
}; /* zero origin */

#define NUM_APL_CONST 68 /* set this to last index + 1 above for check */

#if (NUM_APL_CONST!=apl_consts)
Number of apl constants do no match!
#endif

/*their initial values*/

static const hnval h_nval = { /* numeric initialisers */
/*#IB   #OB   #PP  #PW   #FW   #SD  #EP  #VP---------------- */ 
  10.0, 10.0, -1., 78.0, 0.0, 10.0, -1.0, 0.0, 1.0, 0.0,  1.0, 
  /*#WN---------------- #NF------ */
  -1.0, 1.0, -1.0, 1.0, 1.0, -1.0, 
  /*#CS----- #CR-----  #PM  #PN  #LS  #RL           #IO  #CT    #TR  #AI */
  0.05, 0.1, 1.0, 0.0, 1.0, 1.0, 4.0, 10051954.12,  1.0, 1e-13, 2.0, idadd,
  /*#MI        #MN      #MX  #NN  #RA  #DM  #CC-------   #OP---------- */
  idprod, -DBL_MAX, DBL_MAX, NAN, 0.0, 2.0, 0, 0, 0, 0 , 0.0, 0.0, 0.0 ,
  /*#OF---------- */
    0.0, 0.0, -1.0 ,
  /* #VN        #PL           #PD          #LL             #DP   #DD  #DL */
  ALPS_VERSION, PRINT_LENGTH, PRINT_DEPTH, PRINT_LIST_LEN, -1.0, 3.0, 5.0,
  /* #GC FM---------  FS       #CW  #CH  #GW  #GH  #FN  #WD               */
  0.0, 0.0, 0.0, 0.0, 48000.0, 0.0, 0.0, 0.0, 0.0, 1.0, 5.0
};

/* character initialisers */
hcval h_cval =  {
  "G0000000",                                                 /* #GS */
  CAPSTRING                                                   /* #CP */
};

/*ref initialiser indeces. These are indexes into rootable: r.a[8]==r.s.snil */
static const hwval h_wval = 
  { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8 };

/* Interpreter error messages */

static const errentT errTab[] = {
  {no_err, 	"no_err",      "No Error"                        },
  {no_room, 	"no_room",     "Workspace overflow"              },
  {stk_ovfl,	"stk_ovfl",    "Stack overflow"                  },
  {unb_sym, 	"unb_sym",     "Unbound symbol"                  },
  {inv_sym, 	"inv_sym",     "Invalid symbol"                  },
  {inv_exp, 	"inv_exp",     "Invalid expression"              },
  {inv_fun,	"inv_fun",     "No such function defined"        },
  {inv_numarg,	"inv_numarg",  "Wrong number of arguments"       },
  {inv_funarg,	"inv_funarg",  "Improper function argument"      },
  {dom_range,	"dom_range",   "Domain / Range error"            },
  {sys_cor, 	"sys_cor",     "System tables corrupted"         },/*mea culpa*/
  {con_ass,	"con_ass",     "Invalid assignment target"       },
  {not_imp,	"not_imp",     "Feature not implemented"         },
  {bad_num, 	"bad_num",     "Bad numeric constant"            },
  {bad_str,   	"bad_str",     "Bad string constant"             },
  {bad_vec, 	"bad_vec",     "Bad vector"                      },
  {no_files, 	"no_files",    "No available file attachments"   },
  {inv_funame,	"inv_funame",  "Not a proper function name"      },
  {ios_err,  	"ios_err",     "Input/Output/OS error"           },
  {bad_file,	"bad_file",    "File/Directory not found"        },
  {sys_par, 	"sys_par",     "System parameter out of bounds"  },
  {pgmr_inc,	"pgmr_inc",    "Programmer incompetence detected"}, /* mine  */
  {user_err,    "user_err",    "User defined error"              },
  {user_int,    "user_int",    "User interrupt"                  },
  {sys_int,     "sys_int",     "System interrupt"                }
};

static const char *mmacctd[alst+2] = { /* memory management accounting labels */
  "anonymous       ",
  "sym num chr ref ",
  "environment     ",
  "stack           ",
  "continuation    ",
  "task cntl block ",
  "foreign object  ",
  "FS cons cell    ",
  "Unknown         "
};

static const char *dispstr[] = { /* dispatcher state printables */
  "done    ",  "runable ",  "winput  ",  "swait   ",  "yield   ",  "bench   ",
  "mxdsp   "
};

static const char *bufstatstr[] = { /* buffer status printables */
"ldvs_ok", "ldvs_eof", "ldvs_nodata"
 };

static const char *hosts[] = 
  {"Unknown","Android","Cygwin","Cygwin","Linux","Linux",
   "Solaris","Solaris","webOS" ,"MacOS", "OldAndroid", "Android",
   "Linux S/390" };

/* ********** GLOBAL VARIABLE SECTION ********** */

/* global runtime context */
static lispTypes opcd;         /* Lisp Machine operation dispatch code reg   */
static pointer iexp,parm;      /* LM regs for expression function parameters */
static pointer stkp,dest,envp; /* LM regs for stack destination environment  */
static whole   rtflgs;         /* LM runtime flags                           */
       pointer pnil, ptru;     /* high use global "constants"                */
static whole   evald;          /* evaluation depth counter                   */
static whole   envcnt;         /* environment counter for DEEPC BS           */
static pointer runtcb;         /* tcb of currently executing task            */
static pointer lastcb;         /* predecessor of the guy in runtcb           */
static pointer displock;       /* task locking flag for dispatcher           */
static whole   nxttid=0;       /* next task id no (recalculated in boogey)   */ 
/* memory management globals                                                 */
static pointer env_free[num_env_bkts+1];  /* list of free recycled envblks   */
static integer mmacct[alst + 1];  /* memory management accounting block      */
static integer mmbgcnt[alst + 1]; /* Counters of no of calls to boogey       */
       pointer protstk[MAX_PROT]; /* temporary protection stack              */
       integer protind=0;         /* tos index for protection stack          */
static whole   wsp_scale=BIG;     /* workspace scale factor                  */
static whole   fs_size;           /* no. of list nodes (rg_blks)             */
static whole   ctl_depth;         /* control stack depths (ints)             */
static whole   envsize;           /* same as total for all stacks (ints)     */
static whole   wsp_size;          /* Total workspace  size (ints)            */
static whole   wspBsiz;           /* Total size in (bytes)                   */
static whole   pagesize;          /* pagesize of system we are running on    */
static pointer mapaddr;           /* Starting address of memory mapped area  */

/* input/output globals */
static pointer filin,   filout,   filerr;
fibrec *alpsin, *alpsout, *alpserr=NULL;
/* rounding */
number iround = 0.5;        /* we will be rounding up              */
/* Global flags and exit code*/
static whole initFlags = 0;       /* initial flags from argv[]           */
static bool daemonic   = 0;       /* run as socket daemon                */
static bool rawTerm    = 0;       /* true => don't use alpsTermReader    */
static bool ssid       = 0;       /* run as session group leader         */
static bool end_of_program;       /* set when its time to go now         */
static bool shutting_down;        /* set when leaving from main          */
static bool one_shot   = 0;       /* evaluate def_expre once only        */
static bool savewspc;             /* set to save workspace on exit       */
static bool no_notice = 0;        /* print banner and other notices      */
static int  alpsExitCode = 0;     /* Status code passed to exit(2)       */
/* Time related globals */
static integer alpsCPT;           /* clocks per tick      */
static struct itimerval alpsTick; /* alps tick time spec  */
static number uidle;              /* idle time            */
static number ubusy;              /* not idle time        */
static number boo;                /* alps init timestamp  */
/* Signal / timer related globals */
static struct sigaction stats_sa, timer_action, segv_action,
                        term_action, winch_action, int_action;
static struct sigevent stats_sigev;
/* Debug support */
static sigjmp_buf env_main;      /* protection stack                        */ 
static const pointer crashme;    /* null pointer so we can crash in my nash */
static lispTypes eval, eval1;    /* eval nude or trace pc values            */
static char    *myname;          /* invocation programme name aka argv[0]   */
/* global state */
       ctable a;                  /* address table for apl constants    */
       ctable actv;               /* initial values for apl constants   */
static rootable r;                /* address table for root variables   */
static const int no_root_vars = (sizeof(rootable) / indlen);
static pointer s[s_con];          /* address table for builtins -{nil t}*/
static pointer idelem[prim_dyadics]; /* table of real identity elements */
static pointer errSym[last_err];  /* table of error symbols             */
       pointer chartab[MAX_CS];   /* pointers to single char symbols    */
static pointer sstrtab[MAX_CC];   /* pointers to single char constants  */
                                  /* last one is scratch space !!!      */
/* Dispatch table and built-in tcb's */
static pointer disptab[mxdsp];    /* head of dispatcher table by state  */
static pointer mreptcb;           /* main read-eval-print tcb (builtin) */
static pointer iotcb;             /* io call-back tcb FIXME             */
/* global file handling variables, buffers and callbacks */
static idstr   tline;             /* line buffer for system requests    */
static idstr   tline1;            /* ya line buffer for system requests */
static idstr   eline;             /* line buffer for error messages     */
static fcbs    readcb;            /* read callback array                */
static fcbs    writecb;           /* write callback array               */
static fcbs    exceptcb;          /* exception callback array           */
       fibrec *fparr[max_files];  /* pointers to fibrecs indexed by fd  */
static const char   *mTty=NULL;   /* interaction tty  (in_file / opt)   */
       const char   *mHist=NULL;  /* history filename (hist_file / opt) */
static const char   *mLoadF=NULL; /* initial file to load               */
/* math constants */
static  number sqrt2pi;
/* fill element array indexed by vtype for kompress */
static pointer filltab[6];

/* character sets for parsing etc */
static char specChars[] ={dquot, qquot, colon, comma, comment, excape,
			  lpar,  rpar,  lsbrc, rsbrc, lcbrc,   rcbrc,  0};
static char whiteChars[]={ space, eol, '\r', '\v' , '\t',0 };
static char digitChars[]="0123456789";
static char alphaChars[]="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
static char wordChars []={shriek, alps_quad, dollar,  percent, ampersand, star,
			  plus,   slash,     less,    eqval,   great,     quest,
			  spiral, hat,       uscore,  bar,     tilde,     neg,
			  quote,  point,     0};
BIT_MAP(specCS,256);
BIT_MAP(whiteCS,256);
BIT_MAP(digitCS,256);
BIT_MAP(alphaCS,256);
BIT_MAP(symStCS,256);  /* starting chars for symbol */
BIT_MAP(wordCS,256);   /* other chars for symbol    */

#if (STATS)
typedef whole      stctr[s_trailer+1];
typedef pointer    stptr[s_trailer+1];
static stctr starr;               /* stats array                        */
static stctr sttms;               /* stats times                        */
static stptr stent;               /* pointers to entry points           */
static pointer cvcct,appct,trrct,argct,evact,evct,envct,rasct; /* counters*/
#endif

#if (DEBUG)
static whole   iexctr=0;          /* global instructions executed ctr   */ 
static integer tcbring[BB_TCBDEPTH]; /* ring of last n dispatched tcbs  */
static integer tcbrind=0;         /* index into the tcbring             */
#endif

/********************** Forward declarations  **********************/
FORWARD static pointer   allocEnv(integer); 
FORWARD static pointer   allocv(vtype, const integer, pointer);
FORWARD static void      alpsAbend(const char *);
FORWARD static integer   alpsAccept(integer, char *, char *);
FORWARD static void      alpsDispatcher();
FORWARD        void      alpsInitTerm(fibrec *, int);
FORWARD static void      alpsPause();
FORWARD static void     *alpsMapMem(int);
FORWARD static integer   alpsPrint(fibrec *, pointer, bool, lispTypes);
FORWARD static void      alpsPrintVersion();
FORWARD static void      alpsUnmapMem();
FORWARD static integer   alpsWhzat(fibrec *);
FORWARD        void      alpsWinchHandler(int);
FORWARD static void      boogeyMan(const acctp);
FORWARD static integer   checkAxis(pointer , int);
FORWARD static void      closeFiles();
FORWARD static pointer   deqTcb(dispstate);
FORWARD static void      enqTcb(dispstate, pointer);
FORWARD static integer   envDepth(pointer);
FORWARD extern void      error(errors , char *);
FORWARD static pointer   findCtl(tcbTp, integer, lispTypes);
FORWARD static pointer   findStkSyncPoint(tcbTp);
#if (STATS)
FORWARD static void      incCounter(pointer , integer);
#endif
FORWARD static pointer   list(pointer, pointer);
FORWARD static pointer   list3(pointer, pointer, pointer);
FORWARD static pointer   makeAtom(uchar *, integer, const lispTypes);
FORWARD static void      markacc(pointer);
FORWARD static pointer   mkassoc(pointer, pointer, pointer);
FORWARD static pointer   mkCpxFromNum(pointer);
FORWARD static pointer   mkleg(pointer, pointer, pointer);
FORWARD static number    mkrtime();
FORWARD static pointer   prin0(fibrec *, pointer, lispTypes);
FORWARD static pointer   print(fibrec *, pointer, lispTypes);
FORWARD static integer   printFree(fibrec *, integer);
FORWARD static void      protStack(cntTp, bool);
FORWARD static void      resetStack(cntTp, bool);
FORWARD static void      retEnv(pointer); 
FORWARD static void      scheduleTcb(pointer, lispTypes);
FORWARD static void      showEnvStk(fibrec *,pointer);
FORWARD static void      showTcb(fibrec *,pointer);
FORWARD static void      stats_start();
FORWARD static void      stats_stop();
FORWARD static void      stkPush(pointer,lispTypes);
FORWARD static void      unprotStack(cntTp);

/* ********** SYSTEM SUPPORT CODE SECTION ********** */

/********************** System Specials ****************************/

static inline void mkCharSet(char *chars, uchar *set) {
  uint c;
  BMZERO(set,256);
  while ((c=*chars++)) BMSET(set,c);
}

static inline void addCharSet(char *chars, uchar *set) {
  uint c;
  while ((c=*chars++)) BMSET(set,c);
}

static inline number  sqr(number x)  {return (x * x);}
static inline complex csqr(complex x) {return (x * x);}

/* local protection stack, used only within non-preemptable sections */
inline void protect(pointer lex) {
  ASSERT(protind < MAX_PROT);
  protstk[protind++] = lex;
}

inline pointer unprotect() {
  ASSERT(protind > 0);
  return(protstk[--protind]);
}

inline void unprotn(int n) {protind -= n;}

inline void unprotall() {protind = 0;}

/********************** Essential primitives ***********************/

/* basic constant symbol predicates */
inline static bool isQuote(pointer i) {return(i.pt == s[s_quote].pt);}
inline static bool isQquot(pointer i) {return(i.pt == s[s_qquot].pt);}
inline static bool isComAt(pointer i) {return(i.pt == s[s_comat].pt);}

/* Vich type of animal are you ? */
inline static vtype type(pointer lex) { return (vtype)lex.vr->idntt>>FBits; }

/* basic  flag predicates,and setters Caveat Emptor! */
inline static bool isMarked   (pointer lex) {return (lex.vr->idntt &   mrk);}
inline static void mark       (pointer lex) {        lex.vr->idntt |=  mrk; }
inline static void unMark     (pointer lex) {        lex.vr->idntt &= ~mrk; } 
inline static bool isActive   (pointer lex) {return (lex.ev->idntt &   act);}
inline static void setActive  (pointer lex) {        lex.ev->idntt |=  act; } 
inline static void setInactive(pointer lex) {        lex.ev->idntt &= ~act; }
inline static bool isRsvd     (pointer lex) {return (lex.ev->idntt &   rvd);}
inline static void reserve    (pointer lex) {        lex.ev->idntt |=  rvd; }
inline static void unReserve  (pointer lex) {        lex.ev->idntt &= ~rvd; }
inline static bool isHeld     (pointer lex) {return (lex.ev->idntt &   hld);}
inline static void hold       (pointer lex) {        lex.ev->idntt |=  hld; }
inline static void release    (pointer lex) {        lex.ev->idntt &= ~hld; }
inline static bool isBusy (pointer lex) {return (lex.ev->idntt & (rvd | hld));}
/* for runtime flags */
inline static bool isfSet(tflags flg) { return  (rtflgs & flg); }
inline static bool isfClr(tflags flg) { return !(rtflgs & flg); }
inline static void fSet(tflags flg)   { rtflgs |=  flg; }
inline static void fClr(tflags flg)   { rtflgs &= ~flg; }
inline static bool fFlp(tflags flg)   { return ((rtflgs ^=  flg) & flg);}

inline static bool isFS   (pointer i) {
  return((i.wh >=  r.s.fspc.wh) && (i.wh < r.s.fspc_end.wh));}
/* composite type predicates */
inline static bool isNCS (pointer i) {
  return(isNC(i) || isSym(i) || isCym(i)); /*  NCS => num cpx chr or sym */
}  
inline static bool isNCRS (pointer i) {
  return(isNCR(i) || isSym(i) || isCym(i)); /* NCRS => num cpx chr ref or sym */
}  

inline static bool isNumDigit(char c, int radix) {
  return ((c >= '0' && c <= '9') || (c >= 65 && c <= 65 + radix - 11));}

inline static bool isSymbol (pointer i) {return (isSym(i) || isCym(i));}

inline static bool isCon(pointer i) {return(i.at->spl == s_con);}
inline static bool isVar(pointer i) {return(i.at->spl == s_var);}

inline static bool isTrrSubr (pointer i) {
  return ((i.wh > r.s.bbp.bb->bbase.wh)       && 
	  isSym(i)                            && 
	  (evtab[i.at->spl].lispEval == subr) &&
	  (i.at->spl > s_atom) // folks above may call eval
	  );
}

static bool isPrim(pointer i) {
  if (!isSymbol(i)) return false;
  else return ((evtab[i.at->spl].lispEval == subr) ||
	       (evtab[i.at->spl].lispEval == fsubr));
}

inline static bool typeComp(pointer a, pointer b) {
  ASSERT(isAtom(a) && isAtom(b));
  return ((type(a) == type(b)) || (isaChr(a) && isaChr(b)));
}

inline static bool ctypeComp(pointer a, pointer b) {
  ASSERT(isAtom(a) && isAtom(b));
  return ((type(a) == type(b))     || 
	  (isaNum(a) && isaNum(b)) || 
	  (isaChr(a) && isaChr(b)));
}

inline static bool vtypeComp(vtype a, vtype b) {
  return((a == b)                   || 
	 ((a == num) && (b == cpx)) || 
	 ((a == cpx) && (b == num)));
}

inline static vtype vrtype(pointer i) {return(isaChr(i)) ? chr : type(i);}

inline static bool isNotabfun(pointer i)  {
  return(isSym(i) && 
	 (i.at->spl == s_lambda   ||
	  i.at->spl == s_macro    ||
	  i.at->spl == s_fexpr    ||
	  i.at->spl == s_prog     ||
	  i.at->spl == s_progstar ||
	  i.at->spl == s_let      ||
	  i.at->spl == s_letstar  ||
	  i.at->spl == s_defun    ||
	  i.at->spl == s_defexpr  ||
	  i.at->spl == s_defmacro ||
	  i.at->spl == s_mapcar   ||
	  i.at->spl == s_mapcan   ||
	  i.at->spl == s_find     ||
	  i.at->spl == s_while    ||
	  i.at->spl == s_label    ||
	  i.at->spl == s_uwdprot  ||	  
	  i.at->spl == s_catch
	  ));
}

inline static bool isQuadvar(pointer i) {return(*i.at->varadd.sp==alps_quad);}

inline static bool isAplPrim(lispTypes op) {
  return ((s_dist <= op) && (op < s_roll));
}

inline static bool isNumop(lispTypes op) {
  return ((s_dist <= op) && (op <= s_binom));
}


/* global scope result, destination & envp manipulation primitives */

inline static void setEnv(pointer lex) {
  envp = lex;
#if (BINDING==DEEPC)
  envcnt++;
#endif
}

void checkDenv(pointer denv) { /* Validate destination env for valid argp */
  ASSERT(isEnv(denv));
  if (denv.ev->argp.wh) { /* we have an arg pointer */
    if (denv.ev->nargs > 0) { /* we have one or more args so check valid ptr */
      ASSERT((denv.ev->argp.wh >= (denv.wh+(env_rgohd)*rg_len)) &&
	     (denv.ev->argp.wh < (denv.wh+(env_rgohd + nargs(denv))*rg_len)));
    }  else { /* if no args then must be using a list as dest */
      ASSERT(isCons(denv.ev->argp));
    }
  } /* else zero arg pointer ok */
}

static void tcbCache(tcbTp t) { 
    opcd   = t->opcd;
    iexp   = t->iexp;
    parm   = t->parm;
    dest   = t->dest;
    stkp   = t->cntptr.ct->ctlsp; 
    rtflgs = t->tflags;
    evald  = t->evald;
    setEnv(t->envp);
}

static void tcbFlush(tcbTp t) { 
    t->opcd   = opcd;
    t->iexp   = iexp;
    t->parm   = parm;
    t->dest   = dest;
    t->cntptr.ct->ctlsp =  stkp;
    t->envp   = envp;
    t->tflags = (rtflgs & ~pmtFlag); // clear transitory flags
    t->evald  = evald;
}

/**** symrec /vrec field handling ****/

integer dims(pointer lex) {return lex.vr->vardim;}

integer getDim(pointer lex,integer ind) {
#if (RANGE)
  if (ind < 0) error(dom_range,"getDim");
#endif
  if (ind >= lex.vr->vardim) return 0; /* for convenince */
  return(lex.vr->vard[ind]);
}

static inline uchar * getCadd(pointer lex, integer offset) {
  ASSERT(isaChr(lex));
  return &lex.vr->varadd.sp[offset];
}

#if (HOST==SOLARIS32)  || (HOST==SOLARIS64)
inline static pointer pvard(pointer lex) {
  pointer rex;
  rex.ip = lex.vr->vard;
  return(rex);
}
#else
#define pvard(lex) ((pointer)lex.vr->vard)
#endif

integer nels(pointer lex) { return lex.vr->varels; }
pointer vadd(pointer lex) { return lex.vr->varadd; }

/* symbol value handling NOTE: does not respect binding for DEEP[C] */
void    setVal(pointer lex, pointer val) {lex.at->value = val;}
pointer getVal(pointer lex) {return lex.at->value;}

#if (BINDING==DEEPC)
inline static void cacheSymVal(pointer sym, pointer val) { 
  sym.at->cval = val;
  sym.at->cenv  = envcnt;
}
#else
#define cacheSymVal(x,y)
#endif

#if (BINDING==DEEP || BINDING==DEEPC)
static pointer rlookup(pointer sym) {  /* raw tells it as it is */
  ASSERT(!isNil(sym));
  pointer lex,rex;
  int i,lim;
  
#if (BINDING==DEEPC)
  INCCT(cvcct,0);
  if (sym.at->cenv == envcnt) {
    INCCT(cvcct,1);
    return sym.at->cval; /* cached value cell */
  }
#endif

  for (lex = envp; !isNil(lex); lex=nenv(lex)) {
    lim = nargs(lex)-1;
    rex.rg = &lex.ev->args[lim];
    for (i=lim;i>=0;i--,rex.rg--) 
      if (isEqq(rex.rg->ar,sym)) {
	cacheSymVal(sym,rex.rg->dr);
	return(rex.rg->dr);
      }
  }
#if (BINDING==DEEPC)
  if (sym.at->value.wh) cacheSymVal(sym,sym.at->value);
#endif
  return sym.at->value;
}

static pointer slookup(pointer sym) {  /* slot lookup */
  ASSERT(!isNil(sym));
  pointer lex,rex;
  int i,lim;
  for (lex = envp; !isNil(lex); lex=nenv(lex)) {
    lim = nargs(lex)-1;
    rex.rg = &lex.ev->args[lim];
    for (i=lim;i>=0;i--,rex.rg--) 
      if (isEqq(rex.rg->ar,sym)) {
	cacheSymVal(sym,rex.rg->dr);
	return((pointer)&rex.rg->dr);
      }
  }
  if (sym.at->value.wh) cacheSymVal(sym,sym.at->value);
  return (pointer)&sym.at->value;
}
#endif // DEEP

#if (BINDING==SHALLOW)
inline static pointer rlookup(pointer sym) {return sym.at->value;}
inline static pointer slookup(pointer sym) {return (pointer) &sym.at->value;}
#endif

pointer lookup(pointer sym) { 
  pointer rex = rlookup(sym);
  if (!rex.wh) error(unb_sym,(char *)mkString(sym,eline,"Unbound Symbol"));
  return rex;
}

inline static pointer nlookup(pointer sym) { 
  pointer rex = rlookup(sym);
  if (!rex.wh) return pnil;
  return rex;
}

number getNumVal(pointer lex) {
  return (lookup(lex).vr->varadd.np[0]);
}

number *getNumVec(pointer lex) {
  return (lookup(lex).vr->varadd.np);
}

void setNumval(pointer lex, number val) {
  lookup(lex).vr->varadd.np[0] = val;
}

/*retrieve and set system constant value*/
//inline static pointer hvalue(pointer lex) { return lookup(lex).vr->varadd; }

#if (BINDING==DEEP || BINDING==DEEPC)
void setGSym(pointer sym, pointer val) {sym.at->value = val;}
void setSym(pointer sym, pointer val) {
  pointer lex,rex;
  int i;
  for (lex = envp; !isNil(lex); lex=nenv(lex)) {
    rex.rg = &lex.ev->args[0];
    for (i=0;i<nargs(lex);i++,rex.rg++) 
      if (isEqq(rex.rg->ar,sym)) {
	rex.rg->dr   = val;
	cacheSymVal(sym,val);
	return;
      }
  }
  sym.at->value = val;
  cacheSymVal(sym,val);
}
#endif /* DEEP */

#if (BINDING==SHALLOW)
inline void setSym(pointer sym, pointer val) {sym.at->value = val;}
static void setGSym(pointer sym, pointer val) {
  pointer lex  = envp;
  pointer olex = pnil;
  pointer rex;
  int i;
  if (sym.at->value.wh) {
    for (lex = envp; !isNil(lex); lex=nenv(lex)) {
      rex.rg = &lex.ev->args[0];
      for (i=0;i<nargs(lex);i++,rex.rg++) 
	if (isEqq(rex.rg->ar,sym)) olex = rex;
    }
    if (isNil(olex)) sym.at->value = val;
    else olex.rg->dr = val;
  } else {
    sym.at->value = val;
  }
}

#endif

inline static bool checkPW(int n) {return(n<max_line_len);} /* print width */

/************** Low Level input output Section ***************/

fibrec  *getFil(pointer lex) {return(lex.fb->fobpvt.fi);}
pointer getFname(pointer lex) {return(lex.fb->name);}

inline static bool isEof(pointer f) {return (getFil(f)->bufstat == ldvs_eof);}

fibrec *cin()  { return(getFil(lookup(a.v.hrf))); }
fibrec *cout() { return(getFil(lookup(a.v.hpf))); }
fibrec *cerr() { return(getFil(lookup(a.v.hlf))); }

static void alpsRead(int fd) { /* select callback for files */
  pointer lex;
  int len,ilen;
  fibrec *fp  = fparr[fd];
  lex         = fp->ibuf;
  ilen        = nels(lex);
  if (fp->isatty == 2) {
    alpsBlockSignals();
    len = read(fp->fd,vadd(lex).sp,ilen);
    alpsUnblockSignals();
  } else {
    len = read(fp->fd,vadd(lex).sp,ilen);
  }
  fp->ecci    = len;
  fp->ifpos  += len;    
  fp->ilpos   = 0;
  fp->bufstat = (len > 0) ? ldvs_ok : ldvs_eof;
}

#define KEEP_NEWLINE 0
pointer alpsReadLine(fibrec *fp) {
  integer i,ilen,llen,mlen,mpos;
  uchar  *buf;
  pointer lex;
  lex  = fp->ibuf;
  ilen = nels(lex);
  buf  = vadd(lex).sp;
  if (fp->bufstat == ldvs_eof) return pnil;
 findLine:
  mpos = fp->ilpos;
  mlen = fp->ecci;
  for (i=mpos;i<mlen;i++) {
    if (buf[i] == '\n') {
      llen          = i - mpos + KEEP_NEWLINE;
      fp->ilpos     = i+1;
      return mkChr(&buf[mpos],llen);
    }
  }
  if (mpos < mlen) {/* shuffle left */
    memcpy(buf,&buf[mpos],mlen-mpos);
    mpos = mlen - mpos;              
  } else mpos = 0;
  mlen = read(fp->fd,&buf[mpos],ilen-mpos);
  if (mlen <= 0) {
    if ((mlen == 0) && (fp->isatty == 1)) goto findLine;
    fp->bufstat = ldvs_eof;
    if (mpos > 0) {
      fp->ilpos = 1;
      fp->ecci  = 0;
      return mkChr(buf,mpos);
    } else return pnil;
  }
  fp->ecci  = mpos+mlen;
  fp->ilpos = 0;
  goto findLine;
}

uchar *mkString(pointer p, idstr buf, char *name) {
  integer len;
  char  ebuf[max_line_len];
  if (!isaChr(p)) {
    strncpy(ebuf,name,max_line_len-1);
    error(inv_funarg,strncat(ebuf," not a string",max_line_len-1));
  }
  len = nels(p);
  if (len >= max_line_len){
    strncpy(ebuf,name,max_line_len-1);
    error(inv_funarg,strncat(ebuf," too long",max_line_len-1));
  }
  memcpy(buf,vadd(p).sp,len);
  buf[len] = 0;
  return buf;
}

static void mkGString(pointer p, uchar *buf) {// with tilde expansion
  char *mEnv;
  int l,j=0;
  if (((l=nels(p))>=2) && !memcmp(vadd(p).sp,"~/",2)) {
    mEnv = getenv("HOME");
    j  = strlen(mEnv);
    if (j+l >= max_line_len) error(inv_funarg,"Name too long");;
    memcpy(buf,mEnv,j);
    memcpy(&buf[j],&vadd(p).sp[1],l-1);
    buf[l+j-1] = 0;
  }   else mkString(p,buf,"file/dir name");
}

static integer getChar(fibrec *inp) {
  if (inp->pbbi) return(inp->pbb[--inp->pbbi]);
  if ((inp->ilpos >= inp->ecci) && (inp->bufstat != ldvs_eof)) {
    inp->bufstat = (inp->fob.fb->type == filt) ? ldvs_nodata : ldvs_eof;
    inp->ilpos = inp->ecci = 0;
    return -1;
  }
  if (inp->bufstat == ldvs_eof)  return -1;
  inp->ifpos++;
  return (uchar)inp->ibuf.vr->varadd.sp[inp->ilpos++];
}

static integer nextChar(fibrec *inp) { /* peek ahead */
  if (inp->pbbi) return(inp->pbb[inp->pbbi-1]);
  if ((inp->ilpos >= inp->ecci) && (inp->bufstat != ldvs_eof)) {
    inp->bufstat = (inp->fob.fb->type == filt) ? ldvs_nodata : ldvs_eof;
    inp->ilpos = inp->ecci = 0;
    return -1;
  }
  if (inp->bufstat == ldvs_eof)  return -1;
  return inp->ibuf.vr->varadd.sp[inp->ilpos];
}

static void putbackChar(fibrec *inp, char c) {
#if (DEBUG)
  alpsOutsn(alpsout,3,"Putting back ",itos((int) c),"\n");
  alpsFlush(alpsout);
#endif
  inp->pbb[inp->pbbi++] = c;
}

static void chow_blanks(fibrec *inp, char c) {
  uchar tchar;

  while ((inp->bufstat == ldvs_ok)         &&
	 ((tchar = nextChar(inp)) <= space) &&
	 (tchar != c))                          tchar = getChar(inp);
}

static int chuck_line(fibrec *inp) {
  while (inp->bufstat != ldvs_eof) {
    if (inp->bufstat == ldvs_ok) {
      if (nextChar(inp) == eol) return 0;
    } else return 1;
    getChar(inp);
  }
  return 0; /* eof == eol */
}

static int wintct = 0; // debug write interrupt counter

void alpsFlush(fibrec *out) {
  pointer lex;
  uchar *wbuf;
  int len,wlen;
  lex = out->obuf;
  if (out->fob.fb->type == buft) return;
  if ((out->fdstat != fout) && (out->fdstat != fio))
     error(ios_err,"can't write");
  len = out->ecco;
  out->ecco = 0; /* buffer index empty */
  wbuf = vadd(lex).sp;
  while (len > 0) {
    wlen = write(out->fd,wbuf,len);
    if (wlen < 0) {
      if (errno == EINTR) {
	wintct++;
      } else error(ios_err,"alpsFlush");
    } else {
	len  -= wlen;
	wbuf += wlen;
    }
  }
}

void alpsOutc(fibrec *out,int c) {
  pointer lex = out->obuf;
  if (out->ecco == nels(lex)) alpsFlush(out);
  if (out->ecco == nels(lex)) error(dom_range,"buffer overflow");
  out->obuf.vr->varadd.sp[out->ecco++] = c;
  if (c == eol) {
    out->olpos = 0 ;
    if (out == alpserr) alpsFlush(out); /* need to know this ASAP */
  } else out->olpos++;
}

void alpsOutn(fibrec *out,int c, int n) {
  integer i;
  for (i=0;i<n;i++) alpsOutc(out,c);
}

void alpsOutcn(fibrec *out,int n, ...) {
  integer c,i;
  va_list ap;

  va_start(ap,n);
  for (i=0;i<n;i++) {
    c = va_arg(ap, int);
    alpsOutc(out,c);
  }
  va_end(ap);
}

void alpsTab(fibrec *out,integer t) {
  integer n;

  if (t < out->olpos) { alpsOutc(out,eol);  }
  if (t > out->olpos) {
    n = t - out->olpos;
    alpsOutn(out,space,n);
  }
}

void alpsOutb(fibrec *out, uchar *b, integer len) {
  int i;
  for (i=0;i<len;i++) alpsOutc(out,b[i]);
}

void alpsOuts(fibrec *out, const char *s) { /* write string */
  integer len = strlen(s);
  alpsOutb(out,(uchar *)s,len);
}

void alpsOutsn(fibrec *out,int n, ...) {
  integer i;
  va_list ap;

  va_start(ap,n);
  for (i=0;i<n;i++) alpsOuts(out, va_arg(ap, char *));
  va_end(ap);
}

void alpsOutsln(fibrec *out, char *s) { /* write string + a newline */
  alpsOuts(out,s);
  alpsOutc(out,eol);
}

void alpsOutln(fibrec *out) { /* nl if not at the beginning of a line */
  if (out->olpos) alpsOutc(out,eol);
}

static int unEscLineLen(char *val, integer llen) {
  /* len removing excape characters as required for buildChr */
  integer s=0,d=0;
  while (s < llen) {
    if (val[s++] == excape) s++;
    d++;
  }
  return d;
}

static pointer alps_make_prompt(char *p) {
  return buildChr(p,unEscLineLen(p,strlen(p)));
}

/******************** Debug Support Stuff ****************/

static void freeze(tcbTp task) { displock.tb = task; }

static void thaw(void) {
  pointer task;
  if (!isNil(displock)) {
    displock = pnil;
    /* send all the lads on the bench back onto the field */
    while (!isNil(task = deqTcb(bench))) enqTcb(runbl,task);
  }
}

#define alpsInfo(s) alpsOutsln(alpserr,("alps Info: " s))

static pointer setDbg() {
  pointer lex;
  eval  = s_teval; eval1 = s_teval1;
  fSet(dbgFlag);
  return(ptru);
}

static pointer clrDbg() {
  eval  = s_sys_eval; eval1 = s_sys_eval1;
  fClr(dbgFlag);
  return(pnil);
}

static void dprt(fibrec *out,pointer lex, integer len) { /* debug printer */
  number opl,opd,oll,ofw,opp; /* save users usual stuff */
  if (!lex.wh) { 
    alpsOuts(out,"**dprt Null pointer**");
  } else {
    opl = getNumVal(a.v.hpl);
    opd = getNumVal(a.v.hpd);
    oll = getNumVal(a.v.hll);
    ofw = getNumVal(a.v.hfw);
    opp = getNumVal(a.v.hpp); 
    setNumval(a.v.hpl,(number)len);
    setNumval(a.v.hpd,getNumVal(a.v.hdd));
    setNumval(a.v.hll,getNumVal(a.v.hdl));
    setNumval(a.v.hfw,0.0);
    setNumval(a.v.hpp,getNumVal(a.v.hdp));
    alpsPrint(out,lex,true,s_nop);
    setNumval(a.v.hpl,opl);
    setNumval(a.v.hpd,opd);
    setNumval(a.v.hll,oll);
    setNumval(a.v.hfw,ofw);
    setNumval(a.v.hpp,opp);
  }
}

static void dprtln(fibrec * out,pointer lex, integer len) { /* debug printer */
  dprt(out,lex, len);
  alpsOutln(out);
  alpsFlush(out);
}

static char taschar(pointer lex) {
  whole tp = type(lex);
  if (isNil(lex)) return('^'); /* nil */
  if (isFS(lex)) return('L');
  if (tp < sizeof(typetab)) return(typetab[tp]);
  else return('?');
}

static void showCtl(fibrec * out, pointer cont) {
  pointer lex, olex,base,sp;
  integer cnt;
  char ic,ch;

  cnt = 0;
  base = cont.ct->stack;
  sp   = cont.ct->ctlsp;
  ic   = space;
  lex  = sp;
  while (!isEqq(lex,base) && ic != 'q') {
    if (*lex.wp > r.s.wspc.wh && *lex.wp < r.s.wspc_end.wh)    {
      olex = *lex.pt;
      //
      ch = taschar(olex);
    } else {
      if (*lex.wp <= s_trailer) {
#if (DEBUG)
	alpsOutsn(out,3,"\ns_", evtab[*lex.wp].lispName,":");
	ch = space;
#endif
	if (*lex.wp == s_teval1) {
	  alpsOutsn(out,2,"\n",(char *)itos(cnt));
	  dprt(out,lex.pt[-1],72); /* should be iexp */
	  cnt++;
	  ic =  (cnt % 20) ? space : getChar(alpsin);
	}
      } else ch = '!';
    }
#if (DEBUG)
    if (ch != space) {
      alpsOutc(out,ch);
      if (ch == 's' || ch == 'L') {
	alpsOutc(out,'=');
	dprt(out,olex,30); 
	alpsOutc(out,';');
      }
    }
#endif
    lex.wp--;
  }
  alpsOutln(out);
  alpsFlush(out);
}

static void printPcRing(fibrec *out,pointer lex) {
#if (DEBUG)
  int i,j;
  alpsOutsn(out,7,"\n  LPoPe  ",  itos(lex.tb->iexctr)," ",
	    evtab[(integer)lex.tb->lpope].lispName," ",
	    itos(lex.tb->lpictr),"\n");
  j = lex.tb->pcrind;
  for (i=0;i<BB_PCDEPTH;i++) {
    alpsOutsn(out,2," ", evtab[lex.tb->pcring[j++]].lispName);
    if (j == BB_PCDEPTH) j=0;
  }
#endif
}

static void printTcb(fibrec *, pointer);

void alpsCrash(const char *test, const char *file,
		      uint line, const char *fun) {
  integer i,j,k;
  alpsMsg(9,"ASSERT(",test,") failed in ",fun,"() at ",file,":",
	   itos(line),"\n");
  if (!isNil(runtcb)) {
    printTcb(alpserr,runtcb);
    showCtl(alpserr,runtcb.tb->cntptr);
  }
  alpsMsg(1,"\nCrashing...\n");
  k = *crashme.ip;
  exit(k); // not reached. Prevent poor old crashme from being optimised out
}

void showError(tcbTp t) {
  if (t->errflg == no_err) {
    //    alpsOutsn(alpserr,2,t->errinf,"\n");
    goto showError_out;
  }
  alpsOutsn(alpserr,2,(char *)errTab[t->errflg].errmes,": ");
#if (DEBUG)
  if ((t->eopcd <= s_trailer) & (t->tflags & dbgFlag))
    alpsOutsn(alpserr,2,evtab[t->eopcd].lispName," ");
#endif
  alpsOutsn(alpserr,2,t->errinf,"\n");
  if (t->errflg == ios_err)
    alpsOutsn(alpserr,3,"OS error: ",strerror(t->serrno),"\n");
#if (DEBUG)
  dprtln(alpserr,iexp,80);
#endif
  if (!(t->tflags & dbgFlag) && t->errflg != user_err) {
    if (!isNil(t->errexp)) dprtln(alpserr,t->errexp,80);
    else  alpsOutsn(alpserr,3,"opcode was: ",evtab[t->eopcd].lispName,"\n");
  }
 showError_out:
  alpsFlush(alpserr);
}


void error(errors code, char *info) {
  pointer p1,eexp,lex;
  if (info == NULL) info = "";
  if (!isNil(runtcb)) {
    runtcb.tb->errflg = code;
    runtcb.tb->errinf = info;
    runtcb.tb->serrno = errno;
    tcbFlush(runtcb.tb);               /* ya gat ta flush befoa leavin        */
    p1   = findCtl(runtcb.tb,0,eval1); /* find last eval frame for error iexp */
    eexp =  (isNil(p1)) ? pnil : p1.pt[-1]; /* error iexp */
    runtcb.tb->errexp = eexp;
    runtcb.tb->eopcd  = runtcb.tb->opcd;   /* where was the error signaled   */
  }
  unprotall();           /* no more protection either      */
  if ((code == sys_cor) || (code == pgmr_inc) ) {
    if (!isNil(runtcb)) {
      showTcb(alpserr,runtcb);
      showError(runtcb.tb);
    }
    alpsWhzat(alpserr);
    printFree(alpserr,1);
    alpsFlush(alpserr);
    ASSERT(false); // if not debugging Crash in my Nash for post mortem
  }
  if (!isNil(runtcb)) siglongjmp(env_main,true);
  alpsOutsln(alpserr,"No task running");
}

void alpsMsg(int n, ...) { /* write error message to stderr */
  integer i,len,ind=0;
  uchar  *buf;
  char   *c;
  va_list ap;
  idstr emline;
  buf = emline;
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
  i = write(2,buf,ind);
}

void alpsError(const char *errmess) {
  if (errno)
    alpsMsg(5,"alpsError: ",errmess," sys: ", strerror(errno),"\n");
  else 
    alpsMsg(3,"alpsError: ",errmess,"\n");
}

static void alpsExit(int n) {
  if (alpsout) closeFiles();
  exit(n);
}

#if (SOUND)
pointer alloc_sb(int len, int nchan) {
  pointer ret;
  alpsOutsn(alpsout,5,"alloc_sb ",itos(len), "  " , itos(nchan), "\n");
  if (nchan == 1) r.s.lds.ip[0] = len;
  else if (nchan == 2) {r.s.lds.ip[0] = 2; r.s.lds.ip[1] = len;}
  else error(inv_funarg,"nchan out of bounds");
  ret = allocv(num,nchan,r.s.lds);
  return ret;
}
#endif

#if (STATS)
static pointer make_stats(void) {
  integer li;
  pointer lex,cp,tp;

  li  = (integer)s_nop;
  lex = getfs((int)s_trailer);
  protect(lex);
  while (!isNil(lex)) {
    protect(cp = mkNum(starr[li]));
    protect(tp = mkNum(sttms[li]));
    lex.rg->ar = list3(stent[li],cp,tp);
    unprotn(2);
    starr[li] = 0;
    sttms[li] = 0;
    li++;
    lex = lex.rg->dr;
  }
  lex = unprotect();
  return(lex);
}

static pointer mkCounter(char *name, integer size) {
  pointer ctr,pcp,lex,res;
  ctr = makeAtom((uchar *)name,strlen(name),s_con);
  pcp = getVal(a.v.hpc); // lookup current performance counters
  lex = assoc(res,pcp); // find him among the current performace counters
  if (isNil(lex)) {
    r.s.lds.ip[0] = size;
    res = allocv(num, 1, r.s.lds);
    setSym(a.v.hpc,mkassoc(ctr,res,pcp));
  } else res = cdr(lex);
  return res;
}

static void incCounter(pointer ctr, integer w) {
  ASSERT(isNum(ctr) && (w >= 0));
  if  (w >= nels(ctr)) w = nels(ctr) - 1;
  ctr.vr->varadd.np[w] += 1;
}

static void resetCounter(pointer ctr) {
  integer i;
  ASSERT(isNum(ctr));
  for (i=0;i<nels(ctr);i++) ctr.vr->varadd.np[i] = 0;
}

static void makeCounters() {
#if (BINDING==DEEPC)
  cvcct = mkCounter("cvc",4);
#endif
  trrct = mkCounter("trr",4);
  appct = mkCounter("apply",11);
  evact = mkCounter("evargs",10);
  argct = mkCounter("argcnt",16);
  evct  = mkCounter("eval",14);
  envct = mkCounter("env",7);
  rasct = mkCounter("rasc",Max_FB_Loop+1);
  actv.v.hpc = getVal(a.v.hpc);
}

void bumpRasc(int n) { incCounter(rasct,n); }
#else
void bumpRasc(int n) {}
static void makeCounters() {}
#endif

/* ***************** MEMORY MANAGEMENT SECTION ****************** */

static void init_partn(pointer bbp, pointer add, integer size) {
  integer remain;

  if ((remain = (ALIGN - (add.wh & (ALIGN -1))))) {
    add.wh += remain;
    size   -= remain;
  }
  bbp.bb->dtag_sz = 0;
  *add.wp         = 0;
  add.wh         += ALIGN;
  size           -= ALIGN;
  bbp.bb->bbase   = add;
  bbp.bb->bnext   = add;
  bbp.bb->bprev   = add;
  bbp.bb->groover = add;
  add.bk->bsize   = size;
  add.bk->bnext   = bbp;
  add.bk->bprev   = bbp;
  add.wh         += size;
  *add.wp         = 0;
  bbp.bb->broof   = add;
  add.wh         -= indlen;
  *add.wp         = size;
  bbp.bb->bbsz    = size;
}

static const int alc_adjust  = (indlen * 3); /*linkage, top size, bottom size */
static const whole min_alloc = (vrec_ohd + (indlen * 4));
static pointer alc(integer size, acctp acc) {
  /*blk_hd + 1 for trailing tag  could try 3*/
  pointer add,lex, rex;
  whole remain;
  bool flag, latch;

#ifdef ALCBUG
  if (alpserr) // alc called before initFiles is done 
    alpsOutsn(alpserr,5,"alc: size ",itos(size),
	      ", type ",mmacctd[acc],"\n"); 
#endif
  if (size <= 0) {
    alpsOutsn(alpserr,3,"alc: Requested size not positive: ",itos(size),"\n");
    return crashme;
  }
  size  += alc_adjust;
  remain = size % ALIGN;
  if (remain != 0) size += ALIGN - remain;
  flag   = false;
  latch  = false;
  lex    = r.s.bbp.bb->groover; /* where to start looking for free stuff */
  while (true) {
    /* if we hit the list header skirt around it and remember we were there */
    if (lex.pt == r.s.bbp.pt) {
      if (flag) {  /* hit it again! no space available */
	if (latch) {   /* this time there is no hope */
	  lex.wp = NULL;
	  alpsOutsn(alpserr,4,"alc: No room for ",mmacctd[acc],itos(size),"\n");
	  // printFree(alpserr,1);
	  return crashme;
	}
	boogeyMan(acc);   /*try and find something*/
	latch = true;   /*i have done everything i can for you (almost)*/
      }
      flag = true;
      lex  = r.s.bbp.bk->bnext;
      continue;
    } else {  /* does size fit in this block ? */
      rex.wh = lex.wh + lex.bk->bsize - indlen; /* pointer to bottom size tag */
      if (rex.bk->bsize != lex.bk->bsize) 
	alpsOuts(alpserr,"alc: bad tags\n");
      if (size <= lex.bk->bsize) {
        /* yes.. found a free block that is big enough */
	r.s.bbp.bb->groover = lex.bk->bnext;
	remain              = lex.bk->bsize - size;
	if (remain < min_alloc) {   /* for small remaining fragment */
	  size                    = lex.bk->bsize;
	  rex                     = lex.bk->bprev;
	  rex.bk->bnext           = lex.bk->bnext;
	  lex.bk->bnext.bk->bprev = rex;
	} else {  /* create new free entry out of remaining fragment */
	  lex.bk->bsize = remain;
	  lex.wh       += remain;
	  rex.wh        = lex.wh - indlen;
	  rex.bk->bsize = remain;
	}
	/* set bsize fields in allocated block */
	lex.bk->bsize     = -size;
	rex.wh            = lex.wh + size - indlen;
	rex.bk->bsize     = -size;
	lex.bk->bnext.pt  = NULL;     /* zap link */
	add.wh            = lex.wh + alcbk_ohd;   /*adjust for user part*/
	/*note: bounce over bsize and bnext for trashlist linkage*/
#ifdef ALCBUG
	ASSERT(isEqq(add,(pointer)&lex.bk->bprev));
	ASSERT(!(lex.wh & (ALIGN - 1))); /* die if unaligned */
	if (alpserr) // alc called before initFiles is done 
	  alpsOutsn(alpserr,7,"alc: size ",itos(size)
		    ," @",itox(lex.wh), " user add ",itox(add.wh),"\n"); 
#endif
	mmacct[acc] += size;
        return add;
      } else {  /* no.. this block not big enough  try next block */
	lex = lex.bk->bnext;
	continue;
      }
    }  /*size check*/
  }  /*while true*/
}

static bool rtn(pointer add,acctp acc) {
  pointer lex, rex, tex;
  whole retsize;
#if (MMDBUG!=None)
  ASSERT(!isEnv(add));
#endif
  retsize = -add.bk->bsize;
  lex.wh  = add.wh + retsize;
  rex.wh  = lex.wh - indlen;

#if (RANGE)
  if (add.wh <  r.s.bbp.bb->bbase.wh || retsize <= 0  ||
      rex.wh >= r.s.bbp.bb->broof.wh || rex.bk->bsize != add.bk->bsize)  {
    alpsOutsn(alpserr,7,"Bad rtn! @",itox(add.wh)," sz ",itos(add.bk->bsize),
	      " rsz ",itos(rex.bk->bsize),"\n");
    return(false);
  }
  /* catch free while in use situations early */
  tex.wh = add.wh + alcbk_ohd;
  bzero(tex.vd, retsize - alcbk_ohd - indlen);
#endif

  mmacct[acc] -= retsize;
  rex.wh       = add.wh - indlen;
  if (lex.bk->bsize <= 0) {  /* next block busy */
    if (rex.bk->bsize <= 0) {
      /* prev block busy and next block busy (worst case) */
      tex               = r.s.bbp.bk->bnext;
      add.bk->bnext     = tex;
      add.bk->bprev     = r.s.bbp;
      r.s.bbp.bk->bnext = add;
      tex.bk->bprev     = add;
    } else {  /* prev block free but next block busy */
      retsize += rex.bk->bsize;
      add.wh  -= rex.bk->bsize;
    }
  } else {  /* next block free */
    retsize += lex.bk->bsize;
    if (lex.pt == r.s.bbp.bb->groover.pt) r.s.bbp.bb->groover = r.s.bbp;
    if (rex.bk->bsize > 0) {  /* prev block also free */
      retsize                += rex.bk->bsize;
      add.wh                 -= rex.bk->bsize;
      lex.bk->bprev.bk->bnext = lex.bk->bnext;
      lex.bk->bnext.bk->bprev = lex.bk->bprev;
    } else {  /* prev block not also free */
      add.bk->bnext           = lex.bk->bnext;
      add.bk->bprev           = lex.bk->bprev;
      lex.bk->bprev.bk->bnext = add;
      lex.bk->bnext.bk->bprev = add;
    }
  }
  /* adjust tag_sizes */
  add.bk->bsize = retsize;
  add.wh       += retsize - indlen;
  add.bk->bsize = retsize;
  return true;
}

pointer getfs(integer n) {
  pointer res, lex,olex;

  integer j = 1;
  if (n == 0) return pnil;
  if (r.s.nfsfree<n) {
#if (MMDBUG==High)
    alpsOutsn(alpserr,7,"not enuff FS for ",itos(n)," only got ",
	      itos(r.s.nfsfree)," in ",evtab[(integer)opcd].lispName,"\n");
#endif
    boogeyMan(alst);
    if (r.s.nfsfree < n) error(no_room,"getls");
  }
  /*find first date */
  while ((r.s.fffsi<fs_size) && (BMSETP(r.s.fs_bm.sp,r.s.fffsi))) r.s.fffsi++;
  ASSERT(r.s.fffsi < fs_size); // waste of cycles but we are circumspect
  res.rg = r.s.fspc.rg + r.s.fffsi;
  BMSET(r.s.fs_bm.sp,r.s.fffsi);
  r.s.fffsi++;                // movin' on
  olex = lex = res;
  lex.rg->ar  = pnil; /* some let boogey in before setting all the cars */
/* TODO: for large n do 8 or more at a time */
  while ((j < n) && (r.s.fffsi < fs_size)) {
    if (BMCLRP(r.s.fs_bm.sp,r.s.fffsi)) {
      lex.rg      = r.s.fspc.rg + r.s.fffsi;
      BMSET(r.s.fs_bm.sp,r.s.fffsi);
      r.s.fffsi++;            // mooooovinooooon
      j++; 
      lex.rg->ar  = pnil; /* some let boogey in before setting all the cars */
      olex.rg->dr = lex;
      olex        = lex;
    } else r.s.fffsi++;
  }
  r.s.nfsfree -= n;
  lex.rg->dr = pnil; // terminate list for the lazy ones
  ASSERT((r.s.fffsi <= fs_size) && (j == n)); // (fffsi==fs_size) => nfsfree==0
  return res;
}

inline static int getBkSiz(pointer lex) { 
  lex.wh -= alcbk_ohd; 
  return labs(lex.bk->bsize);
}

static void vrecLink(pointer lex) { /* link to vreched list (death-row) */
  lex.wh       -= alcbk_ohd;
  lex.bk->bnext = r.s.vreched;
  r.s.vreched   = lex;
}

static pointer allocv(vtype idt, const integer vdim, pointer vds) {
 /*try to alloc vrec*/
  integer len, i, vsiz, vreclen, noels;
  pointer kof;
  vrec *vrecp;

  noels = 1;  /*calculate total number of elements*/
  for (i = 0; i < vdim; i++)   noels *= vds.ip[i];
  ASSERT((idt > 0) && (idt <= ref));
  len     = lentab[idt];   /* length of data type to be stored */
  vsiz    = len * noels;
  if (vsiz==0 && vdim==1) {
    switch (idt) {
    case num:
    case cpx: return r.s.nonum;
    case chr: return r.s.nostr;
    case ref: return r.s.noref;
    default:  error(pgmr_inc,"allocv");
    }
  }
  vreclen = vrec_ohd + vdim * indlen;  /* overhead */
#if (ALIGN > indlen)                   /* assume vrec_ohd always aligned     */
  if (vdim & 1) vreclen += indlen;     /* for odd number of dims add padding */
#endif
  vsiz   += vreclen;
  kof     = alc(vsiz,avrc);
  if (!kof.wh) error(no_room,"allocv");
  vrecLink(kof);    /* just born and already on the trash candidate list */
  vrecp            = kof.vr;
  vrecp->idntt     = idt<<FBits;
  vrecp->vardim    = vdim;
  vrecp->varels    = noels;
  vrecp->varadd.wh = kof.wh + vreclen;
  for (i = 0; i < vdim; i++)  vrecp->vard[i] = vds.ip[i];
  return(kof);
}  /*try to alloc vrec*/

static void retv(pointer lex) { /* return a vrec to the pool */
  lex.wh = lex.wh - alcbk_ohd; /* adjust for rtn */
  ASSERT (r.s.vreched.wh == lex.wh); /* can only call this right after allocv */
  r.s.vreched = lex.bk->bnext;
  rtn(lex,avrc);
}

static pointer mkNullV(vtype idt) {
  pointer kof;
  integer vreclen = vrec_ohd + indlen; /* one dimension no elements          */
#if (ALIGN > indlen)                   /* assume vrec_ohd always aligned     */
  vreclen += indlen;                   /* for odd number of dims add padding */
#endif
  ASSERT(idt == num || idt == chr || idt == ref);
  kof = alc(vreclen,avrc);
  if (!kof.wh) error(no_room,"mkNullV");
  kof.vr->idntt     = idt<<FBits;
  kof.vr->vardim    = 1; /* a vector         */
  kof.vr->varels    = 0; /* with no elements */
  kof.vr->vard[0]   = 0;
  kof.vr->varadd.wh = 0; /* death to him who looks into the null vector */
  return(kof);
}

static pointer allocSymbol(char *name, integer llen, lispTypes stype) {
  pointer kof;
  struct symrec *arp;
  integer vsiz;

  vsiz   = llen * lentab[chr];
  vsiz  += symrec_ohd;
  kof    = alc(vsiz,avrc);
  if (!kof.wh) error(no_room,"allocSymbol");
  vrecLink(kof);
  arp            = kof.at;
  arp->idntt     = sym<<FBits;
  arp->varels    = llen;
  arp->varadd.wh = kof.wh + symrec_ohd;
  arp->vardim    = 1;
  arp->dimlen    = llen; /* vard[0] */
  arp->plist     = pnil;
  arp->value.wh  = 0;
  arp->sysp      = crashme;
  arp->spl       = stype;
  memcpy(arp->varadd.sp,name,llen);
#if (BINDING==DEEPC)
  arp->cenv = ~0;
#endif
  return(kof);
}

pointer allocCV(integer len) {
  pointer pd;
  pd.ip = &len;
  return allocv(chr,1,pd);
}

pointer mkChr(const uchar *t, integer len) { /* raw conversion */
  pointer rex;
  if (len==0) return(r.s.nostr);
  if (len==1) return(sstrtab[*t]);
  rex = allocCV(len);
  memcpy(vadd(rex).sp,t,len);
  return(rex);
}

static inline uchar escChar(uchar c) {
  switch (c) {
  case 'n': c = eol;  break;
  case 'q': c = 28;   break; // <CNTRL>-backslash
  case excape: 
  case dquot: break;
  default: 
    if (c >= 64 && c < 96) c = c-64;
  }
  return c;
}

pointer buildChr(const char *val, integer llen) { /* handle escapes */
  integer i; 
  pointer res;
  uchar *ptr,c;

  if (llen==0) return(r.s.nostr);
  if (llen==1) { /* Note llen is actual result string size sans // */
   if (excape == (c = *val++)) c = escChar(*val++);
   return sstrtab[c];
  }
  res = allocCV(llen); /* add of name vect in name */
  ptr = vadd(res).sp;
  for (i = 0; i < llen;i++)  {
    if (excape == (c = *val++)) c = escChar(*val++);
    ptr[i] = c;
  }
  return(res);
}

/*symbol stuff*/
static integer hash(const uchar *iline, integer len) {return (*iline);}

pointer oblis_head(const uchar *iline, integer len) {
  pointer rex;
  rex.rg = &r.s.ospc.rg[hash(iline, len)];
  return rex;
}

integer strcompare(char *s1, integer l1, pointer lex) {
  integer l2 = nels(lex);
  uchar  *s2 = vadd(lex).sp;
  integer p  = 0;
  integer q  = (l1 < l2) ? l1 : l2;

  while ((p < q) && (*s1 == *s2)) {s1++; s2++; p++;};
  return (p == q) ? l1 - l2 : *(uchar*)s1 -  *s2;
}

static pointer makeAtom(uchar *iline, integer len, const lispTypes stype) {
  pointer lex, olex, res, root, obent, obhed;
  integer comp,s;

  for (s=0; s<len; s++)
    if (!isWord(iline[s]) && !isSpec(iline[s])) error(inv_sym,(char *)iline);
  obhed = oblis_head(iline,len);
  root  = olex = lex = car(obhed);
  comp  = 1; /* in case empty list */
  /* scan the list til found or line < list entry  */
  while ((!isNil(lex)) && (comp = strcompare((char *)iline,len,car(lex))) > 0) {
    olex = lex;
    lex  = lex.rg->dr;
  }
  if (comp == 0) return(car(lex));
  /* make new entry in oblis */
  obent = getfs(1);
  if (isEqq(olex,root) && isEqq(lex,root)) {
    obent.rg->dr = olex;
    obhed.rg->ar = obent;
  } else {
    obent.rg->dr = olex.rg->dr;
    olex.rg->dr  = obent;
  }
  res   = allocSymbol((char *)iline,len,stype);
  obent.rg->ar = res;
  return(res); // sendres
}

/********** Environment block handling ************/


/* env block structure
--------->---------------
 	  | idntt| nargs|
 	  |------+------|
 	  | nenv |   o--+--+
 	  |------+------|  |  argp destination slot pointer
          | arg1 | val1 |  |
       	  |------+------|  |
       	     ........	 <-+  points to cdr of slot
          |------+------|
          | argn | valn |
          ---------------

   nenv points to the next env block in the control context */

#if (DEBUG)
static whole mEvctr  = 0; /* serial number */
#endif

static whole envzomb = 0;       /* DEBUG */

static pointer allocEnv(integer n) { /* number of args */
 pointer lex,olex,rex;
 integer i,j,emax,siz,rsiz,bkt;
 bool boogey_been = false;
#ifdef ENVTRACE
alpsOutsn(alpserr,5,
	  "allocEnv(",itos(n),")  in ",evtab[(integer)opcd].lispName,"\n");
#endif
 INCCT(envct,0);
 bkt = (n < num_env_bkts) ? n : num_env_bkts;
 allocenv_again:
/* find free block in re-use zone */
 if ((bkt < num_env_bkts) && (!isNil(lex = env_free[bkt]))) {
   env_free[bkt] = nenv(lex);
#ifdef ENVTRACE
   alpsOutsn(alpserr,3,"found exact @",itox(lex.wh),"\n");
#endif
 } else { /* look in big block bucket (nargs >= ORD(num_env_bkts)) */
   for (olex = lex = env_free[num_env_bkts]; !isNil(lex); lex = nenv(lex)) {
     if (nargs(lex) == n) { /* got exact fit (not bloody likely) */
       if (lex.pt == env_free[num_env_bkts].pt) {
	 env_free[num_env_bkts]  = nenv(lex);
       } else  snenv(olex, nenv(lex)); /* olex.ev->nenv = nenv(lex) */
#ifdef ENVTRACE
       alpsOutsn(alpserr,3,"got exact @",itox(lex.wh),"\n");
#endif
       break;
     }
     siz = n + env_rgohd;     /* We are working in sizes of rg_blk units  */
     if ((emax=nargs(lex)) >= n) {          /* got one that's big enough  */
       if ((rsiz = (emax - n)) < env_rgohd) {
	 /* remainder too small to use - we now have a zombie            */
	 if (lex.pt == env_free[num_env_bkts].pt) { /* unlink lex        */
	   env_free[num_env_bkts]  = nenv(lex);
	 } else  snenv(olex, nenv(lex));
	 rex.wh = lex.wh + (n + env_rgohd) * rg_len; /* rex is remainder */
	 /* zombie fragment at rex has only idntt and nargs available    */
	 bzero(rex.pt,rsiz*rg_len); /* DEBUG */
	 j = rsiz - env_rgohd;       /* negative nargs                   */
	 ASSERT(j<0);
	 rex.ev->idntt = env<<FBits;
	 rex.ev->nargs = j;          /* negative size is zombie marker   */
#if (MMDBUG>=Low)
	 alpsOutsn(alpserr,7,"alloc_env fragment ",itos(j)," @",itox(rex.wh),
		   " size is ",itos(rsiz),"\n");
#endif
	 envzomb++;
	 /* zombie under lex to restore him when lex is returned  */
       } else {   /* have proper fragment with at least 0 args             */
	 rex = lex;                           /* rex is the remainder      */
	 lex.wh = rex.wh + (emax - n)*rg_len; /* candidate at end of block */
	 j = rex.ev->nargs = emax - siz;      /* patch remaining size      */
	 ASSERT(j>=0);
	 if (j < num_env_bkts) {      /* unlink and put rest in its bucket */
	   if (rex.pt == env_free[num_env_bkts].pt) {
	     env_free[num_env_bkts]  = nenv(rex);
	   } else  snenv(olex, nenv(rex)); 
	   snenv(rex,env_free[j]);
	   env_free[j] = rex;
	 } else { /* else rex is still in big bucket list */
	   ASSERT(isEnv(rex) && (isNil(nenv(rex)) || isEnv(nenv(rex))));
	 }
	 r.s.env_blks++;
#ifdef ENVTRACE
	 alpsOutsn(alpserr,3,"created @",itox(lex.wh),"\n");
#endif
       }
       break;
     }
     olex = lex;
   } /* for (olex = lex..) */
 } /* big block bucket */
 if (isNil(lex)) {
#ifdef ENVTRACE
   alpsOuts(alpserr,"allocEnv: need to call boogey\n");
#endif
   if (!boogey_been) {
     boogeyMan(aenv);
     boogey_been = true;
     goto allocenv_again;  /* comment for bug */
   }
   error(no_room, "allocEnv"); /* nothing more to be done for now */
 }
 /* initialise fresh env blk */
 lex.ev->idntt = env<<FBits; /* set type  also inactive  unreserved  */
 lex.ev->nargs = n;          /* only useful for fresher              */
 snenv(lex,pnil);            /* clean link                           */
 lex.ev->argp.wh = 0;        /* kill any attempts to assign here     */
#if (DEBUG)
 for (i=0;i<n;i++) lex.ev->args[i].ar.wh = lex.ev->args[i].dr.wh = 0;
 lex.ev->owner = opcd;         /* debug */
 lex.ev->serno = mEvctr++;
 ASSERT(isEnv(lex));
#else 
 for (i=0;i<n;i++) lex.ev->args[i].ar.wh = 0; // just symbols FWIW
#endif
 return lex;
}

inline static void retEnv(pointer lex) {/*return what you have stolen from me*/
  integer bkt;
#if (MMDBUG!=None)
  ASSERT(isEnv(lex));
  ASSERT(nargs(lex) >= 0);
#endif
#if (MMDBUG==High)
  pointer rex;
  for (bkt=0;bkt<=num_env_bkts;bkt++)
    for (rex=env_free[bkt];!(isNil(rex));rex=nenv(rex))
      if (isEqq(rex,lex)) {
	alpsOutsn(alpserr,7,"From ",evtab[(integer)opcd].lispName,
		  " retEnv n= ",itos(bkt)," double return ", itox(lex.wh),"\n");
#if (DEBUG)	
	alpsOutsn(alpserr,4," ownr = ",evtab[lex.ev->owner].lispName,
	          " serno= ",itos(lex.ev->serno));
	printPcRing(alpserr,runtcb);
#endif

	return;
      }
#endif
  INCCT(envct,1);
  bkt = (nargs(lex) < num_env_bkts) ? nargs(lex) : num_env_bkts;
  snenv(lex,env_free[bkt]);
  env_free[bkt] = lex;
  /* TODO recover zombie: not really worth it since boogey will clean them up */
#ifdef ENVTRACE
  alpsOutsn(alpserr,5,"retEnv(",itos(nargs(lex)),") @ ",itox(lex.wh),"\n");
#endif
}

static void initEnv() { /* initialise environment blocks world */
  integer i;

  /* set up free lists for env blocks */
  for (i=0;i<num_env_bkts;i++) env_free[i] = pnil; /* init free list buckets */
  env_free[num_env_bkts] = r.s.envbase; /* link big block on default list   */
  r.s.envbase.ev->idntt  = env<<FBits;  /* even he needs right type         */
  r.s.envbase.ev->nargs  = envsize/2 - env_rgohd; /* num args he would have */
  snenv(r.s.envbase, pnil);                       /* terminate list         */ 
  r.s.env_blks  = 1;    /* counter of total number of env blks in play */
  envcnt        = 0;    /* environment counter for deep binding        */
  envp          = pnil; /* Initialise global environment pointer       */
}

static pointer allocDest(integer n) {
  pointer lex;
  lex = allocEnv(n);
  snenv(lex,dest);
 return lex;
}

inline static void initDest(pointer mDest) { /* env as destination */
  if (mDest.ev->nargs) {
    mDest.ev->argp.pt = &mDest.ev->args[0].dr;
    *mDest.ev->argp.pt = pnil; /* make sure he starts out clean */
  }
}

pointer mkParmBlock(int n, ...) { /* make a parm block */
  integer i;
  va_list ap;
  pointer parm = allocEnv(n);
  va_start(ap,n);
  for (i=0;i<n;i++) {
    sargn(parm, i, va_arg(ap, pointer));
  }
  va_end(ap);
    return parm;
}

static void showFlags(fibrec * out, pointer lex) {
  integer i;
  whole j = 1 << (FBits - 1);
  if (!lex.wh || !isAtom(lex)) return;
  for(i=FBits-1;i>=0;i--,j>>=1) alpsOutc(out,(j & *lex.wp) ? flagtab[i] : '-');
}

static void showEnv(fibrec * out,pointer p1) {
  integer i,lim = nargs(p1);
  pointer lex;
  alpsOutsn(out,5,"Env@ ",itox(p1.wh)," nargs = ",itos(lim)," flags = ");
  showFlags(out,p1);
  alpsOutln(out);
#if (DEBUG)
  if (p1.ev->argp.wh) {
    alpsOutsn(out,4," argp = ",itox(p1.ev->argp.wh)," argn = ",itos(warg(p1)));
  }
  alpsOutsn(out,4," ownr = ",evtab[p1.ev->owner].lispName,
	    " serno= ",itos(p1.ev->serno));
#endif
  alpsOutln(out);
  for (i = 0; i < lim; i++) {
    lex.rg = &p1.ev->args[i];
    if (lex.rg->ar.wh) {
      alpsTab(out,2);
      dprt(out,lex.rg->ar,16); // symbol
      alpsTab(out,16);
	alpsOuts(out," = ");
	dprt(out,getVal(lex.rg->ar),60); // value
	showFlags(out,getVal(lex.rg->ar));
    }
    alpsOuts(out," <=> ");
    dprt(out,lex.rg->dr,80);
    showFlags(out,lex.rg->dr);
    alpsOutln(out);
  }
}

static void showEnvStk(fibrec * out,pointer p1) {
  while (!isNil(p1)) {
    showEnv(out,p1);
    p1 = nenv(p1);
  }
}

/* Continuation block structure memory management */

static void initCnt() { 
  r.s.cnt_busy.wp = r.s.cnt_free.wp = NULL;
  r.s.cnt_alloc   = 0;
}
static int getStkSize(pointer lex) {
  return(lex.ct->stksiz + lex.ct->stkext);
}

static void protStack(cntTp lex, bool ext) {
  pointer protadd;
  unprotStack(lex);
  /* calaculate aligned protection address with extension */
  protadd.wh = (lex->stack.wh + lex->stksiz) & ~(pagesize - 1);
  if (ext) {                   /* use extension ?    */
    protadd.wh    += pagesize; /* yes add extra page */
    lex->extflg = true;
  } else {
    lex->extflg = false;
  }
  lex->stklim = protadd;  /* set actual stack limit */
#if (MMDBUG==High)
  alpsMsg(5,"protStack  lim ",itox(lex->stklim.wh),
	  "  pagesize = ",itos(pagesize),"\n");
#endif
  if (mprotect(lex->stklim.vd, pagesize,PROT_READ) == -1)
    alpsAbend(mkEmes(6,myname,"  m(un)protect failed at  ",
		       itox(lex->stklim.wh)," ",strerror(errno),"\n"));
  lex->mprotf = true; 
}

static void unprotStack(cntTp lex) {
  if (lex->mprotf) {
#if (MMDBUG==High)
    alpsMsg(3,"unprotstck lim ",itox(lex->stklim.wh),"\n");
#endif
    if (mprotect(lex->stklim.vd, pagesize,PROT_READ | PROT_WRITE) == -1)
       alpsAbend(mkEmes(6,myname,"  m(un)protect failed at  ",
			itox(lex->stklim.wh)," ",strerror(errno),"\n"));
    lex->mprotf = false;
  }
}

static void checkStkExt(cntTp lex) { /* check if extension can be dropped */
  pointer protadd;
  if (lex->extflg) {
    protadd.wh = (lex->stack.wh + lex->stksiz) & ~(pagesize - 1);
    if (stkp.wh < protadd.wh) {
      protStack(lex,false); /* drop extension */
    }
  }
}

static void resetStack(cntTp lex, bool protect)  {
  lex->ctlsp     = lex->stack;
  *lex->stack.pt = pnil;
  lex->extflg    = false;  // not on stack extension
  unprotStack(lex);
  lex->stklim.wh = lex->stack.wh + lex->stksiz;
  if (protect) protStack(lex, false); /* no extension, also sets tex->slim */
}

static void retStack(cntTp lex) {
  pointer rex = lex->stack;
  unprotStack(lex);
  rex.wp--;
  ASSERT(isStk(rex));
  rex.wh = rex.wh - alcbk_ohd; // adjust for rtn
  rtn(rex,astk);
}

/* Check and initialise stack for continuation lex */
static void checkStack(cntTp lex, integer stackSize, bool protect) {
  pointer rex;
  integer actualSize,old = lex->stksiz;
  /* Add two pages for stack memory protect fence */
  int extsz  = (protect*2*pagesize);
  int reqSize = stackSize + extsz;
  if (old < stackSize) {
    if (old > 0) retStack(lex); /* release old stack */
    rex = alc(reqSize + indlen, astk); /* Get us a new one */
    if (!rex.wh) error(no_room,"Stack");
    actualSize = getBkSiz(rex) - alcbk_ohd - 2*indlen;
    *rex.wp++      = stk<<FBits; // set id and adjust
    lex->stack  = rex;
    lex->stksiz = stackSize;
    lex->stkext = extsz;
    resetStack(lex, protect);
#if (MMDBUG==High)
    alpsMsg(9,"allocCnt: new stack ",itos(actualSize),
		" requested ",itos(reqSize),
		" Old stack ",itos(old),
		" prot = ",itos(protect),"\n");
#endif
  }
}

static pointer mkCnt() {
  pointer lex;
  lex = alc(sizeof(cntT),acnt);
  if (!lex.wh) error(no_room,"allocCnt boogey");
  r.s.cnt_alloc++;
  lex.ct->idntt  = cnt<<FBits;
  lex.ct->stksiz = 0;  /* we have no stack */ 
  lex.ct->mprotf = false;
  lex.ct->extflg = false;
  return lex;
}

#undef CNTCACHE 
static pointer allocCnt(integer stackSize, bool protstk) {
  pointer lex;
  if ((lex.wp = r.s.cnt_free.wp)) {  // r.s.cnt_free not NULL => take it
    r.s.cnt_free = r.s.cnt_free.ct->next;
  } else {
    if (r.s.cnt_alloc == maxstacks) {
      boogeyMan(acnt);
#ifdef CNTCACHE
      if ((lex.wp = r.s.cnt_free.wp)) r.s.cnt_free = r.s.cnt_free.ct->next;
      else error(no_room, "allocCnt maxstack");
#else
      if (r.s.cnt_alloc == maxstacks) error(no_room, "allocCnt maxstack");
      else lex = mkCnt();
#endif
    } else lex = mkCnt();
  }
  lex.ct->next = r.s.cnt_busy;
  r.s.cnt_busy = lex;
  protect(lex);
  checkStack(lex.ct,stackSize,protstk); // no need to protect lex: r.s.cnt_busy is scanned
  unprotect();
  return (lex);
}

static void retCnt(pointer lex) {
  pointer rex,tex;
  bool ret;
#ifdef CNTCACHE
  lex.ct->next = r.s.cnt_free;  // cache continuation with its stack (if any)
  r.s.cnt_free = lex;
  return;
#else
  ASSERT(isCnt(lex));
  if (lex.ct->stksiz) retStack(lex.ct);/* release stack */
  rex.wh = lex.wh - alcbk_ohd;    
  ret    = rtn(rex,acnt);    // control stack recovered independently
  r.s.cnt_alloc--;
#endif
}

/* Cnt methods for outside of bigEval scope */
inline static  void stkPush(pointer lex,lispTypes lab) {
  ASSERT(lex.ct->ctlsp.wh < (lex.ct->stklim.wh - indlen));
  *++lex.ct->ctlsp.wp = lab;
}

inline static  void stkPushp(pointer lex ,pointer p) {
  ASSERT(lex.ct->ctlsp.wh < (lex.ct->stklim.wh - indlen));
  *++lex.ct->ctlsp.pt = p;
}

inline static integer stkSize(cntTp c) {  
  return(c->ctlsp.wh + indlen - c->stack.wh); // include space for tos entry
}

static integer stkDepth(cntTp c)    {  return(stkSize(c)/indlen);       }

/* Foreign Object control block structure memory management */

pointer allocFob(fobtp   type,   pointer name,
			pointer data1,	pointer data2,	integer size) {
  pointer lex;
  integer fsize;
  fsize = sizeof(fobrec) + size;
  lex   = alc(fsize,afob);
  if (!lex.wh) error(no_room,"allocFob");
  lex.fb->idntt     = fob<<FBits;   /* its a foreign object! */
  lex.fb->next      = r.s.fob_busy;
  lex.fb->type      = type;    
  lex.fb->name      = name;         /* will be marked by boogey */
  lex.fb->fobpvt.wh = (lex.fb->fobsize=size) ? lex.wh+sizeof(fobrec) : 0;
  lex.fb->data1     = data1;
  lex.fb->data2     = data2;
  r.s.fob_busy      = lex;
  return (lex);
}

static void retFob(pointer lex) {
  pointer rex;
  bool    ret;
  rex.wh = lex.wh - alcbk_ohd;    // move pointer back for $rtn
  ret    = rtn(rex,afob);         /* its all over baby blue... */
}

static void filMarker(pointer lex) {
  fibrec *fex = getFil(lex);
  if (fex->ibuf.wh) markacc(fex->ibuf);;
  if (fex->obuf.wh) markacc(fex->obuf);
  if (fex->isatty == 1) { /* terminal handling extras */
    // hp and cp point into the history list which is marked thru a.v.hhs
    markacc(fex->ubuf); // undo buffer always allocated 
    markacc(fex->ybuf); // yank buffer aweso
  }
}

static void filSweeper(pointer lex) {
  fibrec *fex = getFil(lex);
  if (fex->fdstat != fclos) {
    alpsOutsn(alpserr,3,"filSweeper closing ",itos(fex->fd),"\n");
    alpsCloseF(lex);
  }
  /* ibuf and obuf if not marked elsewhere will be swept by boogeyMan */
}


static void resetFilNState(fibrec *lex) { /* number parsing state variables */
  lex->nstart   = 0;
  lex->nind     = 0;
  lex->ndigs[0] = 0;
  lex->ndigs[1] = 0;
  lex->ndigs[2] = 0;
  lex->hadpoint = 0;
  lex->hadexp   = 0;
  lex->radix    = 0;
  lex->hadneg   = 0;
  lex->hadeneg  = 0;
  lex->hadcpx   = 0;
}

static void resetFil(fibrec *lex) {
  lex->bufstat = ldvs_nodata;
  lex->ifpos   = 0;
  lex->ilpos   = 0;
  lex->ecci    = 0;
  lex->olpos   = 0;
  lex->ecco    = 0;
  lex->alen    = 0;
  lex->qqdepth = 0;
  lex->pbbi    = 0;
  resetFilNState(lex);
}

pointer allocFil(pointer fname) {
  pointer lex;
  fibrec *fex;

  protect(fname);
  lex = allocFob(filt,fname,crashme,crashme,sizeof(fibrec));
  unprotect();
  fex          = getFil(lex);   /* pointer to fibrec */
  fex->fob     = lex;  /* pointer to fob    */
  fex->fd      = -1;
  fex->fdstat  = fclos;
  fex->ibuf.pt = NULL;
  fex->obuf.pt = NULL;
  fex->isatty  = 0;
  resetFil(fex);
  return lex; /* return fob */
}

static void bufMarker(pointer lex) {
  fibrec *fex = getFil(lex);
  if (fex->ibuf.wh) markacc(fex->ibuf);;
  ASSERT(fex->ibuf.wh == fex->obuf.wh);
}

static pointer allocBuf(pointer lex) {
  pointer res;
  fibrec *fex;

  protect(lex);
  res = allocFob(buft,s[s_buf],crashme,crashme,sizeof(fibrec));
  unprotect();
  fex = res.fb->fobpvt.fi;   /* pointer to fibrec */
  fex->fob     = res;  /* pointer to fob    */
  fex->fd      = -1;
  fex->fdstat  = fio;
  fex->ibuf    = lex;
  fex->obuf    = lex;
  resetFil(fex);
  return res; /* return fob */
}

inline static int filfd(fibrec *fex) {return(fex->fd);}

/* Task control block memory management */

static void setTcbStkLim(tcbTp t) {
  t->slim.wh= t->cntptr.ct->stklim.wh-indlen; /* for CKSTK */
}

static void resetTcb(tcbTp t, bool resetStk, pointer result) {
  t->opcd   = s_done;
  t->iexp   = pnil;
  t->parm   = pnil;
  t->dest   = t->defd;
  initDest(t->defd);
  if (resetStk) resetStack(t->cntptr.ct,true); /* enable stack protection */
  setTcbStkLim(t);
  t->base   = t->cntptr.ct->stack;
  t->envp   = t->envbase;
  t->fres   = result;   /* final res, set in Dispatcher done processing */
  t->evald  = 0;
  t->gccnt  = 0;
  t->stime  = 0.0;
  t->tcbwq  = pnil;
  t->filwq  = pnil;
  t->errexp = pnil;
  t->errflg = no_err;
  t->errinf = "";
  t->serrno = 0;
}

#define TCB_MIN_PRI 15
#define TCB_LOW_PRI 13
#define TCB_MED_PRI 10
#define TCB_HI_PRI   5
#define TCB_SYS_PRI  4
#define TCB_MAX_PRI  0

static pointer allocTcb(int stackSize) {
  pointer lex, tdest, tcnt;
  tdest = allocEnv(1); /* only one slot to receive the result from sendRes() */
  initDest(tdest);
  protect(tdest);
  tcnt = allocCnt(stackSize*indlen,true); /* enable stack protection  */
  protect(tcnt);
  lex = alc(sizeof(tcbT),atcb);
  unprotall();
  if (!lex.wh) error(no_room,"allocTcb");
  lex.ct->idntt     = tcb<<FBits; 
  lex.tb->next      = pnil;
  lex.tb->tid       = nxttid++;
  lex.tb->priority  = TCB_LOW_PRI; /* low priority by default */
  lex.tb->tflags    = 0;           /* start out inconspicuous */
  lex.tb->state     = mxdsp;       /* stateless               */
  lex.tb->defd      = tdest;/* must pre-allocate otherwise tcb mark hits    */
  lex.tb->cntptr    = tcnt; /* uninitialised fields below and in resetTcb() */
  lex.tb->envbase   = pnil;
  lex.tb->utime     = 0.0;
  lex.tb->iexpq     = pnil;
#if (DEBUG)
  lex.tb->iexctr    = 0;
  lex.tb->pcrind    = 0;
  lex.tb->lpope     = s_done;
  lex.tb->lpictr    = 0;
  for (int i = 0; i<BB_PCDEPTH;i++) lex.tb->pcring[i] = s_nop;
#endif
  resetTcb(lex.tb,false,pnil); /* do not reset stack, was done in allocCnt */
  return(lex);
}

static void retTcb(pointer lex) {
  pointer rex;
  bool    ret;
  rex.wh = lex.wh - alcbk_ohd;    
  ret    = rtn(rex,atcb);
  // control stack and defd recovered independently when not marked anymore
  ASSERT(ret);
}

static void enqTcb(dispstate d, pointer lex) {
  pointer rex,orex;
  lex.tb->state = d;
  if (isNil(disptab[d])) {
    lex.tb->next  = disptab[d];
    disptab[d]    = lex;
    goto exit;
  }
  switch (d) {
  case done: /* first come first served for done queue */
    for (rex=disptab[done];!isNil(rex.tb->next);rex=rex.tb->next);
    rex.tb->next = lex;
    lex.tb->next = pnil;
    break;
  case runbl: /* priority queuing only for runbl */
    for (orex=rex=disptab[runbl];!isNil(rex);orex=rex,rex=rex.tb->next) {
      if (lex.tb->priority < rex.tb->priority) {
	if (isEqq(rex,disptab[runbl])) {
	  lex.tb->next   = rex;
	  disptab[runbl] = lex; /* go to the top of the class me lad */
	} else {
	  orex.tb->next = lex;
	  lex.tb->next  = rex;
	}
	goto exit;
      }
    }
    /* last in the class */
    orex.tb->next = lex;
    lex.tb->next  = pnil;
    break;
  default:
    lex.tb->next = disptab[d];
    disptab[d]   = lex;
    break;
  }
 exit:
  return;
}

static pointer deqTcb(dispstate d) {
  pointer lex = disptab[d]; // deq head of list
  if (!isNil(lex)) disptab[d] = lex.tb->next;
  return(lex);
}

static pointer remTcb(dispstate d, pointer ent) { /* remove ent from q d */
  pointer lex,olex;
  for (olex=lex=disptab[d];!isNil(lex);lex=lex.tb->next) {
    if (isEqq(lex,ent)) {
      if (isEqq(lex,olex)) disptab[d]=lex.tb->next;
      else olex.tb->next = lex.tb->next;
      return lex;
    } else olex = lex;
  }
  return lex;
}

static void sendTcb(pointer task, pointer mexp, pointer menv) {
  
  if (task.tb->state == done) { /* feed work to the slacker */
      remTcb(done,task);          /* take him off the list    */ 
      task.tb->iexp    = mexp;
      task.tb->envp    = menv;
      task.tb->envbase = menv;
      scheduleTcb(task,eval);     /* and whip him into action */
    } else { /* queue up work for active chaps */
      pointer x = task.tb->iexpq;
      if (isNil(x)) task.tb->iexpq = mkleg(mexp,menv,pnil);
      else {
	while (isCons(cdr(x))) x = cdr(x);
	x.rg->dr = mkleg(mexp,menv,pnil);
      }
    }
}

static void dropqTcb(pointer lex) {
  pointer rex;
  // alpsOutsn(alpsout,3,"dropq: state ",itos(lex.tb->state),"  tcb ");
  //printTcb(alpsout,lex);
  //alpsFlush(alpsout);
  rex = remTcb(lex.tb->state,lex);
  ASSERT(isEqq(lex,rex));
}

integer getTcbTid(pointer tt) {return tt.tb->tid; }

static pointer getTcbRespt(pointer tt) { return(tt.tb->dest.ev->argp); }

static pointer getTcbRes(pointer tt) { 
  if (tt.tb->dest.ev->argp.pt) return(*tt.tb->dest.ev->argp.pt); 
  else return (pointer)NULL;
}

void scheduleIOCB(pointer op, pointer args, int disp) {
  pointer p,cbexp;
  protect(args);
  protect(p = list(s[s_quote],args));
  cbexp  = list(op, p);
  unprotall();
  /* send work to iotcb to run lisp callback */
  sendTcb(iotcb,cbexp,pnil);

  if (disp)  {
    /* Sadly we need this here since EZ_WGL loops in slider widget
     until button release event */
    alpsDispatcher();
  }
}

static void setTcbRes(pointer tt, pointer rr) {*tt.tb->dest.ev->argp.pt = rr; }

static void setTcbStack(tcbTp lex, pointer stk) {
  lex->cntptr.ct->ctlsp = stk;
}

static void printTcb(fibrec *out, pointer lex) {
  alpsOutsn(out,8,itosn(lex.tb->tid,4),itosn(lex.tb->priority,4),
	    itosn(envDepth(lex.tb->envp),10),
	    itosn(stkDepth(lex.tb->cntptr.ct),10),
	    itosn(lex.tb->evald,10),
	    rtot(lex.tb->utime*TICKS/1000)," ",
	    evtab[(integer)lex.tb->opcd].lispName);
  printPcRing(out,lex);
  alpsOutln(out);
}

static void showTcb(fibrec *out, pointer lex) {
  alpsOutsn(out,3,"tid : ",itos(lex.tb->tid),"\n");
  alpsOutsn(out,3,"envp: ",itox(lex.tb->envp.wh),"\n");
  alpsOutsn(out,3,"envb: ",itox(lex.tb->envbase.wh),"\n");
  alpsOuts(out,"iexp: "); dprtln(out,lex.tb->iexp,32);
  alpsOuts(out,"parm: ");
  if (isList(lex.tb->parm))   dprtln(out,lex.tb->parm,32);
  else                        showEnv(out,lex.tb->parm);
  alpsOuts(out,"dest: ");     showEnv(out,lex.tb->dest);
  alpsOuts(out,"res : "); dprtln(out,getTcbRes(lex),72);
  alpsOutln(out);
}

/********** Friends of the boogey man aka Tokoloshe *******/

/*  ACHTUNG variables foa da boogey man  */
static bool in_boogey = false; /* true when he is wandering the streets */
static int boogey_norefs;  /* non address objects seen     */
static int boogey_ecnt;    /* environments marked          */
static int boogey_cnt;     /* control stack ents marked    */
static int boogey_tcb;     /* task control blocks marked   */
static int boogey_fcnt;    /* foreign objects marked count */
static int boogey_sym;     /* symbols marked count         */
static int boogey_lst;     /* fs marked count              */
static int boogey_vrec;    /* vreched list length          */
static int boogey_vgn;     /* marked virgin count          */
static int boogey_mxrdp;   /* max markacc recursion depth  */

static void bdb() { /* boogey debug landing pad */
  int i;
  i = 0;
}

#if (GRAF==1)
int widMarker(pointer lex) {
  ASSERT(isWidget(lex));
  if (isMarked(lex)) return 1;
  // alpsOutsn(alpserr,3,"widMarker marking",itox(lex.wh),"\n");
  mark(lex);
  return 0;
}

static void markTopWdgt(pointer lex) {
  while (!isNil(lex.fb->data1)) lex = lex.fb->data1; /* find top parent */
  markWidgetTree(lex.fb->data2); /* mark whole tree */
}

void widSweeper(pointer lex) {
  alpsDestroyWidget(lex);
}
#endif

inline static void markFob(pointer lex) {
  boogey_fcnt++;
  markacc(lex.fb->name);  /* preserve name */
  if (isFil(lex)) filMarker(lex); /* call fob's marker */
  else if (isBuf(lex)) bufMarker(lex);
#if (GRAF)
  else if (isWidget(lex)) {
    markTopWdgt(lex);
    return;
  }
#endif
  mark(lex);
} 

inline static bool isFSmarked(pointer lex) {
  integer index = (lex.wh - r.s.fspc.wh) / rg_len;
  return(BMSETP(r.s.fs_bm.sp,index)); 
}

inline static int unMarked(pointer rex) {
  return ((isCons(rex) && !isFSmarked(rex)) || (isAtom(rex) && !isMarked(rex)));
}

inline static void markSym(pointer lex) {
#if (MMDBG != None)
  ASSERT(isSym(lex);
#endif
 mark(lex);
 boogey_sym++;
 markacc(lex.at->plist);   /*dont forget the property list*/
 /*       dprtln(out,lex,20); */
 if (lex.at->value.wh) markacc(lex.at->value);
}

static void markEnv(pointer emark) { /* FIXME: Optimise */
  int i,m,n,touched=0;
  int zz;
#if (MMDBUG==High)
    alpsOutsn(alpserr, 9,"Marking envblk #",itos(boogey_ecnt)," @",
	      itox(emark.wh)," for nargs=", itos(n)," argp=",
	      itox(emark.ev->argp.wh), "\n");
#if (DEBUG)	
	alpsOutsn(alpserr,4," ownr = ",evtab[emark.ev->owner].lispName,
	          " serno= ",itos(emark.ev->serno));
#endif
    /* showEnv(alpserr,emark); */
#endif
  ASSERT((emark.wh >= r.s.envbase.wh) && 
	 (emark.wh < r.s.envlim.wh)   && isEnv(emark));
  /* Go down the env stack marking as we go */
  while (!isNil(emark) && !isMarked(emark)) {
#if (MMDBUG==High)
    {pointer rex;
    int bkt;
    /* Paranoia check whether env being marked is part of the free envs */
  for (bkt=0;bkt<=num_env_bkts;bkt++)
    for (rex=env_free[bkt];!(isNil(rex));rex=nenv(rex))
      if (isEqq(rex,emark)) {
	alpsOutsn(alpserr,3,"markEnv free env ", itox(emark.wh),"\n");
  }}
#endif
    n = emark.ev->nargs;
    mark(emark);
    touched++;
    boogey_ecnt++;
    zz = boogey_norefs;
    m = ((n > 0) && (emark.ev->argp.wh)) ? warg(emark) + 1 : n;
    for (i=0; i<m; i++) {
      /* for funargs the symbols in the env may be unbound in oblis
         so would not be marked. So mark them here */
      if (symn(i, emark).wh)
	markacc(symn(i, emark));
      if (argn(i, emark).wh) 
	markacc(argn(i, emark)); 
    }
#if (MMDBUG==High)
    ASSERT(m<=n);
    if (emark.ev->argp.wh) 
      if (unMarked(*emark.ev->argp.pt)) {
	dprtln(alpserr,*emark.ev->argp.pt,50);
	showEnv(alpserr,emark);
	markacc(*emark.ev->argp.pt);
      }
#endif
    if (zz != boogey_norefs) { /* we had some weirdos in env */
      alpsOutsn(alpserr,9,"Env @",itox(emark.wh)," nargs ",itos(n),
		" argp ",itox(emark.ev->argp.wh),
		" argp index ",itos(m),"\n");
#if (DEBUG)
      alpsOutsn(alpserr,5,
		" ownr = ",evtab[emark.ev->owner].lispName,
		" serno= ",itos(emark.ev->serno),"\n");
#endif
      for (i=0;i<n;i++) 
	alpsOutsn(alpserr,4, itos(i)," ",itox(argn(i,emark).wh),"\n");
    }
    emark = nenv(emark);
  }
#if (MMDBUG==High)
  alpsOuts(alpserr, "...done Markenv \n ");
  /*  showEnvStk(emark); */
  alpsOutsn(alpserr,2,itosn(touched,10)," env blocks mkd.\n");
#endif
}

inline static void markStack(pointer base, pointer stk) {
  pointer lex,olex,denv;
  integer touched=0;	
#if (MMDBUG==High)
  alpsOuts(alpserr,"Enter markStack\n");
#endif
  for (lex.pt = stk.pt ; (lex.pt > base.pt); lex.pt-- )  {
    touched++;
    olex = *lex.pt;
    if (olex.wh == s_trailer) {
      denv = *--lex.pt;   /* denv is env          */
      olex = *--lex.pt;   /* olex is arg pointer  */
#if (MMDBUG==High)
      alpsOutsn(alpserr,5,"marking trailer: denv: ",itox(denv.wh),
		" argp: ",itox(olex.wh),"\n"); 
      checkDenv(denv);
#endif
      markEnv(denv); 
      olex = *olex.pt;
#if (MMDBUG==High)
      alpsOuts(alpserr,"arg: "); 
      dprtln(alpserr,olex,20);
      // showEnv(alpserr,denv);
#endif
      if (olex.wh) markacc(olex);  /* Mark the argument */
      continue;
    } else if (olex.wh > s_trailer) {
#if (MMDBUG==High)
      alpsOutc(alpserr,taschar(olex));
#endif
      markacc(olex);
      //      if (isEnv(olex)) showEnv(alpserr,olex);
    } else { /* olex.wh < s_trailer */
#if (MMDBUG==High)
    alpsOutsn(alpserr,3, "s_", evtab[olex.wh].lispName,"\n");
#endif
    }
    /* saved results are swept in parm args else must be on stack */
  } /* for */
#if (MMDBUG==High)
  alpsOutsn(alpserr,3,"...markStack exit:",itosn(touched,10)," entries mkd.\n");
#endif
}

inline static void markCnt(pointer lex) {
  pointer rex;
  ASSERT(isCnt(lex));
  if (isMarked(lex)) return; /* already seen */
  mark(lex);
  if (lex.ct->stksiz) { /* Do we in fact have a stack signor Gonzo ? */
    rex = lex.ct->stack;
    rex.wp--;
    ASSERT(isStk(rex));
    markStack(lex.ct->stack,lex.ct->ctlsp);
  }
  boogey_cnt++;
#if (MMDBUG==High)
  alpsOutsn(alpserr,3," Marked Cont @",itox(lex.wh),"\n");
#endif
}

static void markTcb(pointer lex) {
  pointer rex;
  ASSERT(isTcb(lex));
  if (isMarked(lex)) return; /* already seen */
#if (MMDBUG==High)
  alpsOutsn(alpserr,3," Mark tcb ",itos(lex.tb->tid),"...\n");
#endif
  mark(lex);
  markacc(lex.tb->iexp);
  markacc(lex.tb->parm);
  markEnv(lex.tb->dest);
  markEnv(lex.tb->defd);
  markCnt(lex.tb->cntptr);
  if (!isNil(lex.tb->envp))   markEnv(lex.tb->envp);
#if (MMDBUG==High)
  if (isEqq(lex,runtcb)) {
    alpsOuts(alpserr," parm: ");
    if (isEnv(parm))  showEnv(alpserr,parm);
    else              alpsOutsln(alpserr,(char *)itox(parm.wh));
    alpsOuts(alpserr," dest: "); showEnv(alpserr,dest);
    showCtl(alpserr,lex.tb->cntptr);
    showEnvStk(alpserr,lex.tb->envp);
  }
#endif
  markacc(lex.tb->iexpq); /* work to be done */
  markacc(lex.tb->tcbwq);
  markacc(lex.tb->filwq);
  markacc(lex.tb->fres);  /* if he is done need to preserve for waiters */
  boogey_tcb++;
  rex = getTcbRespt(lex);
  if (rex.wh) { /* is res in play */
    rex = *rex.pt;
    if (unMarked(rex)) {
      alpsOutsn(alpserr,5,"***",itos(lex.tb->tid),"res @",itox(rex.wh),
		"not marked***");
      showTcb(alpserr,lex);
      ASSERT(rex.wh == 0);
    }
#if (MMDBUG==High)
    else alpsOuts(alpserr,"res marked:");
    dprtln(alpserr,rex,20);
    alpsOutsn(alpserr,3,"... marked tcb ",itos(lex.tb->tid),"\n");
#endif
  }
}

inline static void markRef(pointer lex) {
  int i;
  mark(lex);
  /* mark contents of ref */
  for (i=0;i<nels(lex);i++) markacc(vadd(lex).pt[i]);
}

static bool markFS(pointer lex) { /* returns true if freshly marked */
  integer  index = (lex.wh - r.s.fspc.wh) / rg_len;
  if (index < 0 || index >= fs_size) {
    alpsOutsn(alpserr,5, "crazy FS @",itox(lex.wh),"=>",itox(*lex.wp),"\n");
    error(sys_cor,"markFS");
  }
  if (BMSETP(r.s.fs_bm.sp,index)) return false; /* has been marked before */
  BMSET(r.s.fs_bm.sp,index); /* mark fsentry*/
  boogey_lst++;
  return true;
}

static void markAtom(pointer lex) {
  ASSERT(isAtom(lex));
  if (isMarked(lex)) return;
  /* test in decreasing order of popularity if at all possible */
  if (isaNum(lex) || isChr(lex)) { mark(lex); return; } /*  opaque inside */
  if (isSym(lex)) { markSym(lex); return; }
  if (isEnv(lex)) { markEnv(lex); return; }
  if (isCym(lex)) { markSym(lex); return; }
  if (isRef(lex)) { markRef(lex); return; }
  if (isCnt(lex)) { markCnt(lex); return; }
  if (isTcb(lex)) { markTcb(lex); return; }
  if (isFob(lex)) { markFob(lex); return; }
  error(sys_cor,mkEmes(5,"crazy Atom @",itox(lex.wh),"=>",itox(*lex.wp),"\n"));
}

static int maentct  = 0;
static void markacc(pointer lex) { /* Mark object as accessible */ 

  /* TODO: do you want special treatment for nil ? Not really */
  if (isNil(lex)) return;
  if (isAtom(lex)) {markAtom(lex); return; }
  maentct++;
  if (boogey_mxrdp < maentct) boogey_mxrdp = maentct;
  while (isCons(lex)) {/* FS: scud along list to minimise recursion depth */
    if (!markFS(lex)) goto out;/* was already marked */
    markacc(lex.rg->ar);   /*mark the car*/
    lex = lex.rg->dr;      /*mark the cdr*/
  }
  markAtom(lex); /* mark the non cons cdr */
 out:
  maentct--;
}

/* the garbage collector (caveat emptor!) */
static void boogeyMan(const acctp acc) { 
  integer   i,j,k,fsf,gtcb,busyenvs,envsret,envskept,ret;
  pointer   lex, rex, tex, olex;
  lispTypes pctemp,stype;
  pointer   trash_list;
  if (!isNil(runtcb)) {
    tcbFlush(runtcb.tb);       /* actualise cntT field(s) in tcb         */
    pctemp = runtcb.tb->opcd;  /* save programme counter from corruption */
    if (pctemp != s_gc) mmbgcnt[acc]++; /* count who is calling          */
    runtcb.tb->gccnt++;
  } else pctemp = s_trailer;
#if (STATS)
  in_boogey=true;
  starr[s_gc]++;
#endif
#if (MMDBUG>=Low)
  alpsOutsn(alpserr,5,"Enter the Boogey man from ",
	    evtab[(integer)pctemp].lispName," type ",mmacctd[acc],"\n");
#endif
#if (DEBUG)
  //  alpsWhzat(alpserr);
#endif
  alpsFlush(alpsout);
  alpsFlush(alpserr);

  boogey_norefs = 0;      /* no of objects reffed  not found in workspace */
  boogey_ecnt   = 0;      /* number of reffed env pointers encountered    */
  boogey_cnt    = 0;      /* control stack entry count                    */
  boogey_tcb    = 0;      /* tcb marked  count                            */
  boogey_lst    = 0;      /* fs marked  count                             */
  boogey_sym    = 0;      /* symbol marked  count                         */
  boogey_fcnt   = 0;      /* foreign objects marked                       */
  boogey_vgn    = 0;      /* allocSym/v blocks marked as virgins          */
  boogey_vrec   = 0;      /* length of allocSym/a list                    */
  boogey_mxrdp  = 0;      /* max recursion depth for markacc              */
  busyenvs      = 0;      /* busy env blocks, not necessarily reffed      */
  BMZERO(r.s.fs_bm.sp,fs_size);   /*clear bit map                         */

#if (MMDBUG!=None)
    /* count stuff allocated by allocv and allocSymbol */
  for (i=0,lex = r.s.vreched; lex.wh; lex=lex.bk->bnext) {
    boogey_vrec++;
    rex.wh = lex.wh + alcbk_ohd;
    if (!isAtom(rex)) {
      alpsOutsn(alpserr,5,"Nasty chap on vreched @",itox(rex.wh),
		"=>",itos(rex.pt->it),"\n");
    }
    ASSERT(isAtom(rex));   /* Being a curious George */
  }
#endif
  /* Start marking */
  markSym(pnil);        /* markacc assumes he has been marked          */
#if (MMDBUG>=Low)
  alpsOuts(alpserr," mark tcbs\n ");
#endif
  if (!isNil(runtcb)) { /* current running tcb not on any disptab list */
#if (DEBUG)
    runtcb.tb->pcring[runtcb.tb->pcrind++] = s_gc; // slip the old boy in
    if (runtcb.tb->pcrind == BB_PCDEPTH) runtcb.tb->pcrind = 0;
#endif
    markTcb(runtcb); // mark running tcb
  }
  markTcb(iotcb);    // mark iotcb as he might be on done queue
  for (i=done+1;i<mxdsp;i++)  /* mark tcbs and their continuations */
    for (lex=disptab[i];!isNil(lex);lex=lex.tb->next) markTcb(lex);
  ASSERT(isMarked(mreptcb)); /* he is never done so must already be marked */

#if (MMDBUG>=Low)
  alpsOuts(alpserr," All tcbs marked\n ");
  alpsOutsn(alpserr, 2, itosn(boogey_vrec,10)," vreched vrecs \n");
  alpsOuts(alpserr," mark system atom stuff and non-nil entries in oblis\n");
#endif

  /* Protect error symbols */
  for (i=0;i<last_err;i++) markSym(errSym[i]);
  /* Mark single char constants and symbols   */
  for (i=0;i<256;i++) {
    markAtom(chartab[i]);
    markAtom(sstrtab[i]);
  }
  markAtom(sstrtab[256]); /* scratch entry */
  /* Protect exposed fill table entries */
  for (i=cpx;i<env;i++) markacc(filltab[i]);
  /* mark symbols in oblis that have a value */
  i = 0;
  for (lex = r.s.ospc;!isNil(lex); lex=lex.rg->dr) {
    for (olex = lex.rg->ar;!isNil(olex);olex = olex.rg->dr) {
      tex = olex.rg->ar;
      if (isSym(tex)) { /* if value or plist being used keep the bugger */
	if ((getVal(tex).wh || !isNil(tex.at->plist)) && !isMarked(tex)) markSym(tex);
	else {
/*	  alpsOuts(alpserr,"oblis nil dude ");
	  dprtln(alpserr,tex,16); */
	  i++;
	}
      } else if (isCym(tex)) markSym(tex);
      else {
	alpsOutsn(alpserr,5,
	  "non symbol in oblis @",itox(tex.wh),"=>",itox(*tex.ip),"\n");
      }
    }
  }
#if (MMDBUG>=Low)
  alpsOutsn(alpserr,2,itosn(i,10),
	    " nil oblis dudes found, mark prompts and null vectors \n");
#endif
  markacc(r.s.sysprmpt);
  markacc(r.s.brkprmpt);
  markacc(r.s.nonum);
  markacc(r.s.nostr);
  markacc(r.s.noref);
#if (MMDBUG>=Low)
  alpsOuts(alpserr, " mark system atoms and apl contants/variables\n");
#endif
  /* Mark system stuff */
  for (stype = s_nop; stype < s_con; stype++){ markacc(s[stype]); }
  /* Mark APL variables, includes filin,filerr and filout via #RF,#PF and #LF */
  for (i=0;i<apl_consts;i++) { markacc(a.h[i]);    }
  for (i=0;i<apl_consts;i++) { markacc(actv.h[i]); }
  /* Mark transient stuff on protection stack */
  for (j=0; j<protind;j++) markacc(protstk[j]); 
#if (STATS)
  for (stype = s_nop; stype < s_trailer; stype++) markacc(stent[stype]);
#endif
  /* End of Mark Phase for everything except the oblis fs entries */
#if (MMDBUG>=Low)
  alpsOuts(alpserr, " drop crap from oblis: ");
#endif
  /* scan oblis fs for unmarked cars unlinking them while marking the others */
  for (i=0,lex=r.s.ospc; !isNil(lex); lex = lex.rg->dr) {
    markFS(lex); // quick mark of each head
    for (rex = olex = lex.rg->ar;!isNil(rex);rex  = rex.rg->dr) {
      if (isMarked(rex.rg->ar)) {
	markFS(rex); // quick mark of fs with marked car
	olex = rex;
      }  else {	/* was not seen ergo:unlink, rex not marked */
	i++;
	if (isEqq(rex,olex)) olex = lex.rg->ar = rex.rg->dr;
        else olex.rg->dr = rex.rg->dr;
      }
    }
  }
#if (MMDBUG>=Low) 
  alpsOutsn(alpserr, 2,itosn(i,10),
	   " entries dropped\n Now mark oblis fs entries\n");
#endif
  if (boogey_norefs > 0) {
    alpsOutsn(alpserr, 2,itosn(boogey_norefs,10)," weirdos referenced\n");
  }

#if (MMDBUG!=None)
  lex = r.s.vreched;
  for (i=0; lex.pt != NULL; i++) {
    rex.wh = lex.wh + alcbk_ohd; /* pointer to vrec */
    ASSERT(isAtom(rex));
    if (!isMarked(rex)) { boogey_vgn++; } /* checking we hit all these dudes */
    lex = lex.bk->bnext;
  }
#endif


#if (MMDBUG>=Low)
  if (j)  
    alpsOutsn(alpserr,2,itosn(j,10)," transient protect objects marked \n");
  alpsOutsn(alpserr,4, itosn(i,10)," Vrec blocks of which ",
	    itos(boogey_vgn)," were virgins\n");
  alpsOutsn(alpserr,2,itosn(boogey_fcnt,10)," foreign objects marked \n");
  alpsOutsn(alpserr,2,itosn(boogey_cnt,10)," continuations marked \n");
  alpsOutsn(alpserr,2,itosn(boogey_tcb,10)," tcbs marked \n");
  alpsOutsn(alpserr,2,itosn(boogey_sym,10)," symbols marked \n");
  alpsOutsn(alpserr,2,itosn(boogey_lst,10)," cons cells marked \n");
  alpsOuts(alpserr, "end mark phase..trash unreffed FWS \n");
#endif

  /*now chase the vreched list and trash what is really trash*/

  trash_list.np = NULL;
  lex           = r.s.vreched;
  olex          = lex;
  j             = 0;
  while (lex.pt) { /* return what you have stolen from me...*/
    rex    = lex.bk->bnext;
    tex.wh = lex.wh + alcbk_ohd; /* tex pointing to vrec */
    if (!isMarked(tex)) {   /*not marked*/
      j++;
      if (lex.pt == r.s.vreched.pt) {
	r.s.vreched = rex;
	olex        = rex;
      } else	olex.bk->bnext = rex;
      lex.bk->bnext = trash_list;   /*link to list for later trashing */
      trash_list    = lex;
      //alpsOutsn(alpserr,2,itox(lex.it),"  = ");
      //alpsPrint(alpserr,tex,1,s_gc);
      //alpsOutln(alpserr);
    } else {
      unMark(tex);
      olex = lex;
    }
    lex = rex;
  }
#if (MMDBUG>=Low)
  alpsOutsn(alpserr,3, "about to trash ",itos(j)," FWS entries\n");
#endif
  lex = trash_list;
  while (lex.np != NULL) {
    olex = lex;
    lex  = lex.bk->bnext;
    ret  = rtn(olex,avrc);  /* away she goes*/
  }
#if (MMDBUG>=Low)
  alpsOutsn(alpserr, 3,"trashed ",itos(j)," FWS entries\n");
  alpsOuts(alpserr, "Reclaim FS\n");
#endif

  /* Rebuild the free fs space index */

  fsf = 0;
 /* reclaimed = old busy - new busy */
  j   = (fs_size - r.s.nfsfree) - boogey_lst;
  fsf = fs_size - boogey_lst;
  r.s.nfsfree = fsf;
  r.s.fffsi   = hash_size; /* yes, you will regret this */

#if (MMDBUG>=Low)
  alpsOutsn(alpserr,9, "FS: ", itos(j)," reclaimed, ", itos(fsf)," free, ",
	   itos(boogey_lst)," busy;  Total ", itos(fs_size),"\n");
  alpsOuts(alpserr, "Reclaiming env blocks: ");
#endif
  /* reclaim envs */
  envskept = envsret = 0;
  for (i=0;i<=num_env_bkts;i++) env_free[i] = pnil; /* hurl free lists */
  olex = lex = r.s.envbase;
  while (lex.wh < r.s.envlim.wh) {
    /* find maximal contiguous free blocks */
    while (!isMarked(lex) && lex.wh < r.s.envlim.wh) {
      if (nargs(lex) < 0) {
	envzomb--;
#if (DEBUG)
	alpsOutsn(alpserr,5,"envgc saw zombie @",itox(lex.wh)," size ",itos(nargs(lex)),"\n");
#endif
      }
      lex.wh += (nargs(lex) + env_rgohd) * rg_len;
      envsret++;
      r.s.env_blks--;
    }
    ASSERT(lex.wh < r.s.envlim.wh);
    if (isEqq(lex,olex)) { /* busy block */
      unMark(lex); /* unmark */
      lex.wh +=  (nargs(lex) + env_rgohd) * rg_len; /* skip the busy block */
      if (lex.pt == r.s.envlim.pt) break; /* hit the end */
      envskept++;
    } else { /* maximal free block found */
      j = (lex.wh - olex.wh) / rg_len - env_rgohd;
      if (j < 0) { // do we have a lone free zombie ?
	ASSERT(isEnv(olex));
#if (DEBUG)
	alpsOutsn(alpserr,7,"envgc keeping zombie(s) @",itox(olex.wh)," size ",itos(nargs(olex)),":", itos(j),"\n");
	if (nargs(olex) != j) {
	  alpsFlush(alpserr);
	  alpsPause();
	}
#else
	ASSERT(nargs(olex) == j);
#endif
	olex.ev->nargs = j;   // possibly fusing contiguous zombies
	envzomb++; // keeping one after all
      } else {
	olex.ev->nargs = j;
	//	olex.ev->idntt = env<<FBits;
	snenv(olex,env_free[i = ((j < num_env_bkts) ? j : num_env_bkts)]);
	env_free[i] = olex;
	r.s.env_blks++;
      }
    }
    olex = lex;
  }
  ASSERT(lex.pt == r.s.envlim.pt);

#if (MMDBUG>=Low)
  alpsOutsn(alpserr, 6,
	    itos(envsret)," returned + ",
	    itos(envskept)," kept = ",itos(envsret+envskept), "\n");
  alpsOuts(alpserr, "Reclaiming continuation blocks:\n");
#endif
  /* reclaiming continuations */
  i=j=k=0;
#if (MMDBUG!=None)
  for (lex = r.s.cnt_free; lex.pt; lex = lex.ct->next) {
    k++; /* count free list */
    if (!isCnt(lex)) alpsOutsn(alpserr,3,"Bad cntl blk @",itox(lex.wh),"\n");
  }
#endif
  lex = r.s.cnt_busy;
  r.s.cnt_busy.pt = NULL;
  while (lex.pt) {
    rex = lex.ct->next;
    if (isMarked(lex)) { /* still busy */
      unMark(lex); /* clear mark bit */
      ASSERT(isCnt(lex));
      lex.ct->next = r.s.cnt_busy;
      r.s.cnt_busy = lex;
      i++;
    } else { /* free */
      j++;
      retCnt(lex);
    }
    lex = rex;
  }
#if (MMDBUG>=Low)
  alpsOutsn(alpserr,10,itosn(k,5)," free ,",itos(j)," returned + ",itos(i),
	    " kept = total:",itos(k+j+i)," cnt_alloc ",itos(r.s.cnt_alloc),"\n");
  alpsOuts(alpserr,"Releasing unmarked task control blocks: ");
  alpsFlush(alpserr);
#endif
 /* reclaiming tcbs */
  k=j=gtcb=0;
  if (!isNil(lex=runtcb)) { /* First deal with running tcb not on any list */
    ASSERT(isMarked(lex));
    if (gtcb < lex.tb->tid) gtcb = lex.tb->tid; // get largest active tid
    k++; unMark(lex);
  }
  /* count up and unmark all tcbs on all except done list*/
  for (i=done+1;i<mxdsp;i++) { /* for all that are not done */
    for (lex=disptab[i];!isNil(lex);lex=lex.tb->next) {
      ASSERT(isMarked(lex));   /* they had better all be marked */
      if (gtcb < lex.tb->tid) gtcb = lex.tb->tid; // get largest active tid
      k++; unMark(lex);
    }
  }
  /* deal with done list separately where the unmarked dudes are to be found */
  lex = disptab[done];
  disptab[done] = pnil;
  while (!isNil(lex)) {
    if (isMarked(lex)) {
      if (gtcb < lex.tb->tid) gtcb = lex.tb->tid; // get largest active tid
      k++; unMark(lex); /* clear mark bit */
      /* preserve disptab list order minus the unmarked */
      if (isNil(disptab[done])) disptab[done] = lex; 
      else olex.tb->next = lex;
      olex          = lex;
      lex           = lex.tb->next;
      olex.tb->next = pnil;   /* keep list healthy */
    } else { /* unmarked ... fame is fleeting, oblivion lasts forever */
      j++;
      tex = lex;
      lex  = lex.tb->next; /* take link before returning the block! */
      retTcb(tex);
    }
  }
  nxttid = gtcb + 1;  // set new next tcb id

#if (MMDBUG>=Low) 
  /* TODO: Make this a generic consistency check routine */
  {integer flen,tcount,actv=0; /* linear scan through workspace */
    for (lex=r.s.bbp.bb->bbase;lex.wh<r.s.bbp.bb->broof.wh;lex.wh+=flen) {
      flen = labs(lex.bk->bsize);
      if (lex.bk->bsize < 0) { /* busy case */
	rex.wh = lex.wh + alcbk_ohd; /* rex points to typed object */
	if (isFS(rex) || isEqq(rex,r.s.fs_bm)) continue;
	if (isTcb(rex)) {
	  actv++;
	  if (isEqq(rex,runtcb)) continue;
	  tcount = 0; /* home many times does he appear in the disp lists */
	  for (i=done;i<mxdsp;i++) { /* for all on dispatcher lists */
	    for (tex=disptab[i];!isNil(tex);tex=tex.tb->next) {
	      if (tex.wh == rex.wh) tcount++;
	    }
	  }
	  if (tcount != 1) {
	    alpsOutsn(alpserr,5,"Tcb consistency failure:  ",itos(tcount),
		      " num active in linscan so far ",itos(actv),"\n");
	    // showTcb(alpserr,rex);
	    // alpsWhzat(alpserr);
	  }
	}
      }
    }
    alpsOutsn(alpserr,3,"Total active linear ",itos(actv),"\n");
  }
  
  alpsOutsn(alpserr,4,itos(j)," returned, ",itos(k)," kept\n");
  alpsOuts(alpserr,"Releasing unmarked foreign objects: ");
  alpsFlush(alpserr);
#endif 

  /* reclaiming fobs */

  i   = j = 0;
  lex = r.s.fob_busy;
  r.s.fob_busy.pt = NULL;
  while (lex.pt) {
    rex = lex.fb->next;
    if (isMarked(lex)) { /* still busy */
      unMark(lex); /* clear mark bit */
      ASSERT(isFob(lex));
      lex.fb->next = r.s.fob_busy;
      r.s.fob_busy    = lex;
      i++;
    } else { /* free */
      if (isFil(lex)) filSweeper(lex); /* call his sweeper */
#if (GRAF)
      else if (isWidget(lex)) widSweeper(lex);
#endif
#if (INSTCON)
      else if (isInst(lex)) instClean(lex);
#endif
      j++;
      retFob(lex);
    }
    lex = rex;
  }
#if (MMDBUG>=Low)
  alpsOutsn(alpserr,4,itosn(j,6)," returned, ",itosn(i,6)," kept\n");
#endif
  /* having tried to get some free space */
  if (!r.s.nfsfree)       error(no_room,"boogey fs");
  /* bump total gc count */
  *getVal(a.v.hgc).vr->varadd.np += 1.0; /* can do this since #GC is const */ 
#if  (STATS)
  in_boogey = false;
#endif
#if (MMDBUG>=Low)
  //  printFree(alpserr,1); 
  alpsOuts(alpserr,"exit the boogey man\n");
#endif
#if (MMDBUG==High)
  //  alpsWhzat(alpserr);
  alpsOutsn(alpserr,3," max markacc recursion depth ",itos(boogey_mxrdp),"\n");
#endif
  alpsFlush(alpserr);
  /*SETRUNLIGHT('l');*/
}

/* featurless block used for array index manipulation */
static void makeBlock(integer size, pointer *lex) {
  integer ret,rsiz = (size + 1) * indlen ;   /* required size + id tag */
  pointer rex,tex;

  if (lex->wh != 0) {
    /* adjust pointer to allocated block alcbk_ohd + indtt */
    rex.wh = lex->wh - (alcbk_ohd + indlen);  
    if (labs(rex.bk->bsize) >= (rsiz + alc_adjust)) return;
  /*
    Note: Must do manual rtn since these guys are never
     looked at or touched by the boogey man as they are  
     never linked on the vreched list 
  */
    ret = rtn(rex,anon);   /*blow away old */
    alpsOutsn(alpserr,3,"makeBLock: ",itos(size),"\n");
  }
  /* Allocate new or a bigger one */
  tex = alc(rsiz,anon);
  if (!tex.pt)  error(no_room,"makeBlock");
  *tex.wp++ = 0;    /* mark as unknown soldier (anon)     */
  *lex      = tex;  /* send back new improved block       */
}

/* List allocators */

pointer cons(pointer p1, pointer p2) { /* ASSERT all parms protected */
  pointer lex = getfs(1);
  lex.rg->ar  = p1;
  lex.rg->dr  = p2;
   return(lex);
}

static pointer acons(pointer p1) { 
  pointer lex       = getfs(2);
  lex.rg->ar        = lex.rg->dr;
  lex.rg->ar.rg->ar = arg1(p1); 
  lex.rg->ar.rg->dr = arg2(p1); 
  lex.rg->dr        = arg3(p1);
  return(lex);
}

static pointer list(pointer p1, pointer p2) { /* ASSERT all parms protected */
  pointer lex;
  lex               = getfs(2);
  lex.rg->ar        = p1;
  lex.rg->dr.rg->ar = p2;
  return(lex);
}

static pointer list3(pointer p1, pointer p2, pointer p3) {
  pointer lex;
  lex                      = getfs(3);
  lex.rg->ar               = p1;
  lex.rg->dr.rg->ar        = p2;
  lex.rg->dr.rg->dr.rg->ar = p3;
  return(lex);
}

static pointer copyls(pointer lex, pointer *y) {
  register pointer mex,rex,res;
  int i,n;
  if (isAtom(lex)) return(*y = pnil);  /*atomic first argument*/
  n = listLen(lex);
  protect(lex);
  res = getfs(n);                           /* alloc new list */
  unprotn(1);
  for (mex=res,i=0;i<n;i++) {
    rex        = mex; /* slip to last */
    mex.rg->ar = lex.rg->ar;
    lex        = lex.rg->dr;
    mex        = mex.rg->dr;
  }
  ASSERT(isNil(mex) && isNil(lex));
  *y = rex;   /*return address of last item*/
  return res;
}

static pointer mkleg(pointer p1, pointer p2, pointer p3) {
  pointer Result, lex;

  lex               = getfs(3);
  Result            = lex;
  lex.rg->ar        = lex.rg->dr;
  lex.rg->dr        = p3;
  lex               = lex.rg->ar;
  lex.rg->ar        = p1;
  lex.rg->dr.rg->ar = p2;
  return Result;
}

static pointer mkassoc(pointer sym, pointer val, pointer next) {
  pointer Result       = getfs(2);
  Result.rg->ar        = Result.rg->dr;
  Result.rg->dr        = next;
  Result.rg->ar.rg->ar = sym;
  Result.rg->ar.rg->dr = val;
  return Result;
}

/* *************** LISP PRIMITIVES *************** */

pointer nrevList(pointer lex) {
  pointer tex,rex = pnil;
  while (!isNil(lex)) { tex=lex.rg->dr; lex.rg->dr=rex; rex=lex; lex=tex; }
  return rex;
}

static pointer nassoc(pointer *lex, pointer ent, pointer als) { /* see remp */
  for (*lex=als;!isNil(als) && !isEqq(caar(als),ent); als=cdr(als)) *lex = als;
  return als;
}

/********************* List accessors ********************/

pointer mcar(pointer lex)  { return (isAtom(lex)) ? pnil : lex.rg->ar; }
pointer mcdr(pointer lex)  { return (isAtom(lex)) ? pnil : lex.rg->dr; }
pointer mcadr(pointer lex) { return (isAtom(lex)) ? pnil : mcar(cdr(lex)); }
pointer setCar(pointer x, pointer y) {x.rg->ar = y; return y;}
pointer setCdr(pointer x, pointer y) {x.rg->dr = y; return y;}

static pointer getNthCdr(pointer n, pointer l) {
  integer i, lim = getWhole(n);
  for (i = 0; i < lim && isCons(l); i++) l = l.rg->dr;
  return(l);
}

static pointer getNth(pointer n, pointer l) {
  pointer res =  getNthCdr(n, l);
  return (isNil(res)) ? pnil : car(res);
}

static pointer last( int nargs, pointer parm) {
  pointer x   = arg1(parm);
  integer n   = 1;
  integer len = 1;
  if (nargs == 2) n = getWhole(arg2(parm));
  if (isAtom(x) || n < 0) return(pnil);
  while (isCons(cdr(x))) { x = cdr(x); len++; }
  if (n == 1)   return x;
  if (n == 0)   return cdr(x);
  x = arg1(parm);
  len = len - n;
  for (n=0;n<len;n++) x=cdr(x);
  return(x);
}

static pointer revl(int n, pointer parm) {
  pointer last,lex,rex = arg1(parm);
  pointer revlist      = copyls(rex, &last);
  pointer prev         = pnil;
  while (!isNil(revlist)) {
    lex            = revlist.rg->dr;
    revlist.rg->dr = prev;
    prev           = revlist;
    revlist        = lex;
  }
  return(last);
}

static pointer rotl(int n, pointer x) {
  pointer olex,lex,rex,tex;
  integer len,amnt;
  int i;
  
  rex  = argn(n-1,x);   /* rotatee       */
  len  = listLen(rex);
  amnt = (n == 1) ? -1 : getInt(arg1(x)) % len;
  if (amnt < 0) amnt = len + amnt;
  if (amnt == 0) return(rex);
  olex = lex = rex;
  for (i=0;i<amnt;i++) lex = cdr(olex = lex);
  olex.rg->dr = pnil;
  tex = lex;
  while (!isNil(cdr(lex))) lex = cdr(lex);
  lex.rg->dr  = rex;
  return(tex);
}

/*************** numeric comparisons: watch out for the fuzz ***********/

static inline int ctcomp(number l, number r, number fuzz) {
  /* Returns -1 for l < r; 0 for l = r and 1 for l > r */
  if (l == r) return 0;
  if ((isnan(l)) && (isnan(r))) return 0;
  number a = fabs(l);
  number b = fabs(r);
  fuzz *= (a > b) ? a : b;
  if (isinf(fuzz)) fuzz = 0;
  if (l > r) return (r+fuzz >= l) ? 0 : 1;
  return (l+fuzz >= r) ? 0 : -1;
}

static inline bool numequal(number l, number r) {
  return (ctcomp(l,r, getNumVal(a.v.hct)) == 0);
}
  
static inline  bool cpequal(complex l, complex r, number fuzz) {
  /* For complex numbers: Returns 0 for not equal and 1 for equal */
  return (l == r) ?  1 :
    ((ctcomp(creal(l),creal(r),fuzz)==0) &&
     (ctcomp(cimag(l),cimag(r),fuzz)==0));
}

/********************** Conversion predicate assistants *************/

static pointer ltest(pointer p, number val) {
  if (nels(p) != 1) error(dom_range,"arg can have only one element");
  if (!isNum(p)) return pnil;
  return (ctcomp(*vadd(p).np,val, getNumVal(a.v.hct)) == 0) ? ptru : pnil;
}

static pointer ptest(pointer p, number val) {
  if (nels(p) != 1) error(dom_range,"argument must have only one element");
  if (!isNum(p)) return pnil;
  return  (fmod(*vadd(p).np,2) == val) ? ptru : pnil;
}

/********************** Lisp relationals ******************/

static bool isLt(pointer i, pointer j) {
  integer k, lim;
  pointer lex = vadd(i);
  pointer rex = vadd(j);
  number  fuzz;

  if (!typeComp(i,j)) error(inv_funarg,"(lt <NCS> <NCS>)");

  lim = ( nels(i) <= nels(j)) ? nels(i) : nels(j);

  switch (type(i)) {
    
  case num: /* compare left to right el by el until different */
    fuzz = getNumVal(a.v.hct);
    for (k=0; k < lim; k++) 
      if (ctcomp(lex.np[k],rex.np[k],fuzz) != 0) break; /* not equal */
    if (k == lim) return (nels(i) < nels(j));
    return (ctcomp(lex.np[k],rex.np[k],fuzz) < 0) ? true : false;

  case cpx: /* compare left to right el by el until different */
    fuzz = getNumVal(a.v.hct);
    for (k=0; k < lim; k++) 
      if (ctcomp(cabs(lex.cp[k]),cabs(rex.cp[k]),fuzz) != 0) break;
    if (k == lim) return (nels(i) < nels(j));
    return (ctcomp(cabs(lex.cp[k]),cabs(rex.cp[k]),fuzz) < 0) ? true : false;

  case chr: 
  case sym:
  case cym:
    for (k=0; k < lim; k++) if (lex.sp[k] != rex.sp[k]) break;
    if (k == lim) return (nels(i) < nels(j));
    else          return (lex.sp[k] > rex.sp[k]) ? false : true;

  default: error(not_imp,"Unsupported data type in isLt");
  }/*case*/
  error(pgmr_inc,"isLt not reached");
  return(false);
}

static bool isLe(pointer i, pointer j) {
  integer k, lim;
  pointer lex = vadd(i);
  pointer rex = vadd(j);
  number  fuzz;

  if (!typeComp(i,j)) error(inv_funarg,"(le <NCS> <NCS>)");

  lim = ( nels(i) <= nels(j)) ? nels(i) : nels(j);

  switch (type(i)) {
    
  case num: 
    fuzz = getNumVal(a.v.hct);
    for (k=0; k < lim; k++) 
      if (ctcomp(lex.np[k],rex.np[k],fuzz) != 0) break; /* not equal */
    if (k == lim) return (nels(i) <= nels(j));
    return (ctcomp(lex.np[k],rex.np[k],fuzz) <= 0) ? true : false;

  case cpx: /* compare left to right el by el until different */
    fuzz = getNumVal(a.v.hct);
    for (k=0; k < lim; k++) 
      if (ctcomp(cabs(lex.cp[k]),cabs(rex.cp[k]),fuzz) != 0) break;
    if (k == lim) return (nels(i) < nels(j));
    return (ctcomp(cabs(lex.cp[k]),cabs(rex.cp[k]),fuzz) <= 0) ? true : false;

  case chr: 
  case sym:
  case cym:
    lim = ( nels(i) <= nels(j)) ? nels(i) : nels(j);
    for (k=0; k < lim; k++) if (lex.sp[k] != rex.sp[k]) break;
    if (k == lim) return (nels(i) <= nels(j));
    else          return (lex.sp[k] <= rex.sp[k]) ? true : false;    

  default: error(not_imp,"Unsupported data type in le");
  }/*case*/
  error(pgmr_inc,"isLe not reached");
  return(false);
}

static bool mIsEq(pointer i, pointer j) {
  integer k, lim;
  pointer lex = vadd(i);
  pointer rex = vadd(j);
  number  fuzz;
  complex ctem;
  /* type checks have been done upstream except num vs cpx */
  if  (nels(i) != nels(j)) return false; // without much ado
  lim = nels(i);
  switch (type(i)) {
    
  case num: 
    fuzz = getNumVal(a.v.hct);
    if (isNum(j)) {
      for (k=0; k < lim; k++) 
	if (ctcomp(lex.np[k],rex.np[k],fuzz) != 0) return false;
    } else {
      for (k=0; k < lim; k++) {
	ctem =   lex.np[k];
	if (!cpequal(ctem,rex.cp[k],fuzz)) return false;
      }
    }
    return true;

  case cpx: 
    fuzz = getNumVal(a.v.hct);
    for (k=0; k < lim; k++) {
      ctem =  (isNum(j)) ? rex.np[k] : rex.cp[k];
      if (!cpequal(lex.cp[k],ctem,fuzz)) return false;
    }
    return true;

  case chr:
  case sym:
  case cym:
    for (k=0; k < lim; k++) if (lex.sp[k] != rex.sp[k]) return false;
    return true;

  case ref:
  for (k=0; k < lim; k++)
    if (!isEqq(lex.pt[k],rex.pt[k])) return false;
  return true;

  default: error(pgmr_inc,"mIsEq: not reached");
  }/*case*/
  return(false);
}

static inline bool isEq(pointer i, pointer j) {
  if (isEqq(i,j)) return true;
  if (!isNCRS(i) || !isNCRS(j) || !ctypeComp(i,j)) return false;
  if ((nels(i) == 0) && (nels(j) == 0)) return true; // null vectors are equal
  if (!isaNum(i) || (dims(i) != 0) || (dims(j) != 0)) return false;
  return (mIsEq(i,j));
}

static inline bool isEql(pointer i, pointer j) {
  if (isEqq(i,j)) return true;
  if (!isNCRS(i) || !isNCRS(j) || !ctypeComp(i,j)) return false;
  return (mIsEq(i, j));
}

// mutually recursive with mIsRefEqual()
FORWARD static bool isEqual(pointer i, pointer j);

static bool mIsRefEqual(pointer i, pointer j) {
  integer k, lim;
  pointer lex = vadd(i);
  pointer rex = vadd(j);
  /* type checks have been done upstream */
  if  (nels(i) != nels(j)) return false; // without much ado
  lim = nels(i);
  for (k=0; k < lim; k++)
    if (!isEqual(lex.pt[k],rex.pt[k])) return false;
  return true;
}

/* TODO: alleviate recursion depth */
static inline bool isEqual(pointer i, pointer j) {
  if (isEqq(i,j)) return true;
  if (isNCRS(i)) {
    if (isNCRS(j)) {
      if (!ctypeComp(i,j)) return false;
      if (isRef(i)) return(mIsRefEqual(i,j));
      return (mIsEq(i, j));
    } else return false; // i is NCRS but j is not
  } 
  if (!isCons(i) || !isCons(j)) return false;
  if (isEqual(i.rg->ar, j.rg->ar)) return isEqual(i.rg->dr, j.rg->dr);
  return false;
}

static bool isGe(pointer i, pointer j) {
  integer k, lim;
  pointer lex = vadd(i);
  pointer rex = vadd(j);
  number  fuzz;

  if (!typeComp(i,j)) error(inv_funarg,"(ge <NCS> <NCS>)");

  lim = ( nels(i) <= nels(j)) ? nels(i) : nels(j);
  
switch (type(i)) {
    
  case num: 
    fuzz = getNumVal(a.v.hct);
    for (k=0; k < lim; k++) 
      if (ctcomp(lex.np[k],rex.np[k],fuzz) != 0) break; /* not equal */
    if (k >= lim) return (nels(i) >= nels(j));
    return (ctcomp(lex.np[k],rex.np[k],fuzz) >= 0) ? true : false;

 case cpx: /* compare left to right el by el until different */
    fuzz = getNumVal(a.v.hct);
    for (k=0; k < lim; k++) 
      if (ctcomp(cabs(lex.cp[k]),cabs(rex.cp[k]),fuzz) != 0) break;
    if (k == lim) return (nels(i) < nels(j));
    return (ctcomp(cabs(lex.cp[k]),cabs(rex.cp[k]),fuzz) >= 0) ? true : false;

  case chr:
  case sym:
  case cym:
    for (k=0; k < lim; k++) if (lex.sp[k] != rex.sp[k]) break;
    if (k == lim) return (nels(i) >= nels(j));
    else          return (lex.sp[k] >= rex.sp[k]) ? true : false;  

  default: error(not_imp,"Unsupported data type in ge");
  }/*case*/
  error(pgmr_inc,"isGe not reached");
  return(false);
}

static bool isGt(pointer i, pointer j) {
  pointer lex = vadd(i);
  pointer rex = vadd(j);
  integer k, lim;
  number  fuzz;

  if (!typeComp(i,j)) error(inv_funarg,"(gt <NCS> <NCS>)");

  lim = ( nels(i) <= nels(j)) ? nels(i) : nels(j);

  switch (type(i)) {
    
  case num: 
    fuzz = getNumVal(a.v.hct);
    for (k=0; k < lim; k++)
      if (ctcomp(lex.np[k],rex.np[k],fuzz) != 0) break; /* not equal */ 
    if (k == lim) return (nels(i) > nels(j));
    return  (ctcomp(lex.np[k],rex.np[k],fuzz) > 0) ? true : false;

  case cpx: /* compare left to right el by el until different */
    fuzz = getNumVal(a.v.hct);
    for (k=0; k < lim; k++) 
      if (ctcomp(cabs(lex.cp[k]),cabs(rex.cp[k]),fuzz) != 0) break;
    if (k == lim) return (nels(i) < nels(j));
    return (ctcomp(cabs(lex.cp[k]),cabs(rex.cp[k]),fuzz) > 0) ? true : false;

  case chr:
  case sym:
  case cym:
    for (k=0; k < lim; k++) if (lex.sp[k] != rex.sp[k]) break;
    if (k == lim) return (nels(i) > nels(j));
    else          return (lex.sp[k] > rex.sp[k]) ? true : false;
    
  default: error(not_imp,"Unsupported data type in gt");
  }/*case*/
  error(pgmr_inc,"isGt not reached");
  return(false);
}

/****************** making alps numbers ***************************/

/* make an alps number from arg */
pointer mkNum(const number val) {
  pointer rex;
  if (val == 0) return(getVal(a.v.hai));
  if (val == 1) return(getVal(a.v.hmi));
  rex = allocv(num, 0, r.s.lds);
  *rex.vr->varadd.np = val;
  return(rex);
}

pointer mkCpx(const number rval, const number ival) {
  pointer rex;
  rex = allocv(cpx, 0, r.s.lds);
  *rex.vr->varadd.cp = rval + (ival*I);
  return(rex);
}

static pointer mkCpxFromNum(pointer lex) {
  pointer rex;
  int i;
  rex = allocv(cpx, dims(lex),pvard(lex));
  for (i=0;i<nels(lex);i++) rex.vr->varadd.cp[i] = lex.vr->varadd.np[i];
  return(rex);
}

/**************** Attaching lists to one another ******************/

static pointer nconc2(pointer i, pointer j) {
  pointer lex;
  if (isNil(i)) return(j);
  if (isNil(j)) return(i);
  if (isAtom(i)) error(inv_funarg,"nconc arg1 not list");
  for (lex = i; !isNil(cdr(lex)); lex = lex.rg->dr); /* zap to end of list */
  lex.rg->dr = j; /* plug in new rest */
  return(i);
}

static pointer nconc(pointer p) {
  pointer lex,olex=pnil,rex,tex;
  integer i,n;
  n = nargs(p);
  if (n == 0) return pnil;
  if (n == 1) return arg1(p);
  for (i=0;i<(n-1);i++) {
    if (!isList(argn(i,p))) error(inv_funarg,"nconc needs lists");
  }
  tex = pnil;             /* head of list */
  for (i=0;i<n;i++) {
    rex = argn(i,p);
    if (isNil(rex)) continue;
    if (isNil(tex)) {olex = tex = rex; continue; }
    for (lex = olex; !isNil(cdr(lex)); lex = lex.rg->dr); /*zap to end of list*/
   lex.rg->dr = olex = rex; /* plug in new rest */
  }
  return(tex);
}

static pointer append(integer nargs, pointer parms) {
  pointer lex, mex, rex, res;
  integer i;
  bool gotone;   /*we hates this ineff*/

  /*
    Always copies for singleton argument
    Always copies non-final arguments 
  */
  res  = pnil;
  gotone = false;
  for (i=0; i < nargs; i++) {
    lex = argn(i,parms);
    if (!isNil(lex)) {
      if (isAtom(lex)) error(inv_funarg,"append needs lists");
      if (gotone) {
	if (i < (nargs - 1)) { /* more to   come */
	  rex.rg->dr = copyls(lex, &mex);
	  rex        = mex;
	} else rex.rg->dr = lex;
      } else {
	res    = copyls(lex, &rex);
	gotone = true;
      }
    }
  }
  return(res);
}

static pointer member(pointer x, pointer y) {
  /*the slow but thourough version*/
  while (!isNil(y) && !isEqual(x, car(y))) y =  cdr(y);
  return(y);
}  /*member*/

inline static pointer memq(pointer x, pointer y) {
  /*a quicker but  dirtier member*/
  while (!isNil(y) && !isEq(x,car(y)))  y = y.rg->dr;
  return(y);
}

inline static pointer memql(pointer x, pointer y) {
  while (!isNil(y) && !isEql(x,car(y)))  y = y.rg->dr;
  return(y);
}

static pointer subset(pointer x, pointer y) { /* only user of memql */
  pointer cax,z;
  if (isNil(x) || (isEq(x,y)))              return(ptru);
  if (isNil(y) || !isCons(x) || !isCons(y)) return(pnil);
  if (!isCons(x))                           return(memq(x,y));  /* complain?*/
  for (;!isNil(x);x=cdr(x)) {
    cax = car(x);
    for (z=y; !isNil(z); z=cdr(z)) if (isEq(cax,car(z))) break; /* found */
    if (isNil(z)) return(pnil);
  }
  return(ptru);
}

integer listLen(pointer lex) { /* list length */
  integer i;
#if (RANGE)
  for (i = 0;!isAtom(lex); i++) 
    if (isCons(lex)) lex = lex.rg->dr;
    else error(inv_funarg,"len <list>");
  if (!isNil(lex)) error(inv_funarg,"Bad list terminator");
#else
  for (i = 0;!isNil(lex); i++) lex = lex.rg->dr;
#endif
  return i;
}

static pointer position(pointer x, pointer y) {
  int i=0;
  while (!isNil(y) && !isEqual(x, car(y))) {i++; y =  cdr(y);}
  return isNil(y) ? r.s.nonum : mkNum(i);
}  /*position*/

static bool chkList(pointer);
static pointer delete_from_list(pointer x, pointer y) {
  pointer lex, rex;

  lex = y;
  rex = lex;
  if (isAtom(y)) return(pnil);
  while (!isNil(y)) {
    if (!isCons(y)) error(inv_funarg,"Bad list arg2");
    ASSERT(isCons(rex) && isCons(lex));
    if (isEqq(x,y.rg->ar)) {
      if (isEqq(y,rex)) {
	rex = y.rg->dr;
	lex = rex;
      } else lex.rg->dr = y.rg->dr;
    } else lex = y;
    y = y.rg->dr;
  }
  return(rex);
}

/* you are my substitute */
static pointer subst(pointer x, pointer y, pointer z) {

  pointer subaux(pointer lex) {

    if (isAtom(lex)) return (isEqual(y,lex) ? x : lex);
    else if (isEqual(y,lex)) return x;
    else  return(cons(subaux(lex.rg->ar), subaux(lex.rg->dr)));
  }
  return(subaux(z));
}

static pointer uassoc(pointer ent, pointer als) {
  /* this version for users who need numeric and string comparisons  */
  /* with some vetting in case they supply a badly constructed a-list */
  if (!isList(als)) error(inv_funarg,"bad assoc list");
  while (!isNil(als) && (isCons(car(als))) && (!isEql(caar(als), ent))) {
    als = als.rg->dr;
    if (!isList(als)) error(inv_funarg,"bad assoc list");
  }
  if (isNil(als)) return(pnil);
  if (!isCons(car(als))) error(inv_funarg,"bad assoc list entry");
  return(car(als));
}

pointer assoc(pointer ent, pointer als) {
  /*this is obscure for the sake of speed*/
  while (!isNil(als) && !isEqq(als.rg->ar.rg->ar,ent)) als = als.rg->dr;
  return(!isNil(als) ? als.rg->ar : pnil);
}

static pointer pairlis(integer nargs, pointer parm) {
  pointer symbols, args, als, lex, res;

  if (nargs >= 2 && nargs <= 3) {
    symbols = arg1(parm);
    args    = arg2(parm);
    als     =  (nargs == 3) ? arg3(parm) : pnil;
  } else error(inv_numarg,"(pairlis <list> <list> [<a-list>])");
  if (isNil(symbols))  return(als);
  if (!isNil(args)) {
    lex = mkassoc(symbols.rg->ar, args.rg->ar, pnil);
    args = args.rg->dr;
    if (!isList(args)) error(inv_funarg,"Bad args list");
  } else    lex = mkassoc(symbols.rg->ar, pnil, pnil);
  protect(lex);
  symbols = symbols.rg->dr;
  if (!isList(symbols))  error(inv_funarg,"Bad symbols list");
  res = lex;
  while (!isNil(symbols)) {
    if (!isNil(args)) {
      lex.rg->dr = mkassoc(symbols.rg->ar, args.rg->ar, pnil);
      args       = args.rg->dr;
      if (!isList(args)) error(inv_funarg,"Bad args list");
    } else lex.rg->dr = mkassoc(symbols.rg->ar, pnil, pnil);
    lex     = lex.rg->dr;
    symbols = symbols.rg->dr;
    if (!isList(symbols))  error(inv_funarg,"Bad symbols list");
  }
  lex.rg->dr = als;
  unprotect();
  return(res);
}

static pointer putp(pointer atm, pointer prop, pointer valu) {
  pointer lex;
  lex = assoc(prop, atm.at->plist);
  if (isNil(lex)) atm.at->plist     = mkleg(prop, valu, atm.at->plist);
  else            lex.rg->dr.rg->ar = valu;
  return(valu);
}

static pointer getp(pointer atm, pointer prop) {
  pointer als = atm.at->plist;
  while (!isNil(als) && !isEqq(als.rg->ar.rg->ar,prop)) als = als.rg->dr;
  return(!isNil(als) ? als.rg->ar.rg->dr.rg->ar : pnil);
}

static pointer remp(pointer atm, pointer prop) {
  pointer lex, rex;
  rex = nassoc(&lex, prop, atm.at->plist);
  if (isNil(rex))  return(pnil);
  if (rex.pt == atm.at->plist.pt) atm.at->plist = rex.rg->dr;
  else                            lex.rg->dr = lex.rg->dr.rg->dr;
  return(ptru);
}

static pointer explode(integer nargs, pointer parm) {
  integer len,ndims,axis,lnd,d,i,j,k,size,inc,step,rs;
  pointer lex,rex,tex,res;
  if (nargs<1 || nargs>2) error(inv_numarg,"(explod [<axis>] <NCRS>)");
  lex = argn(nargs-1,parm);
  if (!isNCRS(lex)) error(inv_funarg,"explod");
  if (nels(lex)==0) return(pnil);
  ndims = dims(lex);
  axis = (nargs==1) ? ndims - 1 : checkAxis(arg1(parm),ndims);
  lnd  = 0;
  len  = (ndims==0) ? 1 : getDim(lex,axis); /* number of elements in list */
  size =  nels(lex)/len;
  for (i=0;i<ndims;i++)  if (i != axis) r.s.lds.ip[lnd++] = getDim(lex,i);
  inc = step = 1;
  for (i=axis+1;i<ndims;i++) inc *= getDim(lex,i);
  step = nels(lex) / (inc  * len);
  res = getfs(len);
  protect(res);
  tex = vadd(lex);
  if (isaChr(lex)) {
    for (i = 0; i < len; i++) {
      d  = i*inc;
      if (size==1) rex = (isChr(lex)) ? sstrtab[tex.sp[d]] : chartab[tex.sp[d]];
      else {
	rex = allocv(chr, lnd, r.s.lds);
	rs = 0;
	for (j=0;j<step;j++) {
	  for (k=0;k<inc;k++) rex.vr->varadd.sp[rs++] = tex.sp[d+k];
	  d += inc*len;
	}
      }
      res.rg->ar = rex;
      res = res.rg->dr;
    }
    return(unprotect());
  }
  if (isaNum(lex)) {
    for (i = 0; i < len; i++) {
      rex = allocv(type(lex), lnd, r.s.lds);
      rs = 0;
      d  = i*inc;
      for (j=0;j<step;j++) {
	if (isNum(lex)) 
	  for (k=0;k<inc;k++) rex.vr->varadd.np[rs++] = tex.np[d+k];
	else
	  for (k=0;k<inc;k++) rex.vr->varadd.cp[rs++] = tex.cp[d+k];
	d += inc*len;
      }
      res.rg->ar = rex;
      res = res.rg->dr;
    }
    return(unprotect());
  }
  if (isRef(lex)) {
    for (i = 0; i < len; i++) {
      d  = i*inc;
      if (size > 1) {
	rex = allocv(ref, lnd, r.s.lds);
	rs = 0;
	for (j=0;j<step;j++) {
	  for (k=0;k<inc;k++) rex.vr->varadd.pt[rs++] = tex.pt[d+k];
	  d += inc*len;
	} 
      }
      else rex = tex.pt[d];
      res.rg->ar = rex;
      res = res.rg->dr;
    }
    return(unprotect());
  }
  error(sys_cor,"bad case");
  return(pnil); /* NOT REACHED */
}

/* make vector of type thisType from list. If heterogenous type is ref */
/* if a list of non-conformant elements of the same type, either num or chr,
      is imploded we just stitch their ravels together */
static pointer implode(pointer lex, pointer mex, vtype thisType, pointer norav)  {
  integer i,j,llen,lnd,ndim,noels,axis,axels,step,jumps,size,ncomp,s,t,d;
  pointer qex, rex, sex, tex, shapely;
  number dax;
  bool lam=false,conform = true,sawcpx=false;
  if (!isList(lex) || (!isNum(mex) && !isNil(mex)) || !isCym(norav))
    error(inv_funarg,"(implod [axis:ns [norav:llv]] arg:lst)");
  noels   = 0;
  shapely = getVal(a.v.hai); /* additive identity is a scalar */
  ndim    = lnd = dims(shapely);
  for (rex=lex; !isNil(rex); rex = rex.rg->dr) {
    if (!isList(rex))
      error(inv_funarg,"Expected a list for lst argument");
    tex = rex.rg->ar;
    if  (isNil(norav)) {
      if ((thisType == ref) && !isRef(tex)) noels++;
      else if (!vtypeComp(vrtype(tex), thisType)) {
	  error(inv_funarg,"incompatible types");
      } else {
	if (isCpx(tex)) sawcpx = true;
	noels += nels(tex);
	if (dims(tex) > ndim) {
	  ndim    = dims(tex); /* find the shapliest one */
	  shapely = tex;
	}
      }
    } else { /* norav case */
      noels++;
    }
  }
  if (!isNil(norav)) {
    if (!isNil(mex)) error(inv_funarg,"axis not allowed with norav"); // ?
    rex = allocv(ref, 1, (pointer) &noels);
    sex = vadd(rex);
    for (d = 0;!isNil(lex);lex=cdr(lex),d++)  sex.pt[d] = car(lex);
    return rex;
  } /* not norav */
  for (i=0;i<ndim;i++) r.s.lds.ip[i] = getDim(shapely,i);
  if (ndim == 0) { /* ajdust */
    ndim        = 1;
    r.s.lds.ip[0] = 1;
  }
  if (isNil(mex)) axis = ndim ? ndim-1 : 0; /* default */
  else {
    if (!isNum(mex)) error(inv_funarg,"implod axis must be numeric");
    dax  = getNum(mex); /* user gives axis with origin 1 */
    if (dax < 0) dax += ndim + 1; 
    if (dax != trunc(dax)) {lam = true;  axis = trunc(dax);}
    else                   {lam = false; axis = trunc(dax -1);}
    if (axis < 0 || axis >= (lam + ndim)) error(dom_range,"axis");
  }
  ncomp = axels = 0; /* check for conformance and count axis elements */
  for (rex=lex; (!isNil(rex) && conform); rex = rex.rg->dr) {
    tex = rex.rg->ar;
    if ((thisType == ref) && !isRef(tex)) { /* don't ravel non refs */
      axels += 1;
      conform &= (ndim == 1);
      continue;
    }
    if (isNCRS(tex)) {
      if (nels(tex) == 0) continue; /* [] conforms with anything */
      ncomp++;                      /* only relevant for lam     */
      if (dims(tex) == 0) {
	axels += 1;
	conform &= (ndim == 1);
      } else {
	if (conform &= (dims(tex) == ndim)) {
	  for (j=0;j<ndim;j++) {
	    if (!lam && (j == axis)) axels += getDim(tex,axis);
	    else conform &= (getDim(tex,j) == r.s.lds.ip[j]);
	  }
	}
      }
    } else {
      ASSERT(false);
    }
  }
  step = 1;
  if (!conform) {
    if (!isNil(mex)) {
      error(dom_range,"implod with axis on non conforming elements");
    } else { /* just flatten out the whole lot */
      lnd           = 1;
      r.s.lds.ip[0] = noels;
    }
  } else { /*  elements are conforment so keep the shape */
    if (lam) {
      lnd  = ndim + 1;
      for (i=ndim;i>axis;i--) step *= (r.s.lds.ip[i] = r.s.lds.ip[i-1]);
      r.s.lds.ip[axis] = ncomp;
      step *= ncomp;
    } else {
      lnd              = ndim;
      r.s.lds.ip[axis] = axels;
      for (i=(ndim-1);i>=axis;i--) step *= r.s.lds.ip[i];
    }
  }
  if (thisType == num && sawcpx) thisType = cpx;
  rex = allocv(thisType, lnd, r.s.lds);
  sex = vadd(rex);
  for (d = 0;!isNil(lex);lex=cdr(lex),d += size) {
    tex = lex.rg->ar;
    if ((thisType==ref) && !isRef(tex)) {
      size = 1;
      sex.pt[d] = tex;
      continue;
    }
    if (nels(tex)==0) {
      size = 0;
      continue;
    }
    ASSERT(isNCRS(tex));
    qex  = vadd(tex);
    size = 1;
    if (conform) 
      if (lam) size = step/ncomp;
      else for (i=(dims(tex)-1);i>=axis;i--) size *= getDim(tex,i);
    else size = nels(tex);
    jumps = nels(tex) / size ;
    s = 0;
    t = d;
    switch (thisType) {
    case chr:
      for (j = 0; j < jumps; j++,s += size,t += step) 
	for (i = 0; i < size; i++)  sex.sp[t+i] = qex.sp[s+i];
      break;
    case num:
      for (j = 0; j < jumps; j++,s += size,t += step) 
	for (i = 0; i < size; i++)  sex.np[t+i] = qex.np[s+i];
      break;
    case cpx:
      for (j = 0; j < jumps; j++,s += size,t += step) 
	for (i = 0; i < size; i++) 
	  sex.cp[t+i] = (isNum(tex)) ? qex.np[s+i] : qex.cp[s+i];
	break;
    case ref:
      for (j = 0; j < jumps; j++,s += size,t += step) 
	for (i = 0; i < size; i++)  sex.pt[t+i] = qex.pt[s+i];
      break;
    default:;
    }
  }
  return(rex); // protected by sendres
}

static pointer lnum(pointer s) {
  pointer rex;
  integer i, lim;

  rex = allocv(num, dims(s), pvard(s));
  lim = nels(s);
  for (i = 0; i < lim; i++)  rex.vr->varadd.np[i] = vadd(s).sp[i];
  return(rex);  // protected by sendres
}

static pointer lchr(pointer n) {
  pointer rex;
  integer i, lim;

  lim = nels(n);
  if (lim == 1) rex = sstrtab[(((integer)vadd(n).np[0]) & 255)];
  else {
    rex = allocv(chr, dims(n), pvard(n));
    for (i = 0; i < lim; i++)
      rex.vr->varadd.sp[i] = (char)(((integer)vadd(n).np[i]) & 255);
  }
  return(rex);  // protected by sendres
}

static pointer gensym(void) {
  pointer res, rex, rep;
  integer i,noels;
  uchar   tch,charb[2],tbuf[max_line_len];
  bool    carry;

  rex   = lookup(a.v.hgs);
  if (!isChr(rex)) error(inv_funarg,"#GS must be a char vector");
  noels = nels(rex); 
  if (noels >= max_line_len) error(inv_funarg,"#GS too long!");
  rep = vadd(rex); 
  carry = true;
  for (i = noels-1; i > 0; i--) {
    tch = rep.sp[i];
    if ((i && (tch < '0' || tch > '9'))) goto invchr;
    carry = carry & (tch < '9');
    tbuf[i] = tch;
  }
  tch = tbuf[0] = rep.sp[0];
  if (!isAlpha(tch)) goto invchr;
  res = makeAtom(rep.sp,noels, s_var);
  protect(res); // the tokoloshe could take him away as he is valueless
  carry = true;
  for (i = noels-1; i > 0; i--) {
    tch = tbuf[i];
    if (carry) {
      if (tch == '9') tbuf[i] = zero;
      else {
	tbuf[i]++;
	carry = false;
      }
    }
  }
  if (carry) {
     tch = tbuf[0];
     if (tch == 'z') error(inv_funarg,"#GS would wrap");
     tch++;
     while (!isAlpha(tch)) tch++;
     tbuf[0] = tch;
  } 
  rex = allocCV(noels);
  memcpy(vadd(rex).sp,tbuf,noels);
  setSym(a.v.hgs,rex);
  unprotect(); 
  return res;  // sendres protecting him now
 invchr:
  charb[0] = tch;
  charb[1] = 0;
  error(inv_funarg,mkEmes(3,"Invalid character in #GS: <",charb,">"));
  return pnil; /* NOT REACHED */
}

static pointer do_clear(pointer name) {
  pointer lex, rex, tex, res;
  integer i;
  
  res = ptru;
  if (isNil(name)) {   /*clear all values in vars found in oblis*/
    lex = r.s.ospc;
    while (!isNil(lex)) {
      rex = car(lex);
      while (!isNil(rex)) {
	tex = car(rex);
	if (isSym(tex)) { // gotta be symbols in oblis
	  tex.at->value.it = 0;
	  tex.at->plist = pnil;
	  tex.at->sysp  = crashme;
	}
	rex = cdr(rex);
      }
      lex = cdr(lex);
    }
    for (i=0;i<apl_consts;i++) setVal(a.h[i],actv.h[i]); // reset APL consts
  } else {
    lex = name;
    while (!isNil(lex)) {
      name = car(lex);
      if (!isSym(name))  res = pnil;
      else {
	name.at->value.it = 0;
	name.at->plist = pnil;
      // sysp and spl untouched as we could have primitives here
      // currently user has no way to touch them since compiler is gone
	for (i=0;i<apl_consts;i++) {
	  if (a.h[i].wh == name.wh) {
	    setVal(a.h[i],actv.h[i]);
	    alpsOutsn(alpserr,3,"APL const ", itos(i), " reset \n");
	    break;
	  }
	}
      }
      lex = cdr(lex);
    }
  }
  return(res);
}

/* set the interrupt (ctl) flag in the target tcb */
/* argument is either a tid or a tcb */
static pointer doKill(integer nargs, pointer parm) {
  pointer lex;
  integer tid;
  int i;
  pointer tcb = pnil;
  lex = arg1(parm);
  if ((nargs != 1) || (!isNum(lex) && !isTcb(lex)))
    error(inv_funarg,"(kill tcb | tcbid)");
  if (isNum(lex)) {
    tid = getWhole(lex);
    lex = runtcb; // check the guy that is running
    if (!isNil(lex) && (lex.tb->tid == tid)) goto killout;
  }  else {
    tcb = lex;
    if (!isNil(lex) && isEqq(lex,runtcb)) goto killout;
  }
  for (i=done+1;i<mxdsp;i++) { /* check all but done  */
    for (lex=disptab[i];!isNil(lex);lex=lex.tb->next) {
      if (isNil(tcb)) {
	if (lex.tb->tid == tid) goto killout;
      } else {
	if (isEqq(lex,tcb)) goto killout;
      }
    }
  }
  return pnil;
 killout:
  lex.tb->tflags |= ctlFlag;
  return(ptru);
}

static pointer doPrintFree(fibrec *out,pointer parm) {
  integer n;
  n = (nargs(parm) == 0) ? 0 : getWhole(arg1(parm));
  return (printFree(out,n)) ? ptru : pnil;
  /* nil => something is rotten in the state of  Denmark */
}

static pointer pushit(pointer value, pointer name) {
  pointer lex = slookup(name);  /* get slot */
  if (!*lex.wp) *lex.pt = pnil;
  *lex.pt = cons(value,*lex.pt);
#if (BINDING==DEEPC)
  cacheSymVal(name,*lex.pt);
#endif
  return(value);
}

static pointer popit(pointer name) {
  pointer lex,rex;
  lex = slookup(name);
  rex = *lex.pt;
  if (!rex.wh) return(pnil);
  if (isAtom(rex)) return(rex);
  else {
    *lex.pt = rex.rg->dr;
#if (BINDING==DEEPC)
    cacheSymVal(name,*lex.pt);
#endif
    return(rex.rg->ar);
  }
}

static pointer do_tab(pointer parm) {
  pointer lex;
  integer npos;
  fibrec *out;

  if (nargs(parm) != 1) error(inv_funarg,"(tab <num> [<file object>])");
  lex = arg1(parm);
  out = cout();
  if (!isNum(lex)) error(inv_funarg,"tab pos must be numeric");
  if (nargs(parm) == 2) {
      if (isaFil(arg2(parm))) out = getFil(arg2(parm));
      else error(inv_funarg,"(tab <num> <file object>)");
  }
  npos = getWhole(lex);
  alpsTab(out,npos);
  return(lex);
}

static number mkrtime() {
  struct timeval   wtime;
  gettimeofday(&wtime,0);
  return (number)wtime.tv_sec +(number)wtime.tv_usec / 1000000.0;
}

/**************** compiler specials aka peek 'n poke **************/

  /* FIXME
PROCEDURE do_call(fun,args : pointer);
VAR i    : INTEGER;
    proc : proc_type;
BEGIN
i := listLen(args);
proc.int[0] := fun.at^.sysp.it;
proc.int[1] := 0; must be level 1
IF  i = 1 THEN CALL(proc.p1,args.rg^.ar)
ELSE
  IF  i = 2 THEN CALL(proc.p2,args.rg^.ar,args.rg^.dr.rg^.ar)
  ELSE
    IF  i = 3 THEN CALL(proc.p3,args.rg^.ar,args.rg^.dr.rg^.ar,
                                            args.rg^.dr.rg^.dr.rg^.ar)
    ELSE
      IF i = 0 THEN CALL(proc.p0)
      ELSE error(inv_funarg);
END;
*/

static pointer mint(pointer val, pointer len) { /* compiler blob maker */
  pointer rex;                 /* encodes val in len octets */
  integer ilen, ival;

  ival = getWhole(val);
  ilen = getWhole(len);
  if (ilen != 1 && ilen != 2 && ilen != 4) error(inv_funarg,"mint bad len");
  if (ilen == 1)  return sstrtab[0xff & labs(ival)];
  rex = allocCV(ilen);
  if (ilen == 2) {
    rex.vr->varadd.sp[0] = (char)(labs(ival) / 256);
    rex.vr->varadd.sp[1] = (char)(labs(ival) & 255);
  } else  *rex.vr->varadd.ip = ival;
  return(rex);  // protected by sendres
}

static pointer mkprc(pointer parm) {
  pointer fun, code;
  fun  = arg1(parm);
  code = arg2(parm);
  fun.at->spl  = s_con; /* was s_call */
  fun.at->sysp = vadd(code); /* FIXME */
  return(getVal(fun));
}

static pointer dopeep(pointer parm) {
  pointer mex, rex,addr;
  integer i,ilen;
  addr        = arg1(parm);
  ilen        = (nargs(parm)==1) ? 1 : getWhole(arg2(parm));
  mex.wh      = getWhole(addr);
  *r.s.lds.ip = ilen;
  rex         = allocv(num, 1, r.s.lds);
  for (i = 0; i < ilen; i++)  rex.vr->varadd.np[i] = mex.wp[i];
  return(rex); // protected by sendres
}

static pointer mget(pointer parm) {
  pointer addr, mex;
  integer ilen;
  addr   = arg1(parm);
  ilen   = (nargs(parm)==1) ? 1 : getWhole(arg2(parm));
  mex.wh = getWhole(addr);
  return(mkChr(mex.sp,ilen)); // protected by sendres
}

static pointer mput(pointer parm) { /* compiler poke routine */
  pointer addr, lex, mex, tex;
  integer i, len;
  
  addr   = arg1(parm);
  lex    = arg2(parm);
  mex.wh = getWhole(addr);
  len    = nels(lex) * lentab[vrtype(lex)];
  tex    = vadd(lex);
  for (i = 0; i < len; i++)  mex.sp[i] = tex.sp[i]; /* poke */
  return(lex);
}

/* *************** SYSTEM INITIALISATION SECTION *************** */

static void initCharSets() {
/* make character sets (the old Pascal compiler would do this for us) */
  mkCharSet(specChars,specCS);
  mkCharSet(whiteChars,whiteCS);
  mkCharSet(digitChars,digitCS);
  mkCharSet(alphaChars,alphaCS);
  mkCharSet(alphaChars,symStCS); /* Symbol Start chars    */
  addCharSet(wordChars,symStCS); /* neg handled in rataux */
  addCharSet(digitChars,symStCS); 
  mkCharSet(wordChars,wordCS);   /* other symbol chars    */
  addCharSet(digitChars,wordCS);
  addCharSet(alphaChars,wordCS);
}

static void initWorkspace(void) {
  integer i, j, k, l, m, lnd, noels;
  pointer lex, mex, rex, fadd;
  lispTypes  stype;
  vtype      vtyp;
  integer    size;
  char      *mess,*name;
  uchar      charb[8];

  /* initialise variable storage space and initial oblist */
  r.s.bbp = r.s.wspc;
  size    = (wsp_size-4) * (whole) indlen - pcbsize;
  fadd.wh = r.s.wspc.wh + pcbsize;
  init_partn(r.s.bbp, fadd, size);
  /*initialize system variables*/
  r.s.wspc_end    = r.s.bbp.bb->broof;
  r.s.valign      = ALIGN;   /* due to bug in hp 3000 MOD operator    */
  r.s.vreched.wp  = NULL;    /* trash list must always be a good list */
  /* allocate FS (cons) space */
  r.s.fspc        = alc(fs_size*rg_len,alst); /* fs accounting */
  r.s.ospc        = r.s.fspc;
  r.s.fspc_end.wh = r.s.fspc.wh + fs_size*rg_len;
  /* allocate FS bitmap */
  r.s.fs_bm       = alc((fs_size + 7) / 8 + 1,alst);
  /*allocate binding control enviroment*/
  r.s.envbase     = alc(envsize*indlen,aenv); /* env accounting */
  r.s.envlim.wp   = r.s.envbase.wp + envsize;
  /* allocate apl index blocks */
  r.s.lds.wp      = NULL;
  r.s.ldx.wp      = NULL;
  r.s.indad.wp    = NULL;
  makeBlock(init_bsize,&r.s.lds); /*anticipate 6 dims but can grow */
  makeBlock(init_bsize,&r.s.ldx); 
  makeBlock(init_bsize,&r.s.indad);
  /* allocate APL null vectors */
  r.s.nonum      = mkNullV(num);
  r.s.nostr      = mkNullV(chr);
  r.s.noref      = mkNullV(ref);
  /*first a handmade atom NIL for something to start with*/
  pnil           = allocSymbol("nil",3,s_con);
  pnil.at->idntt = cym<<FBits; /* make constant symbol */
  r.s.snil = pnil;
  /* initialise fs space and allocate oblis hash fs entries */
  BMZERO(r.s.fs_bm.sp,fs_size);      /* mark all FS as free     */
  for (i = 0; i < hash_size; i++) {
    BMSET(r.s.fs_bm.sp,i);                       /* mark oblis hash fs busy */
    r.s.ospc.rg[i].ar    = pnil;                 /* oblis hash car          */
    r.s.ospc.rg[i].dr.pt = &r.s.ospc.rg[i+1].ar; /* oblis hash fs link      */
  }
  r.s.fffsi   = hash_size;                       /* set first free fs index */
  r.s.nfsfree = fs_size - hash_size;  /* calc no of remaining free effesses */
  r.s.ospc.rg[hash_size-1].dr = pnil; /* terminate oblis hash fs chain      */
  /*finish our handmade atoms: nil and t*/
  pnil.at->plist = pnil.at->value = pnil;
  lex            = getfs(1);   /* put nil into oblist */
  lex.rg->ar     = pnil;
  lex.rg->dr     = pnil;
  r.s.ospc.rg[hash(vadd(r.s.snil).sp, 3)].ar = lex;
  /* now its true */
  ptru           = allocSymbol("t",1,s_con);
  ptru.at->idntt = cym<<FBits; /* make constant symbol */
  r.s.strue      = ptru;
  ptru.at->plist = pnil;
  ptru.at->value = ptru;
  lex            = getfs(1);
  lex.rg->ar     = ptru;
  lex.rg->dr     = pnil;
  r.s.ospc.rg[hash(vadd(r.s.strue).sp, 1)].ar = lex;

    /*the initial set of primitives*/
  for (stype = s_nop;  stype < s_trailer; stype++) {

    if (stype != evtab[stype].stype) {
      mess = mkEmes(7,"initWorkspace: Programmer incompetence detected:\n "
		    "    lisp type index for ",evtab[stype].lispName," is ",
		    itos(stype)," expected ",itos(evtab[stype].stype),"\n");
      alpsAbend(mess);
    }
    ASSERT(isCym(pnil));
    name = evtab[stype].lispName;
    if (stype <= s_var ) {
      s[stype] = makeAtom((uchar *)name, strlen(name), stype);
      ASSERT(isCym(pnil));
    } else if (stype < s_con) { /* constant symbols */
      lex = makeAtom((uchar *)name, strlen(name), s_con);
      lex.at->idntt = cym<<FBits; /* make constant symbol */
      s[stype]      = lex;
      ASSERT(isCym(pnil));
    }
#if (STATS)
    starr[stype] =  sttms[stype] = 0;
    stent[stype] = (stype < s_con) ? s[stype] : 
      makeAtom((uchar *)name, strlen(name), s_var);
#endif
  }
  /* build character atom table s*/
  for (i=0; i< 256; i++) {
    charb[0]   = (uchar ) i;
    lex        = allocCV(1);
    lex.vr->varadd.sp[0] = i;
    sstrtab[i] = lex;
    if ((i > space) && (i < 127)) {
      stype      = (isSpec(i)) ? s_con : s_var;
      lex        = makeAtom(charb,1,stype);
      if (lex.at->spl==s_con) lex.at->idntt = cym<<FBits; /* constant symbol */
    }
    chartab[i] = lex;
  }
  sstrtab[256] = allocCV(1); /* scratch chr singleton */
  /*apl constants constitute the rest*/
  k = 0;   /* numeric constants value index    */
  l = 0;   /* character constants value index  */
  m = 0;   /* word constants value index       */
  for (i = 0; i < apl_consts; i++) {
    a.h[i] = makeAtom((uchar *)h_consts[i].name,3,h_consts[i].ltp); 
    lex    =  a.h[i];   /*utility pointer to constant name atom*/
    vtyp   = h_consts[i].vtp;
    if (vtyp == ref) {
      actv.h[i] = r.a[h_wval[m++]]; /* initializing from rootable */
      ASSERT(isNil(actv.h[i]));
    } else if (vtyp == num) {
      lnd         = h_consts[i].numdim;
      *r.s.lds.ip = h_consts[i].numels;
      rex = allocv(vtyp, lnd, r.s.lds);   /*create variable storage*/
      actv.h[i] = rex;
      mex   = vadd(rex);
      noels = nels(rex);
      for (j = 0; j < noels; j++) mex.np[j] = h_nval[k++];
    } else if (vtyp == chr) {
      rex = mkChr((uchar *)h_cval[l],strlen(h_cval[l]));
      actv.h[i] = rex;;
      l++;
    } else error(pgmr_inc,"apl constants");
    setVal(lex,actv.h[i]);
  }
  /* Build error symbols */
  for (i = 0; i < last_err; i++){
    if (i != errTab[i].errind) {
      mess = mkEmes(7,"initWorkspace: Programmer incompetence detected:\n "
	     "    error index for ",errTab[i].errname," is ",
		    itos(i)," expected ",itos(errTab[i].errind),"\n"); 
      alpsAbend(mess);
    }
    errSym[i] = makeAtom((uchar *)errTab[i].errname,strlen(errTab[i].errname),s_var);
  }
  /*set executable and workspace names */
  setVal(a.v.hnm,mkChr((uchar *)myname,strlen(myname)));
  setVal(a.v.hws,mkChr((uchar *)save_file,strlen(save_file)));
  ASSERT(isSym(a.v.hcp));
}  /*initializing workspace structures*/

static void initVars( int argc, char **argv ) {
  int i,len;
  pointer lex,ridptt[5];
  uidle = ubusy  = 0.0;
  boo            = mkrtime();
  end_of_program = false;
  shutting_down  = false;
  savewspc       = false;
  ridptt[0]      = getVal(a.v.hai);
  ridptt[1]      = getVal(a.v.hmi);
  ridptt[2]      = getVal(a.v.hmn);
  ridptt[3]      = getVal(a.v.hmx);
  ridptt[4]      = getVal(a.v.hnn);
  for (i=0;i<prim_dyadics;i++) {
    idelem[i] = ridptt[idindx[i]];
  }
  if ((s_dist + prim_dyadics) != s_roll) {
    error(pgmr_inc,"prim_dyadics");
    alpsExit(1);
  }
  /* make fill table for kompress */
  r.s.lds.ip[0] = 1;
  filltab[0] = pnil;                  /* marker (unk) */
  filltab[1] = getVal(a.v.hai);       /* num */
  filltab[2] = allocv(cpx,1,r.s.lds); /* cpx */
  filltab[2].vr->varadd.cp[0] = 0;
  filltab[3] = allocv(chr,1,r.s.lds); /* chr */
  filltab[3].vr->varadd.sp[0] = ' ';
  filltab[4] = allocv(ref,1,r.s.lds); /* ref */
  filltab[4].vr->varadd.pt[0] = pnil;
  filltab[5] = pnil;                  /* marker (env) */
   /* set file variables - assumes files are initialised already */
  setVal(a.v.hrf,actv.v.hrf=filin);
  setVal(a.v.hpf,actv.v.hpf=filout);
  setVal(a.v.hlf,actv.v.hlf=filerr);
  setVal(a.v.hdb,actv.v.hdb=(initFlags & dbgFlag) ? ptru : pnil);
  initFlags &= ~dbgFlag; /* dbgFlag set in mreptcb from #PR by dispatcher  */
  lex.pt = &runtcb;
  getVal(a.v.hra).vr->varadd.np[0] = lex.wh; /* export  tcb for compiler!*/
  if (argc) { /* zero if called from alpsRestore */
    /* set optional argument list variable #AL */
    len = argc - optind;
    lex = getfs(len);
    setVal(a.v.hal,actv.v.hal=lex);
    for (i=optind; i<argc;i++) {
      lex.rg->ar = buildChr(argv[i],strlen(argv[i]));
      lex = lex.rg->dr;
    }
  }
  /* Initialise dispatcher tcb variables */
  runtcb   = pnil;
  lastcb   = pnil;
  displock = pnil;
  /* Math */
  sqrt2pi = sqrt(pi+pi);
}

static void initTcbTab() {
  dispstate ds;
  for (ds=done;ds<mxdsp;ds++) disptab[ds] = pnil;
  mreptcb  = allocTcb(ctl_depth);     /* main rep tcb      */
  mreptcb.tb->priority = TCB_MED_PRI; /* medium priority   */
  iotcb    = allocTcb(ctl_depth);     /* io & EZ callbacks */
  iotcb.tb->priority = TCB_SYS_PRI;   /* not pre-emptible  */
}

static char * helloMess ="";
static bool load_wspc(const uchar *name) {
#if (WSPSAV)
  int wfd;
  int i,j;
  pointer lex,wspa;
  lispTypes stype;
  whole block_size = WSP_BKSIZE*indlen;
  whole wbuf[WSP_BKSIZE];
  whole wsize,scale;
  time_t ts;
  
  if (0 > (wfd = open((char *)name,O_RDONLY))) {
    if ((errno != ENOENT) || (name != (uchar *)save_file))
      alpsMsg(5,"load: Unable to load workspace from ",
	       name,": ",strerror(errno),"\n");
    return false;
  }
  if (block_size != read(wfd,wbuf,block_size)) {
    alpsMsg(5,"load: Error starting load of workspace from ",
	     name,": ",strerror(errno),"\n");
    close(wfd);
    return false;
  }
  j = 0;
  wsize   = wbuf[j++];
  scale   = wbuf[j++];
  if (mapaddr.it) { /* unmap workspace if we already have it mapped */
    alpsMsg(1,"UnmapMem\n");
    alpsUnmapMem();
  }

  wspa.vd = alpsMapMem(scale);
 
  if (wspa.wh != wbuf[j]) {
    alpsMsg(5,"load: Workspace address space mismatch actual= ",
	     itox(wspa.wh)," saved ",itox(wbuf[0]),"\n");
    goto fail;
  }
  wsp_scale = scale; /* override wsp_scale with the saved scale */
  for (i = 0; i < no_root_vars; i++)          r.a[i].wh      = wbuf[j++];
  for (i = 0; i < apl_consts; i++)  {
    a.h[i].wh      = wbuf[j++];
    actv.h[i].wh   = wbuf[j++];
  }
  for (stype = s_nop; stype < s_con; stype++) s[stype].wh    = wbuf[j++];
  for (i=0;i<last_err;i++)                    errSym[i].wh   = wbuf[j++];
  for (i=0;i<MAX_CS;i++)                      chartab[i].wh  = wbuf[j++];
  for (i=0;i<MAX_CC;i++)                      sstrtab[i].wh  = wbuf[j++];
  for (i=0;i<=num_env_bkts;i++)               env_free[i].wh = wbuf[j++];
  /* Task control block stuff */
  for (i=0;i<mxdsp;i++) disptab[i].wh = wbuf[j++]; 
  mreptcb.wh = wbuf[j++];
  iotcb.wh   = wbuf[j++];     
  /* file stuff */
  filin.wh   = wbuf[j++];  
  filout.wh  = wbuf[j++];  
  filerr.wh  = wbuf[j++];
 /* mm acct stuff */
  for (i=0; i<alst+1;i++) mmacct[i]  = wbuf[j++]; 
  for (i=0; i<alst+1;i++) mmbgcnt[i] = wbuf[j++];
  ASSERT(j < WSP_BKSIZE);
  ts    = wbuf[j]; /* timestamp */
  pnil  = r.s.snil;
  ptru  = r.s.strue;
  lex   = r.s.wspc;
  for (i = 0; i < wsp_size / WSP_BKSIZE; i++) {
    if (block_size != read(wfd, lex.ip, block_size)) {
      alpsMsg(5,"load: Error reading workspace from ",name,": ",
	       strerror(errno),"\n");
      close(wfd);
      return false;
    }
    lex.wh += block_size;
  }
  close(wfd);
  lex = getVal(a.v.hcp); /* capability string */
  if (strcompare(CAPSTRING,strlen(CAPSTRING),lex)) {
    alpsMsg(3,"Warning capability mismatch: workspace: ",
	     mkString(lex,tline,"cap"), " current "CAPSTRING"\n");
    lex = mkChr((uchar *)CAPSTRING,strlen(CAPSTRING));
    setVal(a.v.hcp,lex);
  }
  helloMess = mkEmes(5,"Workspace restored from ",name,"\n  last saved: ",
		     ctime(&ts),"\n");
  return true;
 fail:
  close(wfd);
  alpsUnmapMem();
  return false;
#else
  return false;
#endif
}

#if (WSPSAV)
static bool save_wspc(const uchar *name) {
  int wfd;
  integer i,j,wlen;
  pointer lex;
  lispTypes stype;
  whole block_size = WSP_BKSIZE*indlen;
  whole wbuf[WSP_BKSIZE];
  if (0 > (wfd = open((char *)name, O_WRONLY | O_CREAT, 0600))) {
    alpsOutsn(alpserr,5,"save: Unable to save workspace in ",name,
	      ": ",strerror(errno),"\n");
    return(false);
  }
  j = 0;
  wbuf[j++] = wsp_size;   /* note size  */
  wbuf[j++] = wsp_scale;  /* note scale */
  for (i = 0; i < no_root_vars; i++)          wbuf[j++] = r.a[i].wh;
  for (i = 0; i < apl_consts; i++)      {
    wbuf[j++] = a.h[i].wh;
    wbuf[j++] = actv.h[i].wh;
  }
  for (stype = s_nop; stype < s_con; stype++) wbuf[j++] = s[stype].wh;
  for (i=0;i<last_err;i++)                    wbuf[j++] = errSym[i].wh;
  for (i=0;i<MAX_CS;i++)                      wbuf[j++] = chartab[i].wh;
  for (i=0;i<MAX_CC;i++)                      wbuf[j++] = sstrtab[i].wh;
  for (i=0;i<=num_env_bkts;i++)               wbuf[j++] = env_free[i].wh;
  /* Task control block stuff */
  for (i=0;i<mxdsp;i++) wbuf[j++] = disptab[i].wh; 
  wbuf[j++] = mreptcb.wh;
  wbuf[j++] = iotcb.wh;     
  /* file stuff */
  wbuf[j++] = filin.wh;
  wbuf[j++] = filout.wh;
  wbuf[j++] = filerr.wh;
  /* mm acct stuff */
  for (i=0; i<alst+1;i++) wbuf[j++] = mmacct[i]; 
  for (i=0; i<alst+1;i++) wbuf[j++] = mmbgcnt[i];
  wbuf[j]   = time(NULL); /* timestamp  */
  if (block_size != write(wfd, wbuf,block_size)) {
    alpsOutsn(alpserr,5,"save: Write errror, failed to save workspace in ",name,
	      ": ",strerror(errno),"\n");
    close(wfd);
    return(false);
  }
  lex = r.s.wspc;
  for (i = 0; i < wsp_size / WSP_BKSIZE; i++) {
    wlen = write(wfd, lex.ip, block_size);
    lex.wh += block_size;
  }
  close(wfd);
  return true;
}
#endif

#if (SOCKET)
FORWARD static int open_sock_passive(char *,char *);
FORWARD static int open_sock_active(char *,char *);
#endif

static void initFiles(bool no_alloc) {
  int ls, s,ferr;
  fibrec *tmpfib;
  s = -1; /* used as flag to know whether we are on a socket */
  if (!daemonic) {
    if (ssid) {
      close(0);
      close(1);
      close(2);
      if (0 > open(mTty,O_RDWR)){
	alpsError(mTty);
	alpsExit(1);
      }
      ferr = dup(0);
      ferr = dup(0);
    }
  } else { /* we are a daemon */
#if (SOCKET)
    if (ssid) {
      close(0);
      close(1);
    }
    ls = open_sock_passive("","alps");
    if (ls < 0)  {alpsError("alps listen failed"); alpsExit(1); }
    s  = alpsAccept(ls,(char *)tline,(char *)tline1);
    // alpsLog();
    close(ls);
    if (s < 0)  {alpsError("alpsAccept failed"); alpsExit(1); }
    if (ssid) {close(2); ferr = dup(0); ferr = dup(0);}
    else {
      filin          = allocFil(buildChr("alps",4));
      protect(filin);
      alpsin          = getFil(filin);
      alpsin->fd      = s;
      fparr[s]        = alpsin;
      alpsin->fdstat  = fio;
      alpsin->ibuf    = allocCV(max_line_len);
      alpsin->obuf    = allocCV(max_line_len);
      alpsin->readcb  = alpsRead;
      alpsin->twidth  = 80;
      alpsin->theight = 24;
      setNumval(a.v.hcw, alpsin->twidth); 
      setNumval(a.v.hch, alpsin->theight);
      alpserr        = alpsout = alpsin;
      filerr         =  filout  = filin;
      unprotect();
      return;
    }
#else
    alpsAbend("No socket support.");
#endif
  }
/* alpsin */
  if (!no_alloc) filin = allocFil(buildChr(mTty,strlen(mTty)));
  protect(filin);
  alpsin         = getFil(filin);
  alpsin->fd     = 0;
  fparr[0]       = alpsin;
  alpsin->fdstat = finp;
  if (!no_alloc) alpsin->ibuf = allocCV(max_line_len); 
  if ((s < 0) && isatty(0) && !rawTerm) {
    alpsInitTerm(alpsin,no_alloc);
    alpsin->readcb = (fcbT) alpsTermReader;
  } else alpsin->readcb = alpsRead; /* no TermReader if on socket */
  unprotect();
  /* alpsout */
  if (!no_alloc) filout = allocFil(buildChr(mTty,strlen(mTty)));
  protect(filout);
  alpsout         = getFil(filout);
  alpsout->fd     = 1;
  fparr[1]        = alpsout;
  alpsout->fdstat = fout;
  if (!no_alloc) alpsout->obuf = allocCV(max_line_len); 
  unprotect();
  /* alpserr */
  if (!no_alloc) filerr = allocFil(buildChr(mTty,strlen(mTty)));
  protect(filerr);
  tmpfib = getFil(filerr);
  tmpfib->fd     = 2;
  fparr[2]       = tmpfib;
  tmpfib->fdstat = fout;
  if (!no_alloc) tmpfib->obuf =  allocCV(max_line_len);   /*  file buffer */
  alpserr        = tmpfib; /* error output is now viable */
  unprotect();
}

/****************** INPUT OUTPUT ROUTINES *****************/

static bool isFilename(pointer fname) {
  int i;
  uchar *ptr;
  if (!isaChr(fname) || isNil(fname) || (nels(fname) < 1) ||
      (nels(fname) >= max_line_len)) return false;
  ptr = vadd(fname).sp;
  for (i=0;i<nels(fname);i++)
    if ((*ptr <= space) || (*ptr > tilde)) return false;
    else ptr++;
  return true;
}

static integer isNetaddr(pointer fname) {
  int i,j,k;
  uchar *ptr;
  if ((!isaChr(fname) && fname.vr->varadd.sp[0] != colon) || 
      isNil(fname) || (nels(fname) >= max_line_len)) return -1;
  ptr = fname.vr->varadd.sp;
  for (i=j=k=0;i<nels(fname);i++,ptr++) {
    if ((*ptr <= space) || (*ptr > tilde)) return -1;
    if (*ptr == colon) {j = i; k++; }
  }
  if ((k == 1) && ((j + 1) < nels(fname))) return j;
  return -1; /* more than 1 colon or colon at the end */
}

pointer alpsOpenF(pointer parm) { /* open file or socket */
  pointer lex,fname;
  fibrec *fex;
  integer j,fd,f1,len;
  char *fl,*host,*port;
  fstatT fs = finp;
  struct stat sb;
  int flags = O_RDONLY; /* default is read only */
#if (SOCKET)
  char portbuf[NI_MAXSERV];
  host = (char *)tline;
  port = (char *)portbuf;
#endif
  if (nargs(parm) < 1 || nargs(parm) > 3) 
    error(inv_funarg,"(open (<chr>  [[<chr>] [speed]]) | <file object>)");
  fname = arg1(parm);
#if (SOCKET==false)  
  if (!isFilename(fname)) error(inv_funarg,"invalid file name");
#else
  if (!isFil(fname) && !isFilename(fname))
    error(inv_funarg,"invalid file [name]");
  if (isFil(fname)) {
    fex = getFil(fname);
    if ((nargs(parm) > 1) || (fex->fdstat != fpass))
      error(inv_funarg,"(open <passive file object>)");
    fd = alpsAccept(fex->fd,host,port);
    if (0 > fd) error(ios_err,mkEmes(2,"passive open ",strerror(errno)));
    fs = fio;
    len = strlen(host);
    host[len++] = ':';
    strcpy(&host[len],port);
    fname = mkChr((uchar *)host,strlen(host));
  } else { // have an actual file / socket name
    j = isNetaddr(fname);
    if (j >=0) { /* we have a socket address presumably */
      mkString(fname,(uchar *)host,"file name");
      host[j]=0; /* kill colon */
      port   = &host[j+1];
      if (nargs(parm) == 2) { /* deal with flags */
	lex = arg2(parm);
	if (!isaChr(lex)) error(inv_funarg,"(open <chr> <chr>)");
	if ((len = nels(lex)) > 1) error(inv_funarg,"too many flags");
	if (vadd(lex).sp[0] != 'l') 
	  error(inv_funarg,"open with socket only takes ""l"" option");
	fd = open_sock_passive(host,port);
	fs = fpass; // This is a passive guy
      } else {
	fd = open_sock_active(host,port);
	fs = fio;
      }
      if (0 > fd) error(ios_err,
			mkEmes(4,"Offending socket address ",host,":",port));
    } else // it was not a socket address presumably
#endif
    { 
      if (nargs(parm) >= 2) { /* deal with flags */
	lex = arg2(parm);
	if (!isaChr(lex)) error(inv_funarg,"(openf <chr> <chr>)");
	if ((len = nels(lex)) > 2) error(inv_funarg,"too many flags");
	fl    = (char *)vadd(lex).sp;
	flags = f1 = 0;
	if ((len == 1) && (fl[0] == 'r')) { flags = O_RDONLY; fs = finp; }
	else 
	  if ((len == 1) && (fl[0] == 'w')) 
	    { flags = O_WRONLY | O_CREAT | O_TRUNC; fs = fout; }
	  else
	    if ((len == 2) && (fl[0] == 'r') && (fl[1] == 'w'))
	      { flags = O_RDWR | O_CREAT; fs = fio; }
	    else if ((len == 2) && (fl[0] == 'w') && (fl[1] == 'a'))
	      { flags = O_WRONLY | O_CREAT | O_APPEND; fs = fout; }
	    else if ((len == 3) && !memcmp(fl,"rwa",3))
	       { flags = O_RDWR | O_CREAT | O_APPEND; fs = fio; }
	     else error(inv_funarg,"bad flags need {\"r\",\"w\",\"rw\"}");
      }
      mkGString(fname,tline);
      fd = open((char *)tline,flags,00640);
      if (0 > fd) {
	alpsOutsn(alpserr,3,"Open failed on file \"",(char *)tline, "\"\n");
	return pnil;
      }
      if (isatty(fd) && nargs(parm)==3) {
	if (!isNum(arg3(parm))) {
	  close(fd);
	  error(inv_funarg,"tty speed must be numeric");
	}
	speed_t speed = getNum(arg3(parm));
	struct termios termdt;
	tcgetattr(fd,&termdt);
	cfsetspeed(&termdt,speed);
      }
    }
#if (SOCKET)
  }
#endif
  lex = allocFil(fname);
  protect(lex);
  fex = getFil(lex);
  fex->fd     = fd;
  fparr[fd]   = fex;
  fex->fdstat = fs;
  if ((fs == finp) || (fs == fio)) {
    fex->ibuf =  allocCV(max_line_len); 
    fex->readcb = alpsRead;
#if (USBTMC)
    fstat(fd,&sb);  /* Check for usbtmc device */
    if (S_ISCHR(sb.st_mode) && sb.st_rdev == makedev(180,176)) {
      fex->isatty = 2;  // so we can block signals in alpsRead
      fex->exceptcb = alpsRead;
      alpsOutsln(alpserr,"Info: Found usbtmc device");
    }
#endif
  }
  if ((fs == fout) || (fs == fio)) fex->obuf = allocCV(max_line_len); 
  unprotect();
  return(lex); /* send fob */
}

static pointer alpsOpenLoadF(pointer parm) { /* open file for loading */
  pointer fex,lex,fname,dname;
  integer fd=-1,f1,s1,len;
  char *n1,*fl;
  int i,flags       = O_RDONLY; /* default is read only */

  if (nargs(parm) != 1)  error(inv_numarg,"(load <filename>)");
  n1 = (char *)tline;
  fname = arg1(parm);
  if (!isFilename(fname)) error(inv_funarg,"invalid load file name");
  f1  = nels(fname);
  fl  = (char *)vadd(fname).sp;
  s1  = 0;               /* suffix length zero means no suffix */
  lex = lookup(a.v.hlp);
  if (isNil(lex) || fl[0] == '/' || fl[0] == '~' || fl[0] == '.') {
    mkGString(fname,tline);
    fd = open((char *)tline,flags,00640);
  } else {
    /* look for possible suffix i.e a dot somewhere in the file name */
    for (i=f1;i>=0;i--) if (fl[i] == '.' || fl[i] == '/') break;
    if ((i < 0) || fl[i] == '/') { /* no dot found make fname with suffix */ 
      s1 = strlen(alps_suffix);
      if ((f1+s1) >= max_line_len) error(inv_funarg,"File name too long");
      memcpy(tline,fl,f1);
      memcpy(&tline[f1],alps_suffix,s1);
      f1   += s1;
      fname = mkChr(tline,f1);
      fl    = (char *)vadd(fname).sp;
    }
    while (!isNil(lex)) {
      dname = car(lex);
      mkGString(dname,tline);
      len = strlen((char *)tline);
      if (tline[len-1] != '/') tline[len++] = '/';
      if (len + f1 >= max_line_len) error(inv_funarg,"File name too long");
      memcpy(&tline[len],fl,f1);
      len += f1;
      tline[len]  = 0;
      fd = open((char *)tline,flags,00640);
      if (fd >= 0) {
	//	fname = buildChr(tline,len); uncomment for fq file name
	break;
      }
      lex = lex.rg->dr;
    }
  }
  if (0 > fd) error(bad_file,(char *)tline);
  lex            = allocFil(fname);
  protect(lex);
  fex            = lex.fb->fobpvt;
  fex.fi->fd     = fd;
  fparr[fd]      = getFil(lex);
  fex.fi->fdstat = finp;
  fex.fi->ibuf   = allocCV(max_line_len); 
  fex.fi->readcb = alpsRead;
  unprotect();
  return(lex); /* send fob */
}

static pointer alpsOpenB(pointer parm) { /* open buffer */

  pointer lex,rex;
  fibrec *fex;
  integer n = nargs(parm);
  lex = arg1(parm);
  if (isSymbol(lex)) error(inv_funarg,"Buffer cannot be a symbol");
  if (n == 1) {
    rex = allocBuf(lex);
    fex = rex.fb->fobpvt.fi;
    fex->bufstat = ldvs_ok; 
    fex->ecci    = nels(lex);  
  } else {
    rex = arg2(parm);
    if (!isBuf(rex)) error(inv_funarg,"bad buffer object");
    fex = getFil(rex);
    resetFil(fex);
    fex->ibuf = fex->obuf = lex;
    fex->bufstat = ldvs_ok;
    fex->ecci    = nels(lex);  		     
  }
  return(rex); /* send fob */
}

static pointer alpsIOCtl(pointer parm) { /* takes pointer to fob */
  pointer lex;
  fibrec *fex;
  int fd;
  uint cmd,val;
  lex = arg1(parm);
  if (!isFil(lex)) error(ios_err,"(ioctl <file object>");
  fex = getFil(lex);
  fd = fex->fd;
  if ((fd >= 0) && (fex->fdstat != fclos)) {  /* file case */
    cmd = getWhole(arg2(parm));
    if (nargs(parm)==3) {
      val = getWhole(arg3(parm));
      ioctl(fd,cmd,&val);
      return(mkNum(val));
    } 
    ioctl(fd,cmd);
    return(ptru);
  }
  return(pnil);
}

pointer alpsCloseF(pointer lex) { /* takes pointer to fob */
  /*close and release file att*/
  fibrec *fex;
  int fd;

  if (!isaFil(lex)) error(ios_err,"(closef <file object>");
  fex = getFil(lex);
  if (lex.fb->type == buft) { /* buffer case */
    if (fex->fdstat != fclos) {
      fex->fdstat = fclos;
      return(ptru);
    } 
    return(pnil);
  }
  if ((fex->fdstat == fout) || (fex->fdstat == fio)) alpsFlush(fex);
  /* don't close sysfiles */
  if (fex != alpsin && fex != alpsout && fex != alpserr) { 
    fd = fex->fd;
    if ((fd >= 0) && (fex->fdstat != fclos)) {  /* file case */
      readcb[fd]   = NULL;
      exceptcb[fd] = NULL;
      fex->fd      = -1;
      fex->fdstat  = fclos;
      if (!close(fd))  return(ptru);
    }
  }
  return(pnil);
}

static pointer alpsSeekF(pointer lex, pointer rex) { /* fob num */

  fibrec *fex = getFil(lex);
  int fd      = fex->fd;
  long pos    = getNum(rex);
  if (lex.fb->type == buft) {/* buffer case */
    if (pos < 0 || pos >= nels(fex->ibuf)) error(dom_range,"Seek value OOB");
    if (fex->fdstat != fclos) {
      resetFil(fex);
      fex->bufstat = ldvs_ok;
      fex->ecci    = nels(fex->ibuf);
      fex->ifpos   = pos;
      return(rex);
    } 
    return(pnil);
  }
  if ((fd >= 0) && (fex->fdstat != fclos)) { /* file case */
    if (0<=lseek(fd,pos,SEEK_SET)) {
      resetFil(fex);
      fex->ifpos = pos;
      return(rex);
    } else error(ios_err,"operation failed");
  }
  return(pnil);
}

static pointer alpsRewindF(pointer lex) { /* takes pointer to fob */
  /*close and release file att*/
  fibrec *fex = getFil(lex);
  int fd      = fex->fd;
 
  return(isNil(alpsSeekF(lex,getVal(a.v.hai))) ? pnil : ptru);
}

static void closeFiles() {
  pointer lex;
  fstatT  f;
  int     fd;

  alpsWriteHistory();
  alpsCloseTerm();
  alpsFlush(alpsout);
  alpsFlush(alpserr);
 
  for (lex = r.s.fob_busy; lex.pt; lex = lex.fb->next) {
    if (isaFil(lex)) {
      f  = lex.fb->fobpvt.fi->fdstat;
      fd = lex.fb->fobpvt.fi->fd;
      /* close all files except std files */
      if ((f < fclos) && (fd > 2)) close(lex.fb->fobpvt.fi->fd);
    }
  }
}

#if (SOCKET)
static int open_sock_passive(char *interface, char * service) {
  int ls,s;
  whole port;
  struct hostent *hp;             /* pointer to host info for local host */
  struct servent *sp;		  /* pointer to service information      */
  struct sockaddr_in myaddr_in;	  /* for local socket address            */
  socklen_t len;
  int sreuse=1;
  memset ((char *)&myaddr_in, 0, sizeof(struct sockaddr_in));
  myaddr_in.sin_family = AF_INET;
  if (strlen(interface)) {
    if (!(hp = gethostbyname (interface))) {
      alpsError(mkEmes(2, interface," not found\n")); 
      return(-1);
    }
    myaddr_in.sin_addr.s_addr = ((struct in_addr *)(hp->h_addr))->s_addr;
  }  else   myaddr_in.sin_addr.s_addr = INADDR_ANY;
  if (stoi(service,&port)) {
    myaddr_in.sin_port = htons(port);
    } else {
    sp = getservbyname (service, "tcp");
    if (!sp) {alpsError("service not found in /etc/services"); return(-1);}
    myaddr_in.sin_port = sp->s_port;
  }
  if (0 > (ls = socket (AF_INET, SOCK_STREAM, 0))) {
    alpsError("sock driver can't create socket\n");
    return(-1);
  }
  if (bind(ls,(struct sockaddr *) &myaddr_in, sizeof(struct sockaddr_in)) == -1)
    {
      alpsError("sock driver can't bind\n");
      return(-1);
    }
  if (0 > setsockopt(ls,SOL_SOCKET, SO_REUSEADDR,&sreuse,sizeof(int))) {
    alpsError("open_socket_passive:setsockopt: SO_REUSEADDR");
    return -1;
  }
  /*  Set the socket state for Asynchronous
        if (ioctl(ls, FIOASYNC, &flag) == -1) {
	alpsError(" can't set async on socket ");
	exit(1);
        }
        flag = -getpid();
        if (ioctl(ls, SIOCSPGRP, &flag) == -1) {
                alpsError("can't get the process group.");
		}
*/

/*  flag = fcntl(ls,F_GETFL,&arg); */
/*  flag |= O_NDELAY; */
/*  fcntl(ls,F_SETFL,flag); */
  if (listen(ls, 0) == -1) {
    alpsError("sock driver can't listen\n");
    return(-1);
  }
  return ls;
}

 static integer alpsAccept(integer ls, char * host, char * service) {
  struct sockaddr_in peeraddr_in; /* for peer socket address             */
  struct linger l;
  int s,ret;
  int tnd=1;
  socklen_t len = sizeof(struct sockaddr_in);
 try_again:
  memset ((char *)&peeraddr_in, 0, len);
  l.l_onoff  = false;
  l.l_linger = 0;
  if (0 > (s = accept(ls,(struct sockaddr *) &peeraddr_in, &len))) {
    if (errno == EINTR) goto try_again;
    alpsError("alpsAccept failed");
    return(s);
  }
  ret = getnameinfo((struct sockaddr *) &peeraddr_in,len,
		    host, NI_MAXHOST, service, NI_MAXSERV, NI_NUMERICSERV);
  if (ret!=0) alpsError(mkEmes(2,"alpsAccept getnameinfo ",gai_strerror(ret)));

  if (0 > setsockopt(s,SOL_SOCKET,SO_LINGER,(char *)&l,sizeof(l))) {
    alpsError("alpsAccept:setsockopt: SO_LINGER");
    return -1;
  }
  if (0 > setsockopt(s,6,TCP_NODELAY,(char *)&tnd,sizeof(tnd))) {
    alpsError("alpsAccept:setsockopt: TCP_NODELAY");
    return -1;
  }
  return(s);
}

static int open_sock_active(char *host, char *service) {
 int s;
 struct hostent *hp;             /* pointer to host info for remote host */
 struct servent *sp;             /* pointer to service information      */
 struct sockaddr_in myaddr_in;   /* for local socket address           */
 struct sockaddr_in peeraddr_in; /* for peer socket address           */
 struct linger l;
 int tnd=1;
 l.l_onoff = 1;
 memset ((char *)&myaddr_in, 0, sizeof(struct sockaddr_in));
 memset ((char *)&peeraddr_in, 0, sizeof(struct sockaddr_in));
 l.l_linger= 0;
 peeraddr_in.sin_family = AF_INET;

 hp = gethostbyname (host);
 if (!hp) {
   alpsOutsn(alpserr,2,host," not found in /etc/hosts\n"); 
   return(-1);
 }
 peeraddr_in.sin_addr.s_addr = ((struct in_addr *)(hp->h_addr))->s_addr;

 sp = getservbyname (service, "tcp");
 if (!sp) {
   alpsOutsn(alpserr,2,service," not found in /etc/services\n"); 
   return(-1);
 }
 peeraddr_in.sin_port = sp->s_port;
 if (0 > (s = socket (AF_INET, SOCK_STREAM, 0))) {
	alpsError("sock driver unable to create socket\n");
	return(-1);
 }
 if (connect(s, (struct sockaddr *)&peeraddr_in,
	     sizeof(struct sockaddr_in)) == -1) {
	alpsError("sock driver unable to connect");
        return(-1);
 }
 if (0 > setsockopt(s,SOL_SOCKET,SO_LINGER,(char *)&l,sizeof(l))) {
         alpsError("conn_sock:setsockopt: SO_LINGER");
         return(-1);
 }
 if (0 > setsockopt(s,6,TCP_NODELAY,(char *)&tnd,sizeof(tnd))) {
         alpsError("conn_sock:setsockopt: TCP_NODELAY");
         return(-1);
 }
 return(s);
}

#endif

static char *parpairs="()[]{}";

/* print a constant and return printed length.   If !pflag don't print. */
static integer prconst(fibrec *out, pointer lex, integer itab, bool pflag,
		       lispTypes pc) {
  integer i, j, k, p, b, mxfw, pl, fw, cpos, nopad;
  integer flen, tlen, ndigs,ndigap, ndigbp, maxNdigs, maxNdigap, maxNdigbp;
  integer expLen, maxExpLen, sd, ep, fexp, pari;
  integer count, noels, pwidth, totcnt, rowc, rowl,lcnt,vctab;
  pointer nlex,el;
  uchar c;
  vtype ntype;
  idstr s;
  bool autofw,carry,dodq,firstcol,pcfw,manSign,fManSign,expSign,fExpSign,mFlag;

  integer prel(pointer lex, integer i) {
    integer mlen;
    if (ntype == num) {
      mlen  = mxfw;
      nopad = (mxfw!=0) || (i==0);
      fmtnum(s, &mlen, lex.np[i], p, b, sd, fexp, 0, 0, nopad);
    } else  if (ntype == cpx) {
      mlen  = mxfw/2;
      nopad = (mxfw!=0) || (i==0);
      fmtnum(s, &mlen,creal(lex.cp[i]), p, b, sd, fexp, 0, 0, nopad);
      s[mlen++] = 'j';
      k    = mlen;
      mlen = mxfw ? (mxfw - mxfw/2 -1) : 0;
      fmtnum(&s[k], &mlen,cimag(lex.cp[i]), p, b, sd, fexp, 0, 0, -1);
      mlen += k;
    } else if (ntype == chr) {
      c = lex.sp[i];
      s[0] = c;  
      mlen = 1;
      if (dodq) {
	if (c == dquot || c == excape) {s[0]=excape; s[1]=c;    mlen=2;
	} else if (c == eol)           {s[0]=excape; s[1]='n';  mlen=2;
	} else if (c == 28)            {s[0]=excape; s[1]='q';  mlen=2; 
	} else if (lex.sp[i] < space)  {s[0]=excape; s[1]=c+64; mlen=2;
	}
      }
    }
    return mlen;
  }
  
  pwidth    = (integer)getNumVal(a.v.hpw);
  if (!checkPW(pwidth)) error(sys_par,"#PW");
  integer fwarr[pwidth]; // field widths by column
  bool    msarr[pwidth]; // mantissa sign present
  integer ndarr[pwidth]; // number of digits 
  integer bparr[pwidth]; // number of digits before point 
  integer aparr[pwidth]; // number of digits after  point 
  integer ewarr[pwidth]; // exponent width
  bool    esarr[pwidth]; // exponent sign present
  p      = (!isNum(getVal(a.v.hpp))) ? -1 : (integer)getNumVal(a.v.hpp);
  b      = (integer)getNumVal(a.v.hob);
  mxfw   = (integer)getNumVal(a.v.hfw);
  pl     = (integer)getNumVal(a.v.hpl) -4;
  sd     = (integer)getNumVal(a.v.hsd);
  ep     = (integer)getNumVal(a.v.hep);
  nlex   = vadd(lex);
  ntype  = type(lex);
  k      = dims(lex);
  noels  = nels(lex);
  totcnt = 0;  /* total printed length */
  dodq   = !((pc == s_princ) || (pc == s_prat) || (pc == s_prinl));
  if (pc == s_princ) itab = -1;
  count  = 0;           /* chacracters printed in current line so far */
  lcnt   = 0;           /* line count */
  if (p < -1) { p=ep; fexp=1; }
  else fexp = 0;
  if (pflag && (itab >= 0)) alpsTab(out,itab); /* itab < 0 => no indentation */
  if (ntype != ref) {
    flen  = mxfw;
    if (k == 0) { /* scalar */
      if (ntype == num) {
	fmtnum(s, &flen, *nlex.np, p, b, sd, fexp, 0, 0, (mxfw==0)); /*nopad */
	if (pflag) alpsOutb(out, s, flen);
	count = flen;
      } else if (ntype == cpx) {
	fmtnum(s, &flen, creal(*nlex.cp), p, b, sd, fexp, 0, 0, (mxfw==0)); 
	if (fabs(cimag(*nlex.cp)) > 1e-41) {
	  s[flen++]='j';
	  k = flen;
	  flen = mxfw;
	  fmtnum(&s[k], &flen, cimag(*nlex.cp), p, b, sd, fexp,  0, 0, 1); 
	  flen += k;
	}
	if (pflag) alpsOutb(out, s, flen);
	count = flen;
      } else error(pgmr_inc,"Non numeric scalar");
    } else if (k==1) { /* Deal with vectors */
	if (dodq) {
	  count++;
	  if (pflag) {
	    if (itab >= 0) alpsTab(out,itab);
	    alpsOutc(out,isChr(lex)?dquot:lsbrc);
	  }
	}
	for (i=0;i<noels;i++) {
	  flen = prel(nlex,i);
	  if (i == 0) vctab = flen+dodq;
	  if ((itab >= 0) && (itab+count+flen > pwidth)) { /* start new line */
	    if (i == 0) memcpy(s,"??",flen=2);
	    if (pflag) {
	      if (!lcnt) itab += vctab;
	      alpsTab(out,itab);
	      alpsOutb(out,s,flen);
	    }
	    lcnt++;
	    count = flen;
	  } else {
	    if (pflag) alpsOutb(out,s,flen);
	       count += flen;
	  }
	  totcnt += flen;
	  if (pflag && totcnt >= pl) {
	    alpsOuts(out," ..\n"); /* print length abort */
	    count += 3;
	    break;
	  }
	}
	if (dodq) {
	  count ++;
	  if (pflag) alpsOutc(out,isChr(lex)?dquot:rsbrc);
	}
      } else { /* k > 1 */
      integer indarr[k];
      /* TODO: this shit must be rewritten! */
      rowl = getDim(lex,k-1); /* row length         */
      for (i = 0; i < k; i++) indarr[i] = 1; /* dimension level counters */
      for (i = 0; i < min(rowl,pwidth/2); i++) ewarr[i]=esarr[i] = 0;
      if ((mxfw == 0) && isNum(lex)) { /* automatic field width */
	pcfw  = autofw = true;
      tryagain:
	mFlag = false;
	fExpSign=fManSign=maxExpLen=maxNdigs=maxNdigap=maxNdigbp=0;
	for (i = 0; i < min(rowl,pwidth/2); i++) 
	  fwarr[i]=ndarr[i]=bparr[i]=aparr[i]=msarr[i]= 0;
	for (i = 0; i < noels; i++) { /* calc max fw */
	  rowc = min(i % rowl, (pwidth/2 - 1));
	  (void)asc((char *)s, &j, nlex.np[i],ewarr[rowc] ? ep : p, b,
		    &manSign,&ndigs,&ndigap,&expSign,&expLen,0,ewarr[rowc],
		    sd, fexp);
	  /*
		 alpsOutsn(alpsout,7,"asc: ",itos(ndigs),itosn(ndigap,3),
		   itosn(expSign,2),itosn(expLen,2),itosn(ewarr[rowc],2),"\n");
	  */
	  ndigbp = ndigs - ndigap;
	  if (manSign) fManSign = true;
	  if (expSign) fExpSign = true;
	  if (maxNdigs  < ndigs)  maxNdigs  = ndigs;
	  if (maxNdigbp < ndigbp) maxNdigbp = ndigbp;
	  if (maxNdigap < ndigap) maxNdigap = ndigap;
	  if (maxExpLen < expLen) maxExpLen = expLen;
	  mxfw = maxNdigs + (maxNdigap > 0) + fManSign;
	  if (maxExpLen > 0) mxfw +=  maxExpLen + expSign + 2;
	  /* record max field, exp and sign width for column rowc */
	  if (!msarr[rowc]) msarr[rowc] = manSign;
	  if (!ewarr[rowc]) { 
	    if ((ewarr[rowc] = expLen)) mFlag = true;
	    if (expSign)  esarr[rowc] = true;
	    if (!expLen || (ep < 0)) { /* not seen exp yet */
	      if (ndarr[rowc] < ndigs)   ndarr[rowc] = ndigs; 
	      if (bparr[rowc] < ndigbp)  bparr[rowc] = ndigbp;   
	      if (aparr[rowc] < ndigap)  aparr[rowc] = ndigap;   
	    } else { /* first exp seen, from now on calc fw on exps */
	    ndarr[rowc] = ndigs; 
	    aparr[rowc] = ndigap;
	    }
	  } else {
	    if (expLen || (ep < 0)) { /* exp mode */
	      if (ndarr[rowc] < ndigs)  ndarr[rowc] = ndigs; 
	      if (bparr[rowc] < ndigbp) bparr[rowc] = ndigbp;   
	      if (aparr[rowc] < ndigap) aparr[rowc] = ndigap;
	      if (ewarr[rowc] < expLen) ewarr[rowc] = expLen;
	      if (expSign)              esarr[rowc] = true;
	    }
	  }
	}
	// alpsOuts(alpsout,"rowf: \n"); 
	if (mFlag) goto tryagain;
	mxfw++; /* leading space */
	for (rowc=i=0; pcfw && (i<rowl);i++) {
	  fwarr[i] = bparr[i] + aparr[i] + (aparr[i] > 0) + msarr[i] + 1;
	  if (ewarr[i] > 0) // expLen + expSign + e + (point if not already)
	    fwarr[i] += ewarr[i] + esarr[i] + 1 + ((ndarr[i] > 1) && !aparr[i]);
	  /*
	  alpsOutsn(alpsout,8,"{",
		    itos(fwarr[i]),
		    itosn(ndarr[i],3),
		    itosn(bparr[i],3),
		    itosn(aparr[i],3),
		    itosn(ewarr[i],2),
		    itosn(esarr[i],2),
		    "}");
	  */
	  rowc += fwarr[i];
	  if (rowc > pwidth) pcfw = false; /* disble per col fw */
	}
	/*
	alpsOutsn(alpsout,7,"not per col widths: ",
		    itos(mxfw),
		    itosn(maxNdigs,3),
		    itosn(maxNdigap,3),
		    itosn(maxExpLen,2),
		    itosn(fExpSign,2),
		    "\n");
	alpsOutsn(alpsout,4,"total width ",itos(rowc)," pwidth ",itos(pwidth));
	alpsOutln(alpsout);
	*/
      } else { /* not auto field width */
	pcfw = autofw = false;
	fExpSign = fManSign = maxExpLen = maxNdigs = maxNdigap = maxNdigbp = 0;
      }
      if (dodq) {
	count++;
	if (pflag) alpsOutc(out,isChr(lex)?dquot:lsbrc);
      }
      firstcol = true;
      for (i = 0; i < noels; i++) {
	carry = true;
	rowc  = i % rowl;
	nopad = 1;
	if (autofw && (pcfw || !rowc)) {
	  flen    = fwarr[rowc];
	  ndigs   = ndarr[rowc];
	  ndigap  = aparr[rowc];
	  expLen  = ewarr[rowc];
	  expSign = esarr[rowc];
	} else {
	  flen    = mxfw;
	  ndigs   = maxNdigs;
	  expLen  = maxExpLen;
	  ndigap  = (expLen) ? ep : p;
	  expSign = fExpSign;
	}
	if (ntype == num) {
	  if (autofw && firstcol && mxfw) flen--; /* no leading space     */
	  nopad = autofw || firstcol;
	  fexp = (expLen!=0);
	  fmtnum(s,&flen,nlex.np[i],ndigap,b,sd,fexp,expSign,expLen,nopad);
	} else if (ntype == cpx) {
	  ASSERT(autofw==0); /* no auto field width for cpx for now */
	  tlen = flen;
	  cpos = 0;
	  if (!firstcol) s[cpos++] = space;
	  fmtnum(&s[cpos], &flen, creal(nlex.cp[i]), 
		 ndigap, b, sd, 0,  expSign, expLen, 1);
	  if (cimag(nlex.cp[i]) != 0.0) {
	    cpos += flen;
	    s[cpos++]='j';
	    fmtnum(&s[cpos],&tlen, cimag(nlex.cp[i]), 
		   ndigap, b, sd, 0, expSign, expLen, 1);
	    flen = tlen + cpos;
	  } else flen += cpos;
	} else { //  must be chr
	  c = nlex.sp[i];
	  s[0] = c;  
	  flen = 1;
	  if (dodq) {
	    if (c == dquot || c == excape) {s[0] = excape; s[1] = c;    flen=2;
	    } else if (c == eol)           {s[0] = excape; s[1] = 'n';  flen=2;
	    } else if (c == 28)            {s[0] = excape; s[1] = 'q';  flen=2; 
	    } else if (nlex.sp[i] < space) {s[0] = excape; s[1] = c+64; flen=2;
	    }
	  } 
	}
	if (totcnt >= pl) {
	  if (pflag) {
	    alpsOuts(out," .."); /* print length abort */
	    break;
	  }
	  count  += 3;
	  totcnt += count;
	}
	if (count + flen > pwidth) {
	  if (autofw) fw = fwarr[0];
	  else fw = flen + ((dodq)?1:0) - !nopad;
	  if (pflag) {
	    alpsTab(out,itab);
	    alpsOutn(out,space,fw);
	    alpsOutb(out,s,flen);
	  }
	  totcnt += count + 1;
	  count = flen + fw;
	  lcnt++;
	} else {
	  if (pflag) alpsOutb(out, s, flen);
	  count += flen;
	}
	firstcol = false;
	indarr[j = k-1]++;
	while (carry && j > 0) {
	  if (getDim(lex,j) >= indarr[j]) { carry = false;  break;  }
	  indarr[j--] = 1;
	  indarr[j]++;
	  totcnt += count;
	  count = 0;
	  if ((i+1) < noels) {
	    totcnt++;
	    lcnt++;
	    if (pflag) {
	      alpsTab(out,itab);
	      firstcol = true;
	      if (dodq) {
		alpsOutc(out,space); /* align array */
		count = 1; /* itab is not accounted for in here */
	      }
	    }
	  }
	}
      }
      if (dodq) {
	count ++;
	if (pflag) alpsOutc(out,isChr(lex)?dquot:rsbrc);
      }
      if (lcnt > 0) {
	count = 0;
	lcnt++;
	if (pflag) alpsOutln(out);
      }
    }
  } else { /* ref */
    if (dodq) {
      count +=2;
      if (pflag) alpsOuts(out,"$[");
    }
    for (i=0;i<noels;i++) { /* TODO: deal with dims */
      el = nlex.pt[i];
      flen =  alpsPrint(out,el,false,pc);
      if (i == 0) vctab = flen+(2*dodq);
      if ((itab >= 0) && (itab+count+flen > pwidth)) { /* start new line */
	if (i == 0) flen=2; /* can't fit first thing on line */
	if (pflag) {
	  if (!lcnt) itab += vctab;
	  alpsTab(out,itab);
	  if (i==0) alpsOutcn(out,'?',flen=2);
	  else flen  = alpsPrint(out,el,true,pc);
	}
	lcnt++;
	count = flen;
      } else {
	if (pflag) {
	  if (i > 0) alpsOutc(out,space);
	  flen = alpsPrint(out,el,true,pc);
	}
	count += flen + (i > 0);
      }
      totcnt += flen;
      if (pflag && totcnt >= pl) {
	alpsOuts(out," ..\n"); /* print length abort */
	count += 3;
	break;
      }
    }
    if (dodq) {
      count++;
      if (pflag) alpsOutc(out,rsbrc);
    }
  } /* ref */
  return count;
}  /*prconst*/

static pointer terpri(fibrec *out, integer n) {
  if (n) alpsOutn(out,eol,n);
  else out->olpos=0;        /* simulate carriage return */
  alpsFlush(out);
 return(ptru);
}

static integer alpsPrint(fibrec *out,pointer lex, bool pflag,lispTypes pc) {
  pointer rex = pnil; /* remainder of current list to print */
  pointer sa[max_line_len]; /* rest of list at stack depth */
  /* FIXME max_line_len should be pd*/
  integer l[max_line_len]; /* length of output at stack depth      */
  integer t[max_line_len]; /* current indent tab at stack depth    */
  integer p[max_line_len]; /* paren/bracket index at stack depth   */
  integer m,n=0;
  integer sp=0;  /* stack pointer for depth in nested lists */
  integer llen=0;
  integer pl = (integer)getNumVal(a.v.hpl) - 3; /* max print length octets */
  integer pd = (integer)getNumVal(a.v.hpd);     /* how deep we go          */
  integer ll = (integer)getNumVal(a.v.hll);     /* max list length         */
  integer itab = out->olpos;                    /* initial tab position    */
  integer pari = 0;
  char *spec;

  t[0] = itab;
  while (n <= pl) {
    if (lex.wh < r.s.wspc.wh || lex.wh > r.s.wspc_end.wh) {
      if (pflag) alpsOuts(out,"#<?*!>"); /* waddahellizat */
      n += 6;
    } else if (isAtom(lex)) {
      if (isSymbol(lex)) {
	if (pflag) alpsOutb(out, lex.at->varadd.sp, lex.at->varels);
	n +=  lex.at->varels;
      }  else if (isNCR(lex))  {
	m  = prconst(out,lex,sp?-1:itab,pflag,pc);
	n += m;
      } else if ((spec = specToString(lex))) {
	if (pflag) alpsOuts(out,spec);
	n += strlen(spec);
      }	 else {/* don't know what it is */
	if (pflag) alpsOuts(out,"#<bad atom>");
	n += 11;
      }
    }  else { /* must be list so pull it apart */
      if (isQuote(car(lex)) && !isNil(cdr(lex))) {
	if (!isAtom(cdr(lex)) && isNil(cddr(lex)) &&
	    (isSymbol(cadr(lex)) || (isList(cadr(lex))))) { /* quote with '*/
	  if (pflag) alpsOutc(out,quote);
	  n++;
	  lex = cadr(lex);
	  continue;
	} else {  /* (quote . xxx) */
	  if (pflag) alpsOutc(out,lpar);
	  n++;
	  p[sp] = 0;
	  t[sp] = n;
	  sa[sp++] = cdr(lex);
	  lex      = car(lex);
	  continue;
	}
      } else {/* not atom and not (quote car or cdr nil) */
	if ((isEqq(car(lex),s[s_implod])) && (listLen(lex)==4) && 
	    (isEqq(mcar(mcar(cdddr(lex))),s[s_list])))  { /* it's a {...} */
	  pari = 2;
	  if (isTru(mcar(cddr(lex)))) {
	    if (pflag) alpsOutc(out,dollar);
	    n++;
	  }
	  lex  = mcdr(mcar(cdddr(lex)));
	} else pari = 0;
	if (sp < pd) { /* stack not reached print depth */
	  if (pflag) alpsOutc(out,parpairs[pari*2]);
	  n++;
	  sa[sp] = cdr(lex);
	  l[sp]  = 1;
	  t[sp]  = -1;
	  p[sp]  = pari;
	  lex    = car(lex);
	  sp++;
	  continue;
	} else {
	  if (pflag) alpsOuts(out,"...");
	  n +=3;
	  if (sp > 0) sa[sp-1] = pnil; /* skip the rest */
	}
      }
    }
  back_track:
    if (sp == 0) return n;
    while (sp > 0) {
      rex  = sa[--sp];
      llen = l[sp];
      if (isNil(rex)) {
	n++; 
	if (pflag) {
	  if (!t[sp]) alpsTab(out,itab);
	  alpsOutc(out,parpairs[p[sp]*2+1]); 
	}
      } else break;
    }
    if (isNil(rex)) return n;
    if (isCnt(rex) || isAtom(rex)) {
      if (pflag) alpsOuts(out," . ");
      n += 3;
      sa[sp++] = pnil;
      lex = rex;
    } else {
      if (llen < ll) {
	//	if (t[sp] > 0) {
	//  if (n == t[sp]) {
	//    if (pflag) { alpsOutc(out,eol); alpsTab(out,n+itab); }
	//  } else  {	  
	    if (pflag) alpsOutc(out,space);
	    n++;
	    //  }
	    //}
	lex    = car(rex);
	l[sp]  = llen + 1;
	sa[sp] = cdr(rex);
	sp++;
      } else {
	if (pflag) alpsOuts(out,"..)"); /* list len abort */
	n += 3;
	goto back_track;
      }
    }
  }
  if (pflag) alpsOuts(out,"...."); /* print len abort */
  n += 4;
  return n;
}

static integer plen0(pointer lex,lispTypes pc) {
  return alpsPrint(alpserr,lex,false,pc); /* should never print! */
}

static pointer prin0(fibrec *out, pointer lex,lispTypes pc) {
  (void)alpsPrint(out,lex,true,pc);
  return(lex);
}

static pointer pp(fibrec *out, pointer lex,lispTypes pc) {
  integer t;
  pointer x,y,z;
  integer pp_dm, pp_pw;
  bool crank_back;
  bool fun_tab;

void ppaux(fibrec *out, pointer x, integer t, integer depth) {
  integer n;
  bool jumpeol = false;
  /* ASSERT(isList(x)) */
  if (depth >= pp_dm) {    alpsOuts(out,"..");    return;  }
  if (isAtom(x)) {    prin0(out,x,pc);    alpsOutc(out,space);    return;  }
  if (isQuote(car(x)) && !isAtom(cdr(x))) {
    alpsOutc(out,quote);
    ppaux(out,cadr(x),out->olpos, depth);
  } else {
    alpsTab(out,t);
    alpsOutc(out,lpar);
    n = t = out->olpos;
    crank_back = fun_tab = false;
    while (!isAtom(x)) { /* scud along list */
      if (jumpeol || n >= pp_pw) { 
	alpsTab(out,t); n = t; crank_back = true; jumpeol = false;
      }
      if (isAtom(car(x)) && !isNil(car(x))) { // atom in list
	if ((out->olpos + plen0(car(x),pc)) >= pp_pw) {
	  alpsTab(out,t);
	  crank_back = true;
	}
	prin0(out,car(x),pc);
	if (!isNil(cdr(x))) {
	  alpsOutc(out,space);
	  if (n == t) { /* was the first fella */
	    if (!crank_back && !fun_tab && isNotabfun(car(x))
		//	|| (isSym(car(x)) && !isAtom(getVal(car(x))) &&
		//  isNotabfun(car(getVal(car(x)))))
		) { 
	      t += 2;
	      n = out->olpos;
      	      fun_tab = true;
	    } else n = out->olpos;
	  } else n = out->olpos;
	}
      } else { // list in list
	if (out->olpos+plen0(car(x),pc) < pp_pw) {
	  prin0(out,car(x),pc);
	  n = out->olpos;
          
	} else {
	  ppaux(out, car(x), fun_tab ? t : n, depth + 1);
	  jumpeol = true;
	}
	if (!isNil(cdr(x)) && n < pp_pw) { 
	  alpsOutc(out,space); n++;
	  jumpeol =  (out->olpos+plen0(cadr(x),pc) >= pp_pw);
	}
      } 
      x = x.rg->dr;
    } /* while */
    if (!isNil(x)) {
      if ((out->olpos + 2 + plen0(x,pc)) >= pp_pw) {
	  alpsTab(out,t);
	  crank_back = true;
      }
      alpsOuts(out,". ");
      prin0(out,x,pc);
    }
    alpsOutc(out,rpar);
  }
}

  x     = cargn(0,lex);
  y     = cargn(1,lex);
  z     = cargn(2,lex);
  t     = (isNum(y)) ? getInt(y) : 0;
  pp_dm = (isNum(z)) ? getInt(z) : 32767;
  pp_pw = (integer)getNumVal(a.v.hpw);
  crank_back = false;
  ppaux(out, x, t, 0);
  alpsOutc(out,eol);
  alpsFlush(out);
  return(ptru);
}

static void FGOTOXY(fibrec *out, int x, int y) { // x = hori ; y =vert
  if (y == 0) { // stay on line
    if (x == 0) return; // stay on column also => do not move
    alpsOutsn(out,3,"\015\033[",itos(x),"C"); // stay on line move to column x
  } else { // goto line y
    if (x == 0) x = out->olpos + 1; // stay on column
    alpsOutsn(out,5,"\033[",itos(y),";",itos(x),"H"); // vertical ; horizontal
  }
}

static pointer prat(fibrec *out,  pointer parm, lispTypes pc)  {
  pointer row,col,lex;
  int irow,icol,oolpos,n;
  n    = nargs(parm);
  row  = arg1(parm); 
  col  = arg2(parm);
  lex  = pnil;
  icol = getInt(col);
  irow = getInt(row);
  if ((irow < 0) || (icol < 0)) error(dom_range,"prat row or col < 0");
  oolpos = out->olpos;
  FGOTOXY(out, icol, irow);
  if (n == 3) {
      lex = arg3(parm);
      alpsOutb(out,vadd(lex).sp,nels(lex));
  }
  if (icol > 0)  out->olpos = icol-1; // overide automatic update
  else out->olpos = oolpos;
  alpsFlush(out);
  return(lex);
}

static pointer dobeep(fibrec *out, pointer f, pointer d) {
  integer freq, dura;

  if (isNum(f)) freq = getWhole(f) & 63;
  else freq = 5;
  if (isNum(d)) dura = getWhole(d) & 255;
  else dura = 15;
#if (SOUND)
  alps_beep(freq,dura);
#else
  alpsOutc(out,'\007');
  alpsFlush(out);
#endif
  return(pnil);
}

static pointer print(fibrec *out, pointer lex, lispTypes pc) {
  pointer rex;
  if ((out->fdstat != fout) && (out->fdstat != fio)) 
    error(ios_err,"Can't print on readonly file");
  if ((pc == s_prinl) && isList(lex)) {
    rex = lex; /* remember result */
    while (!isNil(rex)) {
      prin0(out,mcar(rex),pc);
      rex = mcdr(rex);
      if (!isNil(rex) && !isNum(mcar(rex))) alpsOutc(out,space);
    }
  } else {
    prin0(out,lex,pc);
    if (pc == s_print) alpsOutc(out,eol);
    else if (pc != s_princ) alpsOutc(out,space);
  }
  alpsFlush(out);
  return(lex);
}

static pointer dodir(pointer lex) {
  pointer rex,orex,res;
  DIR *dp;
  struct dirent *ep;
  struct stat statb;
  char dirnam[max_line_len];

  mkGString(lex,(uchar *)dirnam);
  res = orex = pnil;
  if (0 > lstat(dirnam,&statb)) {
    if (errno == ENOENT) return(res);
    error(bad_file,strerror(errno));
  }
  if (!S_ISDIR(statb.st_mode)) return (lex); 
  dp  = opendir(dirnam);
  if (dp == NULL) error(bad_file,"dir");
  while ((ep = readdir(dp))) {
    rex = getfs(1);
    if (isNil(res)) {res = orex = rex; protect(res); } /* prot head of list */
    else { orex.rg->dr = rex; orex = rex; }
    rex.rg->ar = buildChr(ep->d_name,strlen(ep->d_name));
  }
  closedir(dp);
  if (!isNil(res)) { rex.rg->dr = pnil; unprotect(); }
  return(res);
}

static pointer dofstat(pointer lex) {
  pointer res,rex,ftyp;
  struct stat statb;
  char filnam[max_line_len];
  struct passwd *pwent;
  struct group  *grent;

  mkGString(lex,(uchar *)filnam);
  res = pnil;
  if (0 > lstat(filnam,&statb)) {
    if (errno == ENOENT) return(res);
    error(bad_file,strerror(errno));
  }
  rex = res = getfs(10);
  protect(res);
  rex.rg->ar =  (S_ISDIR(statb.st_mode)) ? pnil : lex;// 0 type
  rex = cdr(rex);
  rex.rg->ar = mkNum(statb.st_size);                  // 1 Size in octets
  rex = cdr(rex);
  rex.rg->ar = // (pwent = getpwuid(statb.st_uid)) ?    // 2 UID
    //    mkChr((uchar *)pwent->pw_name,strlen(pwent->pw_name)) :
    mkNum(statb.st_uid);                   
  rex = cdr(rex);
  rex.rg->ar = // (grent = getgrgid(statb.st_gid))  ?    // 3 GID
    //    mkChr((uchar *)grent->gr_name,strlen(grent->gr_name)) :
    mkNum(statb.st_gid);                  
  rex = cdr(rex);
  rex.rg->ar = mkNum(statb.st_atime);                 // 4 Access time
  rex = cdr(rex);
  rex.rg->ar = mkNum(statb.st_mtime);                 // 5 Modification time
  rex = cdr(rex);
  rex.rg->ar = mkNum(statb.st_ctime);                 // 6 Status change time
  rex = cdr(rex);
  rex.rg->ar = mkNum(statb.st_mode & 07777);          // 7 File mode
  rex = cdr(rex);
  rex.rg->ar = mkNum((statb.st_mode >> 12) & 017);    // 8 File type
  rex = cdr(rex);
  rex.rg->ar = mkNum(statb.st_nlink);                 // 9 Number of links
  unprotect();
  return(res);
}


/*  ************************* SYSTEM OUTPUT SECTION  *********************  */

static char *fstat_NAMES[] = { "FINP", "FOUT", "FIO", "FPASS", "FCLOS" } ;

static integer printFree(fibrec * out, integer n) {
  /* zero for summary, one for detail, two gory detail */
  /*free space print*/
  /* ASSERT must not modify any global state whatsoever since it
     can be called from the boogey man for isfSet(trcFlag) */
  pointer lex, rex, mex;
  integer lfree, ffree, rfree;   /*linear,forward and reverse free spc*/
  integer lbusy, cbusy, ccnt, flen, i,j,k;  /*linear and current list busy spc*/
  integer lffrag, sffrag, lbfrag, sbfrag, nbusy, nfree;
  integer ebbusy,embusy,ebfree,esfree,esbusy,emfree = 0;
  integer cntb, cntf, cntmf, cntmb;
  integer stkb, stkf, stkmf, stkmb;
  integer tcbb, tcbmb, atot, fobb, fobcnt;
  integer fsa[fclos+1];
  integer lactca[alst+1],lactcc[alst+1]; /* linear accout check amount,count */
  integer retval = 1; /* its all good */
  acctp ac;
  fstatT f;

  ebfree=embusy = 0;
  flen = 0;
  /* count busy env blocks and sum size in arg slots */
  for (i=0;i<=num_env_bkts;i++) {/* touch freebies */
    for (j=0,lex = env_free[i]; !isNil(lex); lex = nenv(lex)) {
      ASSERT(!isMarked(lex)); mark(lex);
    }
  }
  lex = r.s.envbase;
  i = 0;
  while (lex.wh < r.s.envlim.wh) {
    if (isMarked(lex)) unMark(lex);
    else { embusy += nargs(lex); i++; }
    lex.wh +=  (nargs(lex) + env_rgohd) * rg_len;
  }
  if (n)  alpsOuts(out,"Env block details\n");
  ebbusy = i;
  for (i=0;i<num_env_bkts;i++) {
    for (j=0,lex=env_free[i];!isNil(lex);lex=nenv(lex)) j++;
    if (j && n) alpsOutsn(out,7,"Env(",itosn(i,3),") Nblks = ",itosn(j,6),
		     " Tsize = ",itosn(i*j,6),"\n");
    ebfree += j;
    emfree += i*j;
  }
  for (i=j=0,lex=env_free[num_env_bkts];!isNil(lex);lex=nenv(lex)) {
    j++;
    i += lex.ev->nargs;
  }
  if (n) alpsOutsn(out,7,"Env(",itosn(num_env_bkts,3),") Nblks = ",itosn(j,6),
		   " Tsize = ",itosn(i,8),"\n");
  ebfree += j;
  emfree += i;
  if (n) alpsOutsn(out,5, "Env(tot) Nblks = ",itosn(ebfree,6),
		   " Tsize = ",itosn(emfree,8),"\n");
  esfree = rg_len*(emfree+ebfree*env_rgohd);
  esbusy = rg_len*(embusy+ebbusy*env_rgohd);
  
  cntb=cntf=cntmf=cntmb=0;      /* count continuation blocks  */
  stkb=stkf=stkmf=stkmb=0;      /* and stacks                 */
  for (lex=r.s.cnt_busy; lex.pt; lex=lex.ct->next) { /* busy  */
    cntb++; cntmb += getBkSiz(lex);
    if ((lex.ct->stksiz)) {
      stkb++;
      /*adj:                 alc_link +  overhead + type_tag */
      j   =  getStkSize(lex) + indlen + alcbk_ohd + indlen;
      rex.wh = lex.ct->stack.wh - indlen;
      if (j != getBkSiz(rex))
	alpsOutsn(out,7,"Bad stksize @",itox(rex.wh),
		  " stksiz ",itos(j)," bksz ",itos(getBkSiz(rex)),"\n");
      stkmb += j;
    }
  }
  for (lex=r.s.cnt_free; lex.pt; lex=lex.ct->next) { /* free */
    cntf++; cntmf += getBkSiz(lex);
    if ((i = lex.ct->stksiz)) {
      stkf++;
      j =  i+indlen+alcbk_ohd+indlen;/*adj: alc_link+ohd+type_tag */
      rex.wh = lex.ct->stack.wh - indlen;
      if (j != getBkSiz(rex))
	alpsOutsn(out,7,"Bad stksize @",itox(rex.wh),
		  " stksiz ",itos(j)," bksz ",
		  itos(getBkSiz(rex)),"\n");
      stkmf += j;
    }
  }

  tcbb = tcbmb = 0; /* count live tcbs */
  if (!isNil(runtcb))  {tcbb++; tcbmb += getBkSiz(runtcb); }
  for (i=done;i<mxdsp;i++) 
    for (lex=disptab[i];!isNil(lex);lex=lex.tb->next) {
      rex.wh = lex.wh - alcbk_ohd;
      ASSERT(rex.bk->bsize < 0); // Bastard must be busy
      tcbb++; tcbmb += getBkSiz(lex);
    }
  
  fobcnt = fobb = 0; /* count fobs */
  for (lex=r.s.fob_busy; lex.pt; lex = lex.fb->next) {
    fobcnt++;
    fobb += getBkSiz(lex);
  }
 
  if (n>1) {
    alpsOutsn(out,7,"bbp @",itox(r.s.bbp.wh),
	      " base @",itox(r.s.bbp.bb->bbase.wh),
	      " roof @",itox(r.s.bbp.bb->broof.wh),"\n");
    alpsOutsn(out,5,"FS   Base @",itox(r.s.fspc.wh),
	      " lim @",itox(r.s.fspc_end.wh),"\n");
    alpsOutsn(out,13,
	    "Env: Base ",itox(r.s.envbase.wh),
	    " lim ",itox(r.s.envlim.wh),
	    "\n blks ",itos(r.s.env_blks),
	    " = busy ",itos(ebbusy),
	    " + free ",itos(ebfree),
	    " diff = ",itos(r.s.env_blks-ebbusy-ebfree),"\n");
    lex = runtcb.tb->cntptr;
    alpsOutsn(out,9,
	      "Ctrl Base @",itox(lex.ct->stack.wh),
	      " stk @",itox(lex.ct->ctlsp.wh),
	      " lim @",itox(lex.ct->stklim.wh),
	      " usage = ",itos(100*(lex.ct->ctlsp.wh - lex.ct->stack.wh)/
			       (lex.ct->stklim.wh - lex.ct->stack.wh)),
	      "%\n");
    alpsOutsn(out,3,"nil @",itox(pnil.wh),"\n");
  }
  if (n==2) {
    alpsOuts(out,"\n    Blk        address    Freesize  Bcnt    Busysize\n");
  }
  /* clear accounting check records */
  for (ac=0;ac<=alst;ac++)  lactca[ac] = lactcc[ac] = 0;
  /*tot up stuff currently on vreched */
  cbusy = 0;
  ccnt  = 0;
  for (lex=r.s.vreched; lex.pt; lex=lex.bk->bnext) {
    cbusy += labs(lex.bk->bsize);
    ccnt++;
    rex.wh = lex.wh + alcbk_ohd;
    //ASSERT(!isMarked(rex));
    //alpsOutsn(out,4,"Vh ",itosn(lex.bk->bsize,8)," tag ",itox(rex.at->idntt));
    //dprtln(out,rex,20);
  }
    
  /*linear free and busy*/
  lfree = 0;
  lbusy = 0;
  nbusy = 0;
  nfree = 0;
  lffrag = lbfrag = 0;
  sffrag = sbfrag =  ((integer)(((whole) -1) >> 1));
  i = j = 0;
  k = alst+1;
  lex = r.s.bbp.bb->bbase;
  while (lex.wh < r.s.bbp.bb->broof.wh) {
    flen = labs(lex.bk->bsize);
    ac = alst+1;
    if (lex.bk->bsize < 0) {
      lbusy += flen;
      i     += flen;
      j++;
      nbusy++;
      if (flen > lbfrag) lbfrag = flen;
      if (flen < sbfrag) sbfrag = flen;
      rex.wh = lex.wh + alcbk_ohd;
      /* must check for this first as these two have no id in 1st word */
      if (isFS(rex) || isEqq(rex,r.s.fs_bm)) ac = alst; 
      else if (isAnon(rex)) ac = anon;
      else if (isNCRS(rex)) ac = avrc;
      else if (isEnv(rex))  ac = aenv;
      else if (isStk(rex))  ac = astk;
      else if (isCnt(rex))  ac = acnt;
      else if (isTcb(rex))  ac = atcb;
      else if (isFob(rex))  ac = afob;
      k = ac;
      if (ac <= alst) {
	lactca[ac] += flen; lactcc[ac]++;
      	if (n>2) {
	  if (ac==avrc) {
	    if (flen > n) {
	      int vi,vl=1;
	      for (vi=0;vi<dims(rex);vi++) vl *= getDim(rex,vi);
		alpsOutsn(out,5,typeDesc[type(rex)],itosn(flen,10),
			  " nels ",itosn(vl,7)," ");
		if (vl < (isaChr(rex) ? 40 : 16)) dprtln(out,rex,40);
		else alpsOutln(out);
	    }
	  } else {
	    if (ac < alst)
	      alpsOutsn(out,3,typeDesc[type(rex)],itosn(flen,10),"\n");
	    else if (isFS(rex))
	      alpsOutsn(out,3,"FS             ",itosn(flen,10),"\n");
	    else if  (isEqq(rex,r.s.fs_bm))
	      alpsOutsn(out,3,"FS BM Table    ",itosn(flen,10),"\n");
	    else
	      alpsOuts(out,"Houston ...\n");
	  }
	}
      } else {
	alpsOutsn(out,5,"Bad blk @",itox(rex.wh),
		  " id-> ",itox(rex.at->idntt),"\n");
      }
    } else {
      //  mex.wh = lex.wh + alcbk_ohd;
      //  alpsOutsn(out,2,stox(mex.sp,flen-alcbk_ohd),"\n");
      lfree += flen;
      nfree++;
      if (flen > lffrag) lffrag = flen;
      if (flen < sffrag) sffrag = flen;
      if (n > 2) {
	if (flen > n)
	  alpsOutsn(out,7,"free           ",itosn(flen,10),
		    " start ",itox(lex.wh),
		    " end ",itox(lex.wh+flen),"\n");
      }
    }
    if (n == 2 && ac==alst+1) {
      alpsOutsn(out,8, "    ",itox(lex.wh),itosn(flen,12),
		itosn(j,6),itosn(i,12)," ",mmacctd[k],"\n");
      i = j = 0;
    }
    lex.wh += flen;
  } 
  if (n == 2 && i > 0) 
  	alpsOutsn(out,8, "    ",itox(lex.wh),itosn(0,12),
		  itosn(j,6),itosn(i,12)," ",mmacctd[ac],"\n");
    /* check free block list along forward link */
  lex   = r.s.bbp.bb->bnext;
  ffree = 0;
  while (lex.pt != r.s.bbp.pt) {
    ffree += lex.bk->bsize;
    lex    = lex.bk->bnext;
  }
  /*reverse*/
  lex = r.s.bbp.bb->bprev;
  rfree = 0;
  while (lex.pt != r.s.bbp.pt) {
    rex.wh = lex.wh + lex.bk->bsize;
    if (rex.bk->bsize > 0)
      alpsOutsn(out,3,"Bad len after free blk ",itos(rex.bk->bsize),"\n");
    rfree += lex.bk->bsize;
    lex    = lex.bk->bprev;
  }
  if (n) {
    alpsOutsn(out,5," Vrec busy count: ",itos(ccnt),
	      "  tot Size ",itos(cbusy),"\n");
    alpsOuts(out,"FWS fragment detail\n");
    alpsOutsn(out,5,
      "Largest:  free=",itosn(lffrag,8),"; busy=",itosn(lbfrag,8),"\n");
    alpsOutsn(out,5,
      "Smallest: free=",itosn(sffrag,8),"; busy=",itosn(sbfrag,8),"\n");
  }
  atot = 0;
  for (ac = anon;ac<=alst;ac++) atot += mmacct[ac];
  if (rfree != ffree || ffree != lfree || atot != lbusy) {
    alpsOutsn(out,7,
	      "free lin:",itosn(lfree,10),
	      ";    fwd:",itosn(ffree,10),
	      ";    rev:",itosn(rfree,10),";\n");
    alpsOutsn(out,5,
	      "busy i:",itosn(atot,10),
	      ";    c:",itosn(cbusy,10),";\n");
    alpsOutsn(out,7,
	      "busy l:",itosn(lbusy,10),
	      " busy g:",itosn(atot,10),
	      " diff = ",itosn(lbusy - flen,10),"\n");
    flen = ffree + atot;
    alpsOutsn(out,7, 
	      "total bb:",itos(r.s.bbp.bb->bbsz),
	      " - (free+busy):",itos(flen),
	      " = ",itos(r.s.bbp.bb->bbsz - flen),"\n");
  }
  flen = r.s.nfsfree;
  for (f = finp; f <= fclos; f++) fsa[(integer)f] = 0;
  for (lex = r.s.fob_busy; lex.pt; lex = lex.fb->next) {
    if (isFil(lex)) {
      f = lex.fb->fobpvt.fi->fdstat;
      fsa[f]++;
    }
  }
  alpsOutsn(out,3,"Memory allocation Summary Total Workspace size: ",
	   itos((512 + wspBsiz)/1048576),"MB\n\n");
  alpsOuts(out,	
	   "Type        Free       Busy      Total      Diff     Nfree"
	   "     NBusy GCcnt\n");
  alpsOutsn(out,8,
	    "WS:  ",itosn(ffree,11),itosn(lbusy,11),itosn(r.s.bbp.bb->bbsz,11),
	    itosn(r.s.bbp.bb->bbsz - lbusy - ffree,10),
	    itosn(nfree, 10),itosn(nbusy, 10),"\n");
  alpsOutsn(out,7,
	    "ATM:            ",itosn(lactca[avrc],11),itosn(lactca[avrc],11),
	    "                    ",itosn(lactcc[avrc],10),
	    itosn(mmbgcnt[avrc],6),"\n"); 
  alpsOutsn(out,9,
	    "FS:  ",itosn(flen*rg_len,11), itosn((fs_size-flen)*rg_len,11),
	    itosn(fs_size*rg_len,11),itosn(0,10),itosn(flen,10),
	    itosn(fs_size-flen,10),itosn(mmbgcnt[alst],6),"\n");
  alpsOutsn(out,9,
	    "Env: ",itosn(esfree,11),itosn(esbusy,11),itosn(envsize*indlen,11),
	    itosn(envsize*indlen-esfree-esbusy,10),
	    itosn(ebfree,10),itosn(ebbusy,10),itosn(mmbgcnt[aenv],6),"\n");
  alpsOutsn(out,9,
	    "Stk: ",itosn(stkmf,11),itosn(stkmb,11),itosn(stkmb+stkmf,11),
	    itosn(mmacct[astk]-(stkmb+stkmf),10),
	    itosn(stkf,10),itosn(stkb,10),itosn(mmbgcnt[astk],6),"\n");
  alpsOutsn(out,9,
	    "Cnt: ",itosn(cntmf,11),itosn(cntmb,11), itosn(cntmb+cntmf,11),
	    itosn(mmacct[acnt]-(cntmb+cntmf),10),
	    itosn(cntf,10),itosn(cntb,10),itosn(mmbgcnt[acnt],6),"\n");
  alpsOutsn(out,9,
	    "Tcb: ",itosn(0,11),itosn(tcbmb,11), itosn(tcbmb,11),
	    itosn(mmacct[atcb]-tcbmb,10),
	    itosn(0,10),itosn(tcbb,10),itosn(mmbgcnt[atcb],6),"\n");
  alpsOutsn(out,9,
	    "Fob: ",itosn(0,11),itosn(fobb,11),itosn(fobb,11),
	    itosn(mmacct[afob] - fobb,10),itosn(0,10),itosn(fobcnt,10),
	    itosn(mmbgcnt[afob],6),"\n");
 
 /* Accounting check */
 if (atot != lbusy || n > 0) {
   integer lctot,dtot,lcnt;
   lcnt = lctot = dtot = 0;
   if (atot != lbusy) retval = 0; /* all is not good */
   alpsOuts(out,"\nBLock allocation Summary\n");
   alpsOuts(out,"Type"); alpsTab(out,strlen(mmacctd[anon]));
   alpsOuts(out,"MMAccounting   Linkcheck      Diff Linkcount\n");
   for (ac = anon;ac<=alst;ac++) {
      alpsOutsn(out,6,mmacctd[ac],itosn(mmacct[ac],12),
		itosn(lactca[ac],12),
		itosn(mmacct[ac]-lactca[ac],10),
		itosn(lactcc[ac],10),"\n");
      lctot += lactca[ac]; dtot += mmacct[ac]-lactca[ac]; lcnt += lactcc[ac];
    }
    alpsTab(out,16); alpsOutn(out,'-',44); alpsOutln(out);
    alpsOutsn(out,6,"Totals:         ",itosn(atot,12),itosn(lctot,12),
	      itosn(dtot,10),itosn(lcnt,10),"\n");
    if (n > 1) {
      alpsOutsn(out,3,"Link   Busy tot ",itosn(lbusy,12),"\n");
      alpsTab(out,16); alpsOutn(out,'=',12); alpsOutln(out);
      alpsOutsn(out,3,"Balance         ",itosn(atot -lbusy,12),"\n");
    }
  }
  
  alpsOuts(out,"\nFile allocation Summary\n");
  for (f = finp; f <= fclos; f++)
    alpsOutsn(out,3,fstat_NAMES[(integer)f],itosn(fsa[(integer)f],4),"; ");
  alpsOutln(out);
#if (DEBUG)
  alpsOutsn(out,3,"Write interrupt count ",itos(wintct),"\n");
#endif
#if (BINNDING==DEEPC)
  alpsOutsn(out,3,"Cache Value Cells hit ratio ",itos(cc*100/lc),"%\n");
#endif
  alpsOutsn(out,3,"Number of zombie envblks ",itos(envzomb),"\n");
  alpsFlush(out);
  lex = getVal(a.v.hfm);
  lex.vr->varadd.np[0] = ffree;  /* FWS bytes      */
  lex.vr->varadd.np[1] = flen;   /* FS  cons cells */
  lex.vr->varadd.np[2] = emfree; /* Env slots      */
  return(true);
}  /*printFree*/

static integer alpsWhzat(fibrec * out) {
  dispstate i;
  integer j,n;
  pointer lex;
  number now = mkrtime();
#if (DEBUG)
    alpsOutsn(out,3,"iexctr=",itos(iexctr),"\n");
    n = tcbrind;
    for (j=0;j<BB_TCBDEPTH;j++) {
      alpsOutsn(out,2," ",itos(tcbring[n++]));
      if (n==BB_TCBDEPTH) n=0;
    }
    alpsOutln(out);
#endif
    n = 0;
    if (!isNil(runtcb)) {
      tcbFlush(runtcb.tb);
      alpsOuts(out,
       "State    Tid Pri  EnvDepth  StkDepth EvalDepth RunningTime pc\n");
      alpsOuts(out,"Running ");
      printTcb(out,runtcb); // iexctr tcb not uptodate val is from last disp
#if (DEBUG)
      showTcb(out,runtcb);
#endif
      n++;
      //    showEnvStk(out,lex.tb->envp);
    }
    for (i=done;i<mxdsp;i++) {
      for (lex=disptab[i];!isNil(lex);lex=lex.tb->next) {
	alpsOuts(out,dispstr[i]);
	printTcb(out,lex);
#if (DEBUG)
	showTcb(out,lex);
#endif
	n++;
	//   showEnvStk(out,lex.tb->envp);
      }
      alpsOutln(out);
    }
    alpsOutsn(out,11,
	      "Elapsed: ", rtot(now-boo),
	      " Idle: ",   rtot(uidle*TICKS/1000),
	      " Busy:",    rtot(ubusy*TICKS/1000),
	      " Diff:",    rtot(now-boo-(uidle+ubusy)*TICKS/1000),
	      "\nTasks: ", itos(n),"\n");
    alpsFlush(out);
    return n;
}


/* ***************** Argument Vetting ************ */

static int sgltn(pointer lex) {
  return (dims(lex)==0 || (dims(lex)==1 && nels(lex)==1));
}

static bool ckArg(vtype k, pointer lex) {
  int i=0;  /* landing pad for gdb */
  switch (k) {
  case unk:  return true; // this and remaining params checked in op routine
  case num:  if (!isNum(lex)) return false;    break;
  case cpx:  if (!isCpx(lex)) return false;    break;
  case chr:  if (!isaChr(lex)) return false;   break;
  case ref:  if (!isRef(lex)) return false;    break;
  case env:  if (!isEnv(lex) && !isList(lex))  return false;  break;
  case stk:  if (!isStk(lex)) return false;    break;
  case cnt:  if (!isCnt(lex)) return false;    break;
  case tcb:  if (!isTcb(lex)) return false;    break;
  case fob:  if (!isFob(lex)) return false;    break;
  case cym:  if (!isCym(lex)) return false;    break;
  case sym:  if (!isSymbol(lex)) return false; break;
  case lst:  if (!isList(lex)) return false;   break;
  case sxp:  if (!isList(lex) && !isAtom(lex)) return false;   break;
  case cnm:  if (!isaNum(lex)) return false;   break;
  case nc:   if (!isNCS(lex))  return false;   break;
  case ncr:  if (!isNCRS(lex)) return false;   break;
  case ncra: if (!isNCRS(lex)|| (dims(lex) < 1)) return false; break;
  case sclr: if (!isNum(lex) || (dims(lex)!=0))  return false; break;
  case nsgl: if (!isNum(lex) || (!sgltn(lex)))   return false; break;
  case axn:
    if (!isNil(lex) && !(isNum(lex) && sgltn(lex))) return false; break;
  case nsv: if (!isNum(lex)  || (dims(lex)>1))  return false; break;
  case nsa: if (!isNum(lex)) return false; break; /* Redundant with num */
  case nv:  if (!isNum(lex)  || (dims(lex)!=1)) return false; break;
  case na:  if (!isNum(lex)  || (dims(lex)<1))  return false; break;
  case csgl:if (!isaChr(lex) || (!sgltn(lex)))  return false; break;
  case cv:  if (!isaChr(lex) || (dims(lex)!=1)) return false; break;
  case nscv:if ((!isNum(lex) && !isaChr(lex)) ||
		(isNum(lex) && (dims(lex)!=0)) ||
		(isaChr(lex) && (dims(lex)!=1))) return false; break;
  case pdn: // dyadic primitive numeric 
    if (!isSym(lex) || (!isNumop(lex.at->spl))) return false; break;
  case pdo: // dyadic primitive
    if (!isSym(lex) || (!isAplPrim(lex.at->spl))) return false; break;
  case fun:  if (isConst(lex))   return false; break;
  case cns:  if (!isCons(lex))   return false; break;
  case flk:  if (!isaFil(lex))   return false; break;
  case fil:  if (!isFil(lex))    return false; break;
  case buf:  if (!isBuf(lex))    return false; break;
  case cvf:  if (!isFil(lex) && !isaChr(lex)) return false; break; 
  case ins:  if (!isInst(lex))   return false; break;
  case wgt:  if (!isWidget(lex)) return false; break;
  case any: break;
  default: error(pgmr_inc,"in checkArg");
  }
  return true;
}

 /* TODO: make him return restyp also */
static bool ck2Arg(lispTypes op, pointer lent, pointer rent) {
  int tp;
  vtype k1,k2;
  ASSERT(isAplPrim(op));
  tp = evtab[op].fparmTemplate;
  k1 = ftt[tp].plist[0].typ;
  k2 = ftt[tp].plist[1].typ;
  return (ctypeComp(lent,rent) && ckArg(k1,lent) && ckArg(k2,rent));
 }

static void checkArgs(lispTypes op, pointer p1) {
  pointer lex;
  vtype mtyp;
  enum reqtp mreq;
  int i,j,valid=1;
  int tp = evtab[op].fparmTemplate;;
  struct fparms mpe = ftt[tp];
  int tot  = mpe.fman + mpe.fopt;
  int n    = nargs(p1);
  int nopt = n - mpe.fman;

  if (n < mpe.fman) 
    error(inv_numarg,
	  mkEmes(4,!(mpe.fopt+mpe.frst)?" requires " : " needs at least ",
		 itos(mpe.fman)," argument", (mpe.fman==1)?"":"s"));
  if (!mpe.frst && (n > tot)) 
    error(inv_numarg,
	  mkEmes(4,!mpe.fopt?" requires ":" takes at most ",
		 itos(tot)," argument", (tot==1)?"":"s"));
  for (i=j=0;i<n;) {
    mtyp = mpe.plist[j].typ;
    mreq = mpe.plist[j].req;
    lex  = argn(i,p1);
    switch (mreq) {
    case man: valid = ckArg(mtyp,lex); i++; j++; break;
    case opt:
      if (nopt) { valid = ckArg(mtyp,lex); i++; nopt--; }
      j++;
      break;
    case optn:	 /* optn, if present, must be penultimate parment */
      if (nopt && (i<n-1)) {
	valid = ckArg(mtyp,lex); i++;
	nopt--;
	if (i==n-1) j++;
      } else j++; /* run out of optionals: skip */
      break;
    case rst: /* rst, if present, must be the last fparm */
      valid = ckArg(mtyp,lex); i++; break;
    }
    if (!valid)
      error(inv_funarg,mkEmes(4,"arg ",itos(i),
			      // " has type ",typeDesc[type(lex)],
			      " expected ",typeDesc[mtyp]));
  }
  return;
}

static void ck_ass(pointer lex) {
  if (!isSym(lex) || (lex.at->spl == s_con))
    error(con_ass,"expected var symbol");
}

static void checkFunName(pointer lex) {
  if (!isSym(lex)) error(inv_funame,"expected symbol");
  if (evtab[lex.at->spl].lispEval != evar)
      error(inv_funame,(char *)mkString(lex,eline,evtab[opcd].lispName));
}

static void checkSymList(pointer lex) {
  while (!isNil(lex)) {
    if (!isSym(car(lex))) error(inv_funarg,"expected symbols in argument list");
    lex = cdr(lex);
  }
}
static void checkDef(pointer lex, char *msg) {
  if (!isCons(iexp)) error(inv_funarg,msg);
    checkFunName(car(iexp));
    if (!isCons(cdr(iexp)) || !isList(cadr(iexp)))
      error(inv_funarg,"Expected argument list");
    checkSymList(cadr(iexp));
    if (!isList(cddr(iexp))) error(inv_funarg,"Invalid function body");
}

static integer checkAxis(pointer tex, int lim) {
  integer axis;
  if (!isNum(tex)) error(inv_funarg,"axis not numeric");
  if (nels(tex) != 1) error(inv_funarg,"axis can only have 1 element");
  axis = getInt(tex);
  if (axis < 0) axis = lim+1+axis; /* convenience for last dimension */
  if (axis < 1 || axis > lim) error(dom_range,"axis arg out of bounds");
  return axis-1; // change to zero origin
}

static bool chkList(pointer lex) {
  int i;
  for (i = 0;!isNil(lex); i++) 
    if (isCons(lex)) lex = lex.rg->dr;
    else return false;
  return true;
}

/* ***************** APL SECTION ***************** */

/* global index variables (used to be in exec scope in Pascal) 
   used between execute and vexec,vmove,amove */
static  integer ls, rs, li, ri;

/* APL support functions */

vtype getResType(lispTypes op, pointer lent, pointer rent, vtype restyp,
		 bool crossp, integer *needrcpx) {
    number temp;
    *needrcpx = false;
    if ((op >= s_andd  && op <= s_nord)  ||
	(op >= s_great && op <= s_neapl) || (op==s_dist)) return num;
#if (COMPLEX)
    pointer lex,rex;
    number fc;
    int i, j, ji, atanflag = false;
    restyp =  ((type(lent)==cpx) || (type(rent)==cpx)) ? cpx : num;
    if (op == s_trig) { /* handle special trig cases */
      if (!isNum(lent))  error(inv_funarg,"Trig function code not numeric");
      if (isNum(rent))   {
	for (i=0;i<nels(lent);i++) {
	  fc = vadd(lent).np[i];
	  if (fc > 12 || fc <-12) error(inv_funarg,"Invalid function code");
	  if (fc < -8) { restyp = cpx; break; }
	  if (fc >  8) { *needrcpx = true; }
	  if (fc == -7) atanflag = true;
	}
	if (atanflag) {
	  rex = vadd(rent); /* special handling for atan */
	  for (i=0;i<nels(rent);i++) 
	    if (fabs(rex.np[i])>1.0) {restyp = cpx; break;}
	}
      } else if (isCpx(rent)) { /* check for possible num restyp */
	for (i=0;i<nels(lent);i++) {
	    fc = vadd(lent).np[i];
	    if (fc > 12 || fc < -12) error(inv_funarg,"Invalid function code");
	    if (fc < 9)  break;
	  }
	if (i==nels(lent))  restyp = num; 
      } else error(pgmr_inc,"Unexpected data type in trig");
    } else if ((op==s_power) || (op==s_log)) {
      if (restyp==num) {
	lex = vadd(lent); 
	rex = vadd(rent);
	if (crossp) { /* special needs for outerproduct */
	  for (i=0;i<nels(lent);i++) 
	    for (j=0;j<nels(rent);j++) {
	      if ((op==s_power && ((lex.np[i] < 0) &&
				   (modf(rex.np[j], &temp) != 0))) ||
		  (op==s_log   && ((lex.np[i] < 0) || (rex.np[j] < 0)))) {
		restyp = cpx; 
		break;
	      }
	    }
	} else {
	  j  = 0;
	  ji = (nels(rent) > 1) ? 1 : 0;
	  for (i=0;i<nels(lent);i++) {
	    if ((op==s_power && ((lex.np[i] < 0) &&
				 (modf(rex.np[j],&temp) != 0))) ||
		(op==s_log   && ((lex.np[i] < 0) || (rex.np[j] < 0)))) {
	      restyp = cpx; 
	      break; 
	    }
	    j += ji;
	    if (j >= nels(rent)) break;
	  }
	}
      }
    }
#endif
    return restyp;
}

#if (COMPLEX)
static void cpxAdjust(lispTypes op, pointer *lent, pointer * rent, 
		      pointer * result) {
  pointer lex = *lent, rex = *rent, res;
  integer i;
  
  if ((type(lex)==num) && (type(rex)==cpx)) {
    if (op != s_trig) {
      if (nels(lex)==1) {
	protect(res= allocv(cpx,dims(lex),pvard(lex)));
	res.vr->varadd.cp[0] = vadd(lex).np[0];
	*lent = res;
      } else {
	if (result==NULL) protect(res = allocv(cpx,dims(lex),pvard(lex)));
	else res = *result;
	ASSERT(type(res)==cpx);
	for (i=0;i<nels(lex);i++) res.vr->varadd.cp[i] = vadd(lex).np[i];
	*lent = res;
      }
    } 
  } else if ((type(lex)==cpx || op==s_trig) && (type(rex)==num)) {
    if (nels(rex)==1) {
      protect(res = allocv(cpx,dims(rex),pvard(rex)));
      res.vr->varadd.cp[0] = vadd(rex).np[0];
      *rent = res;
    } else {
      if ((result==NULL) || ((*result).wh == lex.wh)) {
	/* no result yet or result already used for lent */
	res = allocv(cpx,dims(rex),pvard(rex));
	protect(res);
      } else res = *result;
      ASSERT(type(res)==cpx);
      for (i=0;i<nels(rex);i++) res.vr->varadd.cp[i] = vadd(rex).np[i];
      *rent = res;
    }
  } else if ((type(lex)==num) && (type(rex)==num) && (result!=NULL) &&
	     (type(*result)==cpx)) {
    if (nels(lex)==nels(*result)) {
      res = *result;
      for (i=0;i<nels(lex);i++) res.vr->varadd.cp[i] = vadd(lex).np[i];
      *lent = res;
      protect(res = allocv(cpx,dims(rex),pvard(rex)));
      for (i=0;i<nels(rex);i++) res.vr->varadd.cp[i] = vadd(rex).np[i];
      *rent = res;
    } else if (nels(rex)==nels(*result)) {
      res = *result;
      ASSERT(type(res)==cpx);
      for (i=0;i<nels(rex);i++) res.vr->varadd.cp[i] = vadd(rex).np[i];
      *rent = res;
      protect(res = allocv(cpx,dims(lex),pvard(lex)));
      for (i=0;i<nels(lex);i++) res.vr->varadd.cp[i] = vadd(lex).np[i];
      *lent = res;
    }
  }
 } /* make sure unprotall() is called before going back to bigEval */
#endif

static double pwr(double left, double right) {
  if (left == idadd) {
    if (right == idadd) return idprod;   /*a la apl*/
    else                return idadd;   /*we presume right was whole and even*/
  } else return pow(left,right);
}

static double fact(double arg) {
  integer i;
  number tem = idprod;
  for (i = 2; i <= (integer)arg; i++)    tem *= i;
  return tem;
}

static const number lancsoz9[9+2] = 
  {      1.000000000000000174663,
      5716.400188274341379136,
    -14815.30426768413909044,
     14291.49277657478554025,
     -6348.160217641458813289,
      1301.608286058321874105,
      -108.1767053514369634679,
         2.605696505611755827729,
        -0.7423452510201416151527e-2,
         0.5384136432509564062961e-7,
        -0.4023533141268236372067e-8};

#if (COMPLEX)
/* Derived from Paul Godfrey's Matlab version */
static complex clgamma(complex arg) {
  int j;
  complex  ctmp = 0;
  
  for (j=10;j>0;j--)  ctmp += lancsoz9[j] / (arg + (complex) (j - 1));
  ctmp += lancsoz9[0];
  return clog(ctmp*sqrt2pi) + (arg-0.5)*clog(arg+8.5) - arg - 8.5;
}

static complex acexp(complex v) {
  // so that Euler's identity holds nicely
  // libc's cexp  gives -1j1.224646799e-16
  // assume this is because ieee754 representation of sin is not as good as cos
   number mexp = exp(creal(v));
   number  ang = cimag(v);
   number mcos = cos(ang);
   number sign = copysign(idprod,sin(ang));
   return (mexp*mcos + mexp*sign*sqrt(1-mcos*mcos)*I);
   // return mexp*cos(ang) + mexp*sin(ang)*I;
}

static complex cgamma(complex arg) {
  number tmp;
  complex res;
    
  if (creal(arg) <= 0) {
    /* for the non positive real half of the complex plane */
    if ((cimag(arg) == 0) && (creal(arg) == trunc(creal(arg))))
      res = INFINITY; /* handle poles at 0 and negative integers */ 
    else 
      res = -pi/(arg*cexp(clgamma(-arg))*csin(pi*arg));/* reflection formula */
  } else { /* positive half */
    if ((cimag(arg) == 0) && (creal(arg) == (tmp=trunc(creal(arg)))))
      res = fact(tmp); /* exact results for positive integers */
    else
      res = cexp(clgamma(arg));
  }
  return res;
}

static complex cflr(complex arg) {
  return floor(creal(arg)) + I*floor(cimag(arg));
}

static complex cceil(complex arg) {
  return ceil(creal(arg)) + I*ceil(cimag(arg));
}

static complex cdir(complex arg) {
  number ang = atan2(cimag(arg),creal(arg));
  return cos(ang) + I*sin(ang);
}

#endif

static number rlgamma(number arg) {
  int j;
  number  tmp = 0;

  for (j=10;j>0;j--)  tmp += lancsoz9[j] / (arg + ((number) (j - 1)));
  tmp += lancsoz9[0];
  return log(tmp*sqrt2pi) + (arg-0.5)*log(arg+8.5) - arg - 8.5;
}

static number rgamma(number arg) {
  number tmp, res;
  // res = tgamma(arg+idprod);
  if (arg < 0) {
    /* (! (- n) <=> (Gamma(- 1 n)) */
    /* reflection formula mathworld.wolfram.com/GammaFunction.html (42) */
    // (* (Gamma n) (Gamma (- 1 n))) <=> (/ pi (* n (sin (* pi n))))
    tmp = fabs(arg);
    res = pi/(exp(rlgamma(tmp))*sin(pi*tmp));
  } else {
    if  (arg == (tmp=trunc(arg)))  res = fact(tmp);
    else res = exp(rlgamma(arg+idprod));
  }
  return res;
}

inline static double pprodDiv(double s, int val) {
  /* (a #IO 0) (/ (r '* (+ s (i val))) (! val)) */
  register int i;
  double r = idprod;
  for (i=1;i<=val;i++) r *= s++/(double)i;
  return r;
}

inline static double nprodDiv(double s, int val) {
  /* (a #IO 0) (/ (r '* (* (exp -1 (+ 1 (i val))) (+ s (i val)))) (! val)) */  
  register int i;
  double r = idprod;
  for (i=1;i<=val;i++) r *= -s++/(double)i;
  return r;
}

/* binomial coefficients generalised for negative and real arguments */
static double bin(double m, double n) { 
  /*  (de bin (m n) (/ (! n) (* (! m) (! (- n m))))) */
  if (m==n) return idprod; // get this case out of the way now
  if ((trunc(m) != m) || (trunc(n) != n))  /* use gamma fun for non int */
    return rgamma(n)/(rgamma(m) * rgamma(n - m));
  if (m > 0) {
    if (n > m) return pprodDiv(n - m + idprod, m);  // normal case
    if (n >= 0) return idadd; // n and m > 0 and n < m => division by 0!/0
    /* m > 0 and n < 0 => (n-m) < 0 => n! cancels top and bottom, no div  */
    return nprodDiv(fabs(n), m);
  } else if (m < 0) {
    if (n >= 0) return idadd; // m! has 0!/0 term, n! and (n-m)! > 0
    /*  n<m => 0!/0 cancels between n! and m! and (n - m) < 0 => div by 0!/0 */
    if (n < m) return idadd;
    /* m and n <0 and (n-m)>0 => n! cancels top & bottom, no div */
    return  nprodDiv(fabs(n), fabs(n-m));
  } else { // m is zero
    return idprod;
  }
}


static int    krun = 0;
static number krl  = 0.0; /* last known random link hrl */
static int    knext,knextp;
static number kbuf[55];
static const number knigget = 4.0e+7;

static void krinit() { /* Initialise krand */
  number kj,kk,ksporas;
  integer i,j,k;
  ksporas = getNumVal(a.v.hrl);
  if (!krun || (krl != ksporas)) {
    krun = true;
    krl  = ksporas;
    kbuf[54] = kj =  fmod(ksporas, knigget);
    kk=1;
    for (i=0;i<54;i++) {
      j       = (21*(i+1)) % 54;
      kbuf[j] = kk;
      kk      = kj - kk;
      if (kk < 0) kk += knigget;
      kj = kbuf[j];
    }
    for (k=0;k<3;k++)
      for (i=0;i<55;i++) /* warm up */
	if ((kbuf[i] -= kbuf[(i+31) % 55]) < 0) kbuf[i] += knigget;
    knext  = 0;
    knextp = 30;
  }
}

/* Knuth subtractive random number generator */
static double krand() { /* 0<= krand() < 1 */
  number kj;
  if (knext == 55) knext = 0;
  if (knextp == 55) knextp = 0;
  if ((kj = kbuf[knext] - kbuf[knextp]) < 0) kj += knigget;
  kbuf[knext++] = kj;
  knextp++;
  return (kj/knigget);
}

static inline double alps_rand(double arg, double org) {
  unsigned long val = lrint(fabs(arg));
  number rand = krand();
  if (val == 0) return rand;
  rand *= val;
  return ceil(rand)-(org==0); 
}

static bool sameShape(pointer lex, pointer rex) {
  integer i;
  if (dims(lex) != dims(rex)) return false;
  for (i=0;i<dims(lex);i++) if (getDim(lex,i) != getDim(rex,i)) return false;
  return true;
}

static inline number alpsDiv(number x, number y) {
  return (x == y) ? 1.0 : x / y;
}

static number alpsGcd(number l, number r) {
  integer y0,x,y,t;
  x = labs((integer)l);
  y = labs((integer)r);
  while (y > 0) { t = y; y = x % y; x = t;}
  return((number)x);
}

/* initialise index vector lex from rex */
integer *init_aind(pointer *lex, integer ndim, integer noels, integer *rex) {
  pointer tex;
  int i,s=1;
  makeBlock(2*(ndim + 1),lex);
  tex.pt = lex->pt;
  tex.ip[0] = ndim;      /* size of index vector   */
  tex.ip[1] = noels;     /* limit                  */
  for (i=0;i<ndim;i++) {
    tex.ip[i+2]        = rex[i]; /* limit for index i  */
    tex.ip[i+2 + ndim] = 0;      /* index i            */
  }
  return(&tex.ip[2+ndim]);
}

int bump_aind(pointer lex, integer index, integer amount) {
  int ndim = lex.ip[0];
  int carries = 0;
  int i,j,val,oind;
  for (i=index;(i>=0 && amount!=0);i--) {
    oind    = lex.ip[i+2 + ndim];
    val     = oind + amount;
    lex.ip[i+2 + ndim] = val%lex.ip[i+2];
    amount  = val/lex.ip[i+2];
    if (amount) carries++;
  }
  if (amount) return -1; /* out of bounds */
  return carries;
}

void reset_aind(pointer lex) { /* reset index vector to zeros */
  int i,ndim = lex.ip[0];
  for (i=0;i<ndim;i++) {
    lex.ip[i+2 + ndim] = 0;        
  }
}


static void vexec(lispTypes op, vtype atp,
		  pointer lex, pointer rex, pointer mres,
		  integer *ls, integer *rs, integer *as,
		  integer  li, integer  ri, integer  ai,
		  integer noels) {
  pointer res = vadd(mres);
  integer i, ifun;
  number  tmp;
  
  switch (op) {

  case s_dist: 
    if (atp==num) {
      for (i = 0; i < noels; i++) {
	res.np[*as] = sqrt(sqr(lex.np[*ls]) + sqr(rex.np[*rs]));
	*ls += li;     *rs += ri;     *as += ai;
      }
    } else if (atp==cpx) {
      for (i = 0; i < noels; i++) {
	res.np[*as] = sqrt(
			   sqr(creal(lex.cp[*ls]) - creal(rex.cp[*rs]))
			   +
			   sqr(cimag(lex.cp[*ls]) - cimag(rex.cp[*rs])));
	*ls += li;     *rs += ri;     *as += ai;
      }
    }
    break;

  case s_plus:
    if (atp==num) {
      for (i = 0; i < noels; i++) {
	res.np[*as] = lex.np[*ls] + rex.np[*rs];
	*ls += li;     *rs += ri;     *as += ai;
      }
    } else if (atp==cpx) {
      for (i = 0; i < noels; i++) {
	res.cp[*as] = lex.cp[*ls] + rex.cp[*rs];
	*ls += li;     *rs += ri;     *as += ai;
      }
    }
    break;

  case s_minus: 
    if (atp==num) {
      for (i = 0; i < noels; i++) {
       	res.np[*as] = lex.np[*ls] - rex.np[*rs];
	*ls += li;     *rs += ri;     *as += ai;
      } 
    } else if (atp==cpx) {
      for (i = 0; i < noels; i++) {
	res.cp[*as] = lex.cp[*ls] - rex.cp[*rs];
	*ls += li;     *rs += ri;     *as += ai;
      }
    }
    break;

  case s_times:
    if (atp==num) {
      for (i = 0; i < noels; i++) {
	res.np[*as] = lex.np[*ls] * rex.np[*rs];
	*ls += li;     *rs += ri;     *as += ai;
      } 
    } else if (atp==cpx) {
      for (i = 0; i < noels; i++) {
	res.cp[*as] = lex.cp[*ls] * rex.cp[*rs];
	*ls += li;     *rs += ri;     *as += ai;
      }
    }
    break;

  case s_divid: 
    if (atp==num) {
      for (i = 0; i < noels; i++) {
	res.np[*as] = alpsDiv(lex.np[*ls], rex.np[*rs]);
	*ls += li;     *rs += ri;     *as += ai;
      } 
    } else if (atp==cpx) {
      for (i = 0; i < noels; i++) {
	res.cp[*as] = lex.cp[*ls] / rex.cp[*rs];
	*ls += li;     *rs += ri;     *as += ai;
      }
    }
    break;

  case s_power: 
    if (atp==num) {
      for (i = 0; i < noels; i++) {
	res.np[*as] = pwr(lex.np[*ls], rex.np[*rs]);
	*ls += li;      *rs += ri;        *as += ai;
      }
    } else if (atp==cpx) {
      for (i = 0; i < noels; i++) {
	if (rex.cp[*rs]==0.5) {
	  res.cp[*as] = csqrt(lex.cp[*ls]);
	} else	if (rex.cp[*rs]==0.0) {
	  res.cp[*as] = 1.0;
	} else {
	  if ((cimag(lex.cp[*ls]) == 0.0) && (cimag(rex.cp[*rs]) == 0.0) &&
	      modf(creal(rex.cp[*rs]),&tmp) == 0.0)
	    res.cp[*as] = pow(creal(lex.cp[*ls]),creal(rex.cp[*rs]));
	  else
	    res.cp[*as] = cpow(lex.cp[*ls],rex.cp[*rs]);
	}
	*ls += li;     *rs += ri;     *as += ai;
      }
    }
    break;

  case s_resid:
    for (i = 0; i < noels; i++) {
      if (lex.np[*ls] == 0)	res.np[*as] = rex.np[*rs];
      else 	res.np[*as] = fmod(rex.np[*rs],lex.np[*ls]);
      if (res.np[*as] < 0) res.np[*as] += lex.np[*ls]; 
      *ls += li;      *rs += ri;      *as += ai;
    }
    break;

  case s_binom:
    for (i = 0; i < noels; i++) {
      res.np[*as] = bin(lex.np[*ls], rex.np[*rs]);
      *ls += li;     *rs += ri;     *as += ai;
    }
    break;

  case s_floor: 
    if (atp==num) {
      for (i = 0; i < noels; i++) {
	res.np[*as] =  (lex.np[*ls] < rex.np[*rs]) ? lex.np[*ls] : rex.np[*rs];
	*ls += li;      *rs += ri;      *as += ai;
      }
    } else if (atp==cpx) {
      for (i = 0; i < noels; i++) {
	res.cp[*as] = (cabs(lex.cp[*ls]) < cabs(rex.cp[*rs])) ? 
	  lex.cp[*ls] : rex.cp[*rs];
	*ls += li;      *rs += ri;      *as += ai;
      }
    }
    break;

  case s_ceil:
    if (atp==num) {
      for (i = 0; i < noels; i++) {
	res.np[*as] = (lex.np[*ls] > rex.np[*rs]) ? lex.np[*ls] :  rex.np[*rs];
	*ls += li;      *rs += ri;      *as += ai;
      }
    } else if (atp==cpx) {
      for (i = 0; i < noels; i++) {
	res.cp[*as] = (cabs(lex.cp[*ls]) > cabs(rex.cp[*rs])) ? 
	  lex.cp[*ls] : rex.cp[*rs];
	*ls += li;      *rs += ri;      *as += ai;
      }
    }
    break;

  case s_log:
    if (atp==num) {
      for (i = 0; i < noels; i++) {
	if (lex.np[*ls] == 2.0) res.np[*as] = log2(rex.np[*rs]);
	else if (lex.np[*ls] == 10.0) res.np[*as] = log10(rex.np[*rs]);
	else res.np[*as] = log(rex.np[*rs]) / log(lex.np[*ls]);
	*ls += li;      *rs += ri;      *as += ai;
      }
    } else if (atp==cpx) {
      for (i = 0; i < noels; i++) {
	res.cp[*as] =  clog(rex.cp[*rs]) / clog(lex.cp[*ls]);
	*ls += li;     *rs += ri;     *as += ai;
      }
    }
    break;

  case s_trig:
    if (atp==num) {
      number r;
      for (i = 0; i < noels; i++) {
	ifun = (integer)lex.np[*ls];
	r    = rex.np[*rs];
	switch (ifun) {
	case  -8: res.np[*as] = sqrt(-1 - sqr(r));
	  if (r >= 0) res.np[*as] = -res.np[*as];
	  break;
	case -7: res.np[*as] = atanh(r);	         break;
	case -6: res.np[*as] = acosh(r);	         break;
	case -5: res.np[*as] = asinh(r);	         break;
	case -4: res.np[*as] = sqrt(-idprod + sqr(r));   break;
	case -3: res.np[*as] = atan(r);          	 break;
	case -2: res.np[*as] = acos(r);        	         break;
	case -1: res.np[*as] = asin(r);        	         break;
	case 0:	 res.np[*as] = sqrt(idprod - sqr(r));	 break;
	case 1:	 res.np[*as] = sin(r);         	         break;
	case 2:  res.np[*as] = cos(r);         	         break;
	case 3:	 res.np[*as] = tan(r);             	 break;
	case 4:	 res.np[*as] = sqrt(idprod + sqr(r));	 break;
	case 5:	 res.np[*as] = sinh(r);        	         break;
	case 6:	 res.np[*as] = cosh(r);        	         break;
	case 7:	 res.np[*as] = tanh(r);        	         break;
	case 8:  res.np[*as] = sqrt(-idprod - sqr(r));
	  if (r< 0) res.np[*as] = -res.np[*as];
	  break;
	case  9:  res.np[*as] = r; 	  break;
	case 10:  res.np[*as] = fabs(r);  break;
	case 11:  res.np[*as] = idadd;    break;
	case 12:  res.np[*as] = idadd;	  break;
	default:  error(inv_funarg,"trig operation");
	}
	*ls += li;      *rs += ri;      *as += ai;
      }
    } 
#if (COMPLEX)
    else if (atp==cpx) {
      complex r;
      for (i = 0; i < noels; i++) {
	ifun = (integer)lex.np[*ls];
	r    = rex.cp[*rs];
	switch (ifun) {
	case -12: res.cp[*as] = cexp(r * I);              break;
	case -11: res.cp[*as] = r * I;                    break;
	case -10: res.cp[*as] = conj(r);                  break;
	case  -9: res.cp[*as] = r;                        break;
	case  -8: res.cp[*as] = csqrt(-1 - csqr(r));   
	  if ((creal(r)>0  && cimag(r)>0) ||
	      (creal(r)==0 && cimag(r)>1) ||
	      (creal(r)<0  && cimag(r)>=0))
	    res.cp[*as] = -res.cp[*as];
	  break;
	case -7: res.cp[*as] = catanh(r);	           break;
	case -6: res.cp[*as] = cacosh(r);	           break;
	case -5: res.cp[*as] = casinh(r);	           break;
	case -4: res.cp[*as] = csqrt(-idprod - csqr(r));
	  if ((creal(r)<0) && !((creal(r)>1) && (creal(r)<0) && (cimag(r)==0)))
	    res.cp[*as] = -res.cp[*as];
	  break;
	case -3: res.cp[*as] = catan(r);          	   break;
	case -2: res.cp[*as] = cacos(r);        	   break;
	case -1: res.cp[*as] = casin(r);        	   break;
	case  0: res.cp[*as] = csqrt(idprod - csqr(r));    break;
	case  1: res.cp[*as] = csin(r);         	   break;
	case  2: res.cp[*as] = ccos(r);         	   break;
	case  3: res.cp[*as] = ctan(r);             	   break;
	case  4: res.cp[*as] = csqrt(idprod + csqr(r));    break;
	case  5: res.cp[*as] = csinh(r);        	   break;
	case  6: res.cp[*as] = ccosh(r);        	   break;
	case  7: res.cp[*as] = ctanh(r);        	   break;
	case  8: res.cp[*as] = csqrt(-1 - csqr(r));   
	  if (!((creal(r)>0  && cimag(r)>0) ||
		(creal(r)==0 && cimag(r)>1) ||
		(creal(r)<0  && cimag(r)>=0)))  
	    res.cp[*as] = -res.cp[*as];
	  break;
	case  9: 
	  if (isNum(mres)) res.np[*as] = creal(r); 
	  else             res.cp[*as] = creal(r); 
	  break;
	case 10:  
	  if (isNum(mres)) res.np[*as] = cabs(r); 
	  else             res.cp[*as] = cabs(r); 
	  break;
	case 11:
	  if (isNum(mres))  res.np[*as] = cimag(r); 
	  else              res.cp[*as] = cimag(r); 
	  break;
	case 12: 
	  if (isNum(mres))  res.np[*as] = carg(r); 
	  else              res.cp[*as] = carg(r);
	  break;
	default: res.cp[*as] = -idprod;
	}
	*ls += li;      *rs += ri;      *as += ai;
      }
    }
#endif
      break;
    /*trig*/

  case s_lessn:
    {number t,fuzz = getNumVal(a.v.hct);
    for (i = 0; i < noels; i++) {
      switch (atp) {
      case num:	res.np[*as] = 
	  (ctcomp(lex.np[*ls], rex.np[*rs],fuzz) < 0) ? idprod : idadd; 
	break;
      case cpx:	res.np[*as] = 
	  (ctcomp(cabs(lex.cp[*ls]), cabs(rex.cp[*rs]),fuzz) < 0) ? 
	  idprod : idadd; 
	break;
      case sym:
      case cym:
      case chr:	res.np[*as] = (lex.sp[*ls] < rex.sp[*rs])?idprod:idadd;	break;
      case ref: error(not_imp,"Unsupported data type in vexec");
      default:  error(sys_cor,"vexec lt");
      }
      *ls += li;      *rs += ri;      *as += ai;
    }}
    break;

 case s_lessq:
    {number fuzz = getNumVal(a.v.hct);
    for (i = 0; i < noels; i++) {
      switch (atp) {
      case num:	res.np[*as] = 
	  (ctcomp(lex.np[*ls], rex.np[*rs],fuzz) <= 0) ? idprod : idadd; 
	break;
      case cpx:	res.np[*as] = 
	  (ctcomp(cabs(lex.cp[*ls]), cabs(rex.cp[*rs]),fuzz) <= 0) ? 
	  idprod : idadd; 
	break;
      case sym:
      case cym:
      case chr:	res.np[*as] = (lex.sp[*ls] <= rex.sp[*rs])?idprod:idadd; break;
      case ref: error(not_imp,"Unsupported data type in vexec");
      default:  error(sys_cor,"vexec le");
      }
      *ls += li;      *rs += ri;      *as += ai;
    }}
    break;

  case s_eqapl:
  case s_neapl:
    {number et,ef,fuzz = getNumVal(a.v.hct);
    if (op == s_eqapl) { et = idprod; ef = idadd;}
    else               { ef = idprod; et = idadd;}
    for (i = 0; i < noels; i++) {
      switch (atp) {
      case num:
	res.np[*as] = (ctcomp(lex.np[*ls], rex.np[*rs],fuzz) == 0) ? et : ef;
	break;
      case cpx:	res.np[*as] = 
	  (cpequal(lex.cp[*ls], rex.cp[*rs],fuzz) == 1) ? et : ef; 
	break;
      case sym:
      case cym:
      case chr:
	res.np[*as] =  (lex.sp[*ls] == rex.sp[*rs]) ?  et : ef;
	break;
      case ref:
	res.np[*as] = (isEqual(lex.pt[*ls],rex.pt[*rs])) ? et : ef;
	break;
      default: error(sys_cor,"vexec eq");
      }
      *ls += li;      *rs += ri;      *as += ai;
    }}
    break;

  case s_greatq:
    {number fuzz = getNumVal(a.v.hct);
    for (i = 0; i < noels; i++) {
      switch (atp) {
      case num:	res.np[*as] = 
	   (ctcomp(lex.np[*ls], rex.np[*rs],fuzz) >= 0) ? idprod : idadd;
	break;
      case cpx:	res.np[*as] = 
	  (ctcomp(cabs(lex.cp[*ls]), cabs(rex.cp[*rs]),fuzz) >= 0) ? 
	  idprod : idadd; 
	break;
      case sym:
      case cym:
      case chr:	res.np[*as] = (lex.sp[*ls] >= rex.sp[*rs])?idprod:idadd; break;
      case ref: error(not_imp,"Unsupported data type in vexec");
      default:  error(sys_cor,"vexec ge");
      }
      *ls += li;      *rs += ri;      *as += ai;
    }}
    break;

  case s_great:
    {number fuzz = getNumVal(a.v.hct);
    for (i = 0; i < noels; i++) {
      switch (atp) {
      case num:	
	res.np[*as] =
	  (ctcomp(lex.np[*ls], rex.np[*rs],fuzz) > 0) ?  idprod : idadd;	
	break;
      case cpx:	res.np[*as] = 
	  (ctcomp(cabs(lex.cp[*ls]), cabs(rex.cp[*rs]),fuzz) > 0) ? 
	  idprod : idadd; 
	break;
      case sym:
      case cym:
      case chr:	res.np[*as] = (lex.sp[*ls] > rex.sp[*rs])?idprod:idadd;	break;
      case ref: error(not_imp,"Unsupported data type in vexec");
      default:  error(sys_cor,"vexec gt");
      }
      *ls += li;      *rs += ri;      *as += ai;
    }}
    break;

  case s_xor:
    for (i = 0; i < noels; i++) {
      if ((lex.np[*ls] == idprod || rex.np[*rs] == idprod) &&
	  lex.np[*ls] * rex.np[*rs] == idadd)
	res.np[*as] = idprod;
      else	res.np[*as] = idadd;
      *ls += li;      *rs += ri;      *as += ai;
    }
    break;

  case s_vor: /* gcd */
    for (i = 0; i < noels; i++) {
      res.np[*as] = alpsGcd(lex.np[*ls],rex.np[*rs]);
      *ls += li;      *rs += ri;      *as += ai;
    }
    break;

  case s_nord:
    for (i = 0; i < noels; i++) {
      if (lex.np[*ls] == idprod || rex.np[*rs] == idprod) res.np[*as] = idadd;
      else                                                res.np[*as] = idprod;
      *ls += li;      *rs += ri;      *as += ai;
    }
    break;

  case s_andd: /* lcm */
    for (i = 0; i < noels; i++) {
      res.np[*as] = alpsDiv(lex.np[*ls],alpsGcd(lex.np[*ls],rex.np[*rs])) *
	rex.np[*rs];
      *ls += li;      *rs += ri;      *as += ai;
    }
    break;

case s_nandd:
    for (i = 0; i < noels; i++) {
      if (lex.np[*ls] == idprod && rex.np[*rs] == idprod) res.np[*as] = idadd;
      else	                                          res.np[*as] = idprod;
      *ls += li;      *rs += ri;      *as += ai;
    }
    break;
  default: error(pgmr_inc,"vexec");
  }/*case op*/
}  /*vexec*/

static void vmove(pointer left, pointer right, integer lim) {
  integer i;
  pointer lex, rex;

  lex = vadd(left);
  rex = vadd(right);
  switch (type(left)) {
  case num:
    for (i = 0; i < lim; i++) {lex.np[ls] = rex.np[rs]; ls += li; rs += ri; }
    break;
  case cpx:
    for (i = 0; i < lim; i++) {lex.cp[ls] = rex.cp[rs]; ls += li; rs += ri; }
    break;
  case chr:
    for (i = 0; i < lim; i++) {lex.sp[ls] = rex.sp[rs]; ls += li; rs += ri; }
    break;
  case ref:
    for (i = 0; i < lim; i++) {lex.pt[ls] = rex.pt[rs]; ls += li; rs += ri; }
    break;
  default: error(sys_cor,"vmove");
  }
}


static void amove(pointer left, pointer right, integer lim) {
  ls = 0;
  rs = 0;
  li = 1;
  ri = 1;
  vmove(left, right, lim);
}  /*amove*/

/* replicate move */
static integer rmove(pointer lex, pointer rex, vtype typ, integer lim,
		  integer ls, integer rs, integer li) {
  int i;
  switch (typ) {
  case num:
    for (i = 0; i < lim; i++) {lex.np[ls] = rex.np[rs]; ls += li; }
    break;
  case cpx:
    for (i = 0; i < lim; i++) {lex.cp[ls] = rex.cp[rs]; ls += li; }
    break;
  case chr:
    for (i = 0; i < lim; i++) {lex.sp[ls] = rex.sp[rs]; ls += li; }
    break;
  case ref:
    for (i = 0; i < lim; i++) {lex.pt[ls] = rex.pt[rs]; ls += li; }
    break;
  default: error(sys_cor,"rmove");
  }
  return ls;
}

static bool hasNeg(pointer lex) {
  int i;
  int lim=nels(lex);
  lex = vadd(lex);
  for (i=0;i<lim;i++) if (lex.np[i]<0) return true;
  return false;
}

/*now for the heavy stuff*/

static pointer execute(lispTypes op, integer nargs, pointer parms)    {
  pointer gong, lex, rex, res, lent, rent, result, lp, rp, dex;
  integer i, noels, dimd, as, ai, rep, fc, needrcpx, xarg; /* for nargs > 2 */
  number  tmp, tol;
  bool    dimseq, rankeq, conformable;
  vtype   restyp;
  result = pnil;

  if (!isAplPrim(op)) error(not_imp,"apl primitive");
  if (nargs == 0) { /* niladic */
    i = op - s_dist;
    ASSERT((i >= 0) && (i < prim_dyadics));
    return(idelem[i]); /* return corresponding identity element */
  }
  if (nargs == 1) {  /* monadic */
    if (!isaNum(rent=arg1(parms))) error(inv_funarg,"expected numeric arg");
    noels  = nels(rent);
    if ((op == s_resid) || ((op >= s_great) && (op <= s_neapl))) restyp = num;
    else restyp = type(rent);
    if ((restyp==num) && (op==s_log) && hasNeg(rent)) {
      restyp = cpx;
      result = allocv(restyp, dims(rent), pvard(rent));
      for (i=0;i<noels;i++) result.vr->varadd.cp[i] = vadd(rent).np[i];
      rent = result;
    } else {
      result = allocv(restyp, dims(rent), pvard(rent));
    }
    lex    = vadd(result);
    rex    = vadd(rent);
    tol    = getNumVal(a.v.hct);
#define apl_comp(COMP) (rex.np[i] COMP idadd) ? idprod : idadd
#define apl_ccomp(COMP) (cabs(rex.cp[i]) COMP idadd) ? idprod : idadd
#define apl_signum(T)  (fabs(rex.np[i]) < T ? idadd: copysign(idprod,rex.np[i]))
    if (type(rent)==num) {
    switch (op) { /* can't rely on compiler to generate separate loops */
    case s_andd: case s_xor: case s_vor: case s_nandd: case s_nord:
    case s_plus:  for (i=0; i < noels; i++) lex.np[i] =  rex.np[i];       break;
    case s_minus: for (i=0; i < noels; i++) lex.np[i] = -rex.np[i];       break;
    case s_times: for (i=0; i < noels; i++) lex.np[i] = apl_signum(tol);  break;
    case s_divid: for (i=0; i < noels; i++) lex.np[i] = 1.0 / rex.np[i];  break;
    case s_power: for (i=0; i < noels; i++) lex.np[i] = exp(rex.np[i]);   break;
    case s_resid: for (i=0; i < noels; i++) lex.np[i] = fabs(rex.np[i]);  break;
    case s_binom: for (i=0; i < noels; i++) lex.np[i] = rgamma(rex.np[i]);break;
    case s_floor: for (i=0; i < noels; i++) lex.np[i] = floor(rex.np[i]); break;
    case s_ceil:  for (i=0; i < noels; i++) lex.np[i] = ceil(rex.np[i]);  break;
    case s_trig:  for (i=0; i < noels; i++) lex.np[i] = pi * rex.np[i];   break;
    case s_log:   for (i=0; i < noels; i++) lex.np[i] = log(rex.np[i]);   break;
    case s_lessn: for (i=0; i < noels; i++) lex.np[i] = apl_comp(<);      break;
    case s_lessq: for (i=0; i < noels; i++) lex.np[i] = apl_comp(<=);     break;
    case s_eqapl: for (i=0; i < noels; i++) lex.np[i] = apl_comp(==);     break;
    case s_neapl: for (i=0; i < noels; i++) lex.np[i] = apl_comp(!=);     break;
    case s_greatq:for (i=0; i < noels; i++) lex.np[i] = apl_comp(>=);     break;
    case s_great: for (i=0; i < noels; i++) lex.np[i] = apl_comp(>);      break;
    default: error(inv_fun,"exec monadic numops");                        break;
    } // monadic real
    return result; // protected by sendres
    } 
#if (COMPLEX)
    else if (type(rent)==cpx) {
    switch (op) { 
    case s_plus:  for (i=0; i < noels; i++) lex.cp[i] = conj(rex.cp[i]);  break;
    case s_minus: for (i=0; i < noels; i++) lex.cp[i] = -rex.cp[i];       break;
    case s_times: for (i=0; i < noels; i++) lex.cp[i] = cdir(rex.cp[i]);  break;
    case s_divid: for (i=0; i < noels; i++) lex.cp[i] = 1.0 / rex.cp[i];  break;
    case s_power: for (i=0; i < noels; i++) lex.cp[i] = acexp(rex.cp[i]); break;
    case s_resid: for (i=0; i < noels; i++) lex.np[i] = cabs(rex.cp[i]);  break;
    case s_binom: for (i=0; i < noels; i++) lex.cp[i] = cgamma(rex.cp[i]);break;
    case s_floor: for (i=0; i < noels; i++) lex.cp[i] = cflr(rex.cp[i]);  break;
    case s_ceil:  for (i=0; i < noels; i++) lex.cp[i] = cceil(rex.cp[i]); break;
    case s_trig:  for (i=0; i < noels; i++) lex.cp[i] = pi * rex.cp[i];   break;
    case s_log:   for (i=0; i < noels; i++) lex.cp[i] = clog(rex.cp[i]);  break;
    case s_lessn: for (i=0; i < noels; i++) lex.np[i] = apl_ccomp(<);     break;
    case s_lessq: for (i=0; i < noels; i++) lex.np[i] = apl_ccomp(<=);    break;
    case s_eqapl: for (i=0; i < noels; i++) lex.np[i] = apl_ccomp(==);    break;
    case s_neapl: for (i=0; i < noels; i++) lex.np[i] = apl_ccomp(!=);    break;
    case s_greatq:for (i=0; i < noels; i++) lex.np[i] = apl_ccomp(>=);    break;
    case s_great: for (i=0; i < noels; i++) lex.np[i] = apl_ccomp(>);     break;
    default: error(inv_fun,"exec monadic numops");                        break;
    } // monadic complex
    return result;
    }
#endif
    else error(not_imp,"monadic"); 
#undef apl_comp
#undef apl_ccomp
#undef apl_signum
  } /* nargs == 1 */

  if ((nargs > 2) && (op > s_nord)) error(inv_funarg,"too many arguments");
  xarg = 1;
  lent = arg1(parms);
  do {
    rent = argn(xarg,parms);
    xarg++;
    ls = rs = as = 0;
    li = ri = ai = 1;
    i  = 0;
    if (dims(lent) == dims(rent)) {
      rankeq = true;
      dimd = dims(rent);
      while (i < dimd && getDim(lent,i) == getDim(rent,i)) i++;
      dimseq = (i==dimd);
    } else {
      rankeq = false;
      dimseq = false;
    }
    if (rankeq) {
      if (dimseq) {
	gong = lent;
	conformable = true;
      } else {  /* rankeq ^ ~ dimseq*/
	if (i == (dimd-1) && (getDim(rent,i) == 1 || nels(lent)==0)) {
	  gong = lent;
	  ai   = li = getDim(lent,i);
	  conformable = true;
	} else {
	  if (i == (dimd-1) && (getDim(lent,i) == 1 || nels(rent)==0)) {
	    gong = rent;
	    ai   = ri = getDim(rent,i);
	    conformable = true;
	  } else {
	    gong = lent;
	    conformable = false;
	  }
	}
      }
    } else {  /* ~rankeq*/
      gong = dims(lent) > dims(rent) ? lent : rent;
      if (nels(rent) == 1) {
	ri = 0;
	conformable = true;
      } else {
	if (nels(lent) == 1) {
	  li = 0;
	  conformable = true;
	} else conformable = false;
      }
    }
    if (isNumop(op) && !isaNum(gong)) error(inv_funarg,"exec");
    if (!conformable)  error(dom_range,"exec"); /*not conformable*/
    restyp = getResType(op,lent,rent,false,type(gong),&needrcpx);
    protect(lent);
    if (isNil(result) || type(result) != restyp|| !(rankeq && dimseq)) {
      result = allocv(restyp, dims(gong), pvard(gong));
    }
#if (COMPLEX)
    if (restyp==cpx) {
      protect(result);
      cpxAdjust(op,&lent,&rent,&result);
    } else {
      ASSERT(restyp==num);
      if  (isaNum(lent) && ((type(lent) != type(rent)) || needrcpx)) {
	protect(result);
	cpxAdjust(op,&lent,&rent,NULL);
      }
    }
#endif
    unprotall();
    if (!ctypeComp(rent,lent) && (op == s_eqapl || op == s_neapl)) {
      res = vadd(result);
      if (op == s_eqapl) for (i=0;i<nels(result);i++) res.np[i] = idadd;
      else               for (i=0;i<nels(result);i++) res.np[i] = idprod;
    } else {
      noels = nels(gong);
      lp    = vadd(lent);
      rp    = vadd(rent);
      rep   = (ai > 0) ? (noels / ai) : 1;
      if (ai == ri) for (i = 0; i < ai; i++) {
	  as = i;	    rs = as;	    ls = 0;
	  vexec(op,type(rent),lp, rp, result, &ls, &rs, &as, li, ri, ai, rep);
	}  else for (i = 0; i < ai; i++) {
	  as = i;	  ls = as;	  rs = 0;
	  vexec(op,type(rent),lp, rp, result, &ls, &rs, &as, li, ri, ai, rep);
	}
    }
    if ((xarg > 2)  && (!isEqq(lent,result))) {
      /*	alpsOuts(alpserr,"exec: could drop lent\n"); */
    }
    lent = result;
  } while (xarg < nargs);
  return(result);  // protected by sendres
}  /*end exec*/

static pointer alps_not(integer nargs, pointer parms) {
  pointer res,lex,rex;
  integer noels,i;
  lex   = arg1(parms);
  noels = nels(lex);
  res   = allocv(num, dims(lex), pvard(lex));
  rex   = vadd(res);
  lex   = vadd(lex);
  for (i=0; i < noels; i++) rex.np[i] = (lex.np[i]==idadd) ? idprod : idadd;
  return(res); // protected by sendres
}

static pointer roll(integer nargs, pointer parms) {
  pointer res,lex,rex,mex;
  integer noels,i,j,n,org,lim,val;
  number dlim;
  org = getNumVal(a.v.hio);
  krinit();
  if (nargs == 1) {
    lex = arg1(parms);
    if (!isNum(lex)) error(inv_funarg,"roll");
    else {
      noels = nels(lex);
      res   = allocv(num, dims(lex), pvard(lex));
      rex   = vadd(res);
      lex   = vadd(lex);
      for (i = 0; i < noels; i++) rex.np[i] = alps_rand(lex.np[i],org);
    }
  } else {
    lex=arg1(parms);
    rex=arg2(parms);
    if ((nels(lex)!=1) || (nels(rex)!=1)) error(inv_funarg,"non scalar arg");
    noels = getWhole(lex);
    lim   = dlim = getWhole(rex);
    if (noels > lim) error(dom_range,"deal arg1 <= arg2");
    r.s.lds.ip[0] = noels;
    res = allocv(num, 1, r.s.lds);
    mex = vadd(res);
#ifdef QND
    {
      integer tval[lim+1];/* fast: produces normal distribution (not good) */
      for (i=0;i<=lim;i++) tval[i] = i;
      for (i=org;i<noels+org;i++) {
	if ((j=alps_rand(lim,org)) != i) {
	  n = tval[j]; tval[j] = tval[i]; tval[i] = n;
	}
      }
      for (i=0;i<noels;i++) mex.np[i] = tval[i+org];
    }
#else
    BIT_MAP(seen,(lim+1)); /* for both cases of #IO */
    BMZERO(seen,(lim+1));
    for (i=0;i<noels;) {
      val=(int)alps_rand(lim,org);
      if (BMCLRP(seen,val)) { BMSET(seen,val); mex.np[i++] = val; }
    }
#endif
  }
  return(res); // protected by sendres
}

static pointer shape(integer nargs, pointer parms) {
  pointer res,rex,rent,lent;
  integer i,j,lnd,noels,resels;
  if (nargs == 0) res = r.s.nonum; /* numeric by default */
  else if (nargs == 1) { /* only one arg */
    rent = arg1(parms);
    if (!isNCRS(rent)) error(inv_funarg, "shape wants atom");
    if (dims(rent) == 0) res = r.s.nonum; // null vector
    else {
      r.s.lds.ip[0] = dims(rent);
      res = allocv(num, 1, r.s.lds);
      for (i = 0; i < dims(rent); i++) res.vr->varadd.np[i] = getDim(rent,i);
    }
  } else {  /*reshape*/
    lent = arg1(parms);
    rent = arg2(parms);
    if (!isNum(lent) || dims(lent) > 1)
	error(inv_funarg,"(p <numeric vector> <any>)");
    lnd = nels(lent);
    if ((lnd == 0) && !isaNum(rent))
      error(inv_funarg,"cannot make dimensionless chr or ref");
    makeBlock(lnd, &r.s.lds);
    for (i = 0; i < lnd; i++) r.s.lds.ip[i] = (integer)vadd(lent).np[i];
    res    = allocv(vrtype(rent), lnd, r.s.lds);
    resels = nels(res);
    if (resels==0) return res;
    ls     = 0;
    li     = 1;
    ri     = 1;
    noels = nels(rent);
    if (noels==0) { /* extension of null vectors as for take !APL compatible */
      rent  = filltab[vrtype(rent)];
      noels = 1;
      ri    = 0;
    } else {
      for (i = 0; i < resels / noels; i++) {rs = 0; vmove(res, rent, noels); }
      resels %= noels;
    }
    rs = 0;
    vmove(res, rent, resels);   /*move the rest*/
  } /* reshape */
  return res; // protected by sendres
}

static pointer catenate(integer n, pointer parms) {
  pointer lent,rent,bent,gong,chac,res;
  number  dax;
  integer axis,lax,rax,gax,lsize,rsize,i,j,k,lnd,numoves;
  integer linc,rinc,ldim,rdim,gspa,gssa,gels,gdim;
  bool lam,dimseq;
  lent = argn(n-2,parms);
  rent = argn(n-1,parms);
  if (vrtype(rent) != vrtype(lent)) {
    if (isaNum(rent) && isaNum(lent)) {
      if (isCpx(rent) && isNum(lent)) lent = mkCpxFromNum(lent);
      else if (isCpx(lent) && isNum(rent)) rent = mkCpxFromNum(rent);
      else error(pgmr_inc,"cat");
    } else {
    error(inv_funarg,"arguments not type compatible");
    }
  }
  ldim = dims(lent); 
  rdim = dims(rent);
  /* gong = argument with the highest dimension, the one axis refers to */
  if (ldim > rdim) { gong = lent;  chac = rent; }
  else             { gong = rent;  chac = lent; }
  gdim = lnd  = dims(gong);
  gels = nels(gong);
  if (n == 3) {
    dax  = getNum(arg1(parms));
    if ((lam = (dax != trunc(dax)))) { axis = (integer)trunc(dax);lnd++;}
    else axis = (integer) dax - 1;
    if (axis < 0 || axis >= lnd) error(inv_funarg,"cat axis");
  } else {
    axis = lnd-1;
    lam  = false;
  }
  dimseq = true;
  gspa  = 1;  /* product of dimensions preceding axis */
  gssa  = 1;  /* product of dimensions succeeding axis */
  gax   = 0;  /* num els along axis */
  if (gdim) { /* dims not both zero */
    if (ldim && rdim) {  /* both dims not zero */
      if ((lam && (ldim != rdim) && !sgltn(chac)) ||
	  (!lam && (labs(ldim - rdim) > 1) && !sgltn(chac)))
	error(dom_range,"rank");
      else {
	for (j=k=0;k<gdim && dimseq;k++) 
	  if (lam || (k != axis))
	    dimseq=(getDim(gong,k) == getDim(chac,j++));
	  else if (ldim==rdim) j++;
      }
    } 
    if (!dimseq) {
      if (sgltn(lent)) ldim=0;
      else if (sgltn(rent)) rdim=0;
      else error(dom_range,"catenate dimensions");
    }
    for (i=gdim-1;i>axis;i--) gspa *= getDim(gong,i); 
    gax = getDim(gong,axis); /* num els along axis */
    for (i=0;i<axis;i++) gssa *= getDim(gong,i);
  }
  linc  = ldim ? 1 : 0;
  rinc  = rdim ? 1 : 0;
  makeBlock(lnd, &r.s.lds); 
  if (lam) {
    for (i=k=0;k<lnd;k++) r.s.lds.ip[k] = (k==axis) ? 2 : getDim(gong,i++);
    rsize = lsize = (gax==0) ? gspa : gspa*gax;
  } else {
    if (lnd==0) { /* two scalars */
      lnd  = 1; axis = 0;
    } else{
      for (k=0;k<lnd;k++) r.s.lds.ip[k] = getDim(gong,k);
    }
    lsize = rsize = 1;
    if (!ldim || (ldim!=gdim)) {
      lax  = 1; lsize = gspa;
    } else {
      lax  = getDim(lent,axis);
      for (j=lnd-1;j>=axis;j--) lsize *= getDim(lent,j);
    }
    if (!rdim || (rdim !=gdim)) {
      rax  = 1; rsize = gspa;
    } else {
      rax  = getDim(rent,axis);
      for (j=lnd-1;j>=axis;j--) rsize *= getDim(rent,j);
    }
    r.s.lds.ip[axis] = lax+rax;
  }
  /* special case for chr singleton */
  if ((vrtype(lent) == chr) && (lnd == 1) && (r.s.lds.ip[0] == 1)) {
    return (nels(lent)==1) ? lent : rent;
  }
  res = allocv(vrtype(lent), lnd, r.s.lds);
  if (nels(res) > 0) {
    ls = 0;
    li = 1;
    numoves = gssa;
    for (j = 0; j < numoves; j++) {
      rs = lsize * j * linc;
      ri = linc;
      vmove(res, lent, lsize);
      rs = rsize * j * rinc;
      ri = rinc;
      vmove(res, rent, rsize);
    }
  }
  return res;
}

static pointer ravel(integer nargs, pointer parms) {
  pointer rent,res;

  rent = arg1(parms);
  r.s.lds.ip[0] = nels(rent);
  if ((nels(rent)==1) && (vrtype(rent) == chr)) { /* chr singleton */
    return sstrtab[rent.vr->varadd.sp[0]];
  }
  res = allocv(vrtype(rent), 1, r.s.lds);
  amove(res, rent,nels(rent));
  return res; // protected by sendres
}

static pointer grade(lispTypes pc, integer nargs, pointer parms) {
  pointer lex,res;
  integer n,org;
  pointer x;
  number *rr;

  lex = arg1(parms);
  n   = nels(lex);
  if (n == 0) return(r.s.nonum);
  if (n == 1) return (getVal(a.v.hio));
  x   = vadd(lex);
  res = allocv(num,dims(lex),pvard(lex)); /* same as lex */
  rr  = vadd(res).np; 
  org = getNumVal(a.v.hio);
  { /* heapsort adapted from Programming Pearls: Jon Bentley, AT&T Bell Labs */
    integer ind[n+1];
    register integer i,t;
    integer nm=n-1,nm2=(n-1)/2;

    void sdcu(int i, int k) { /* up */
      int c,t;
      while ((c = 2*i) < k) {
	if  (((c+1) < n) &&
	     ((x.sp[ind[c+1]] >  x.sp[ind[c]]) || 
	      ((x.sp[ind[c+1]] == x.sp[ind[c]]) && (ind[c+1] > ind[c])))) c++;
	if ((x.sp[ind[i]]   > x.sp[ind[c]])|| 
	    ((x.sp[ind[i]] == x.sp[ind[c]]) && (ind[i] >= ind[c]))) break;
	t = ind[c]; ind[c] = ind[i]; ind[i] = t;
	i = c;
      }
    }

    void sdcd(int i, int k) { /* down */
      int c,t;
      while ((c = 2*i) < k) {
	if  (((c+1) < n) &&
	     ((x.sp[ind[c+1]] <  x.sp[ind[c]]) || 
	      ((x.sp[ind[c+1]] == x.sp[ind[c]]) && (ind[c+1] > ind[c])))) c++;
	if ((x.sp[ind[i]]  < x.sp[ind[c]])|| 
	    ((x.sp[ind[i]] == x.sp[ind[c]]) && (ind[i] >= ind[c]))) break;
	t = ind[c]; ind[c] = ind[i]; ind[i] = t;
	i = c;
      }
    }
    
    void sdnu(int i, int k) { /* up */
      int c,t;
      while ((c = 2*i) < k) {
	if (((c+1) < n) &&
	    ((x.np[ind[c+1]] >  x.np[ind[c]])|| 
	     ((x.np[ind[c+1]] == x.np[ind[c]]) && (ind[c+1] > ind[c])))) c++;
	if ((x.np[ind[i]]   > x.np[ind[c]]) || 
	    ((x.np[ind[i]] == x.np[ind[c]]) && (ind[i] >= ind[c]))) break;
	t = ind[c]; ind[c] = ind[i]; ind[i] = t;
	i = c;
      }
    }

    void sdnd(int i, int k) { /* down */
      int c,t;
      while ((c = 2*i) < k) {
	if (((c+1) < n) &&
	    ((x.np[ind[c+1]] <  x.np[ind[c]])|| 
	     ((x.np[ind[c+1]] == x.np[ind[c]]) && (ind[c+1] > ind[c])))) c++;
	if ((x.np[ind[i]]   < x.np[ind[c]]) || 
	    ((x.np[ind[i]] == x.np[ind[c]]) && (ind[i] >= ind[c]))) break;
	t = ind[c]; ind[c] = ind[i]; ind[i] = t;
	i = c;
      }
    }
    
    /* BEGIN Heapsort */
    for (i=0;i<n;i++) ind[i] = i;
    if (pc==s_gup) { /* going up */
      if (isaChr(lex)) {
	for (i=nm2;i>=0;i--) sdcu(i,n);
	for (i=nm;i>0;i--) {
	  t = ind[0]; ind[0] = ind[i]; ind[i] = t; /* swap(x[0],x[i]) */
	  sdcu(0,i-1);
	}
      } else if (isNum(lex)) {
	for (i=nm2;i>=0;i--) sdnu(i,n);
	for (i=nm;i>0;i--) {
	  t = ind[0]; ind[0] = ind[i]; ind[i] = t; /* swap(x[0],x[i]) */
	  sdnu(0,i-1);
	}   
      } else error(inv_funarg,"gup needs chr or num");
    } else { /* going down */
      if (isaChr(lex)) {
	for (i=nm2;i>=0;i--) sdcd(i,n);
	for (i=nm;i>0;i--) {
	  t = ind[0]; ind[0] = ind[i]; ind[i] = t; /* swap(x[0],x[i]) */
	  sdcd(0,i-1);
	}
      } else if (isNum(lex)) {
	for (i=nm2;i>=0;i--) sdnd(i,n);
	for (i=nm;i>0;i--) {
	  t = ind[0]; ind[0] = ind[i]; ind[i] = t; /* swap(x[0],x[i]) */
	  sdnd(0,i-1);
	}   
      } else error(inv_funarg,"gdn needs chr or num");
    }
    for (i=0 ;i<n; i++) rr[i] = ind[i   ]+org;  /* going up */
  }
    return(res); // protected by sendres
}

static pointer kompress(integer nargs, pointer parms) {
      /* kompress [along (AXIS)] by (VECT) (ARR1)*/
      /*                  arg1        lent  rent */
  pointer lent,rent,lex,rex,chac,result,res;
  integer i,j,k,l,m,rsize,lsize,destl,srcl,axis,noels,lnd,ldim,rep,lim,rrc,linc;
  vtype typ;
  
  lent  = argn(nargs-2,parms);
  lex   = vadd(lent);
  lim   = nels(lent);
  rent  = argn(nargs-1,parms);
  rex   = vadd(rent);
  lnd   = dims(rent);
  noels = nels(rent);
  typ   = vrtype(rent);
  if (lnd==0 && nargs==3) error(inv_numarg,"axis not allowed with scalar arg");
  if (lnd==0 || noels==1) { /* extend singleton rarg */
    axis = ri  = 0;
    rep  = lnd = li = 1;
  } else {
    axis = (nargs == 2) ? lnd-1 : checkAxis(arg1(parms),lnd);
    rep  = rent.vr->vard[axis];
    ri = 1;
    for (i = 0; i < lnd; i++) {
      r.s.lds.ip[i] = getDim(rent,i);
      if (i > axis) ri *= getDim(rent,i);
    }
    li = ri;
  }
  linc = 1; /* increment along selector vector */
  if (lim==1) { lim = rep; linc=0; }   /* extend singleton selector */
  for (ldim=l=j=i=0; i<lim; i++,j+=linc) {
    ldim += labs((integer)lex.np[j]);    /* dimension along result axis */
    if (lex.np[j] >= 0) l++;    /* number of non-negative selector elements */
  }
  if ((ri != 0) && (rep != l))
    error(dom_range,
	  "Expected (eql (p (k (<= 0 sel) sel)) (aref (p arg) axis)) => t");
  r.s.lds.ip[axis] = ldim;
  result = allocv(typ, lnd, r.s.lds);
  res    = vadd(result);
  rsize  = li * rep;
  lsize  = li * ldim;
  destl  = srcl = 0;
  chac   = filltab[typ].vr->varadd;
  for (j = 0; j < noels; j+=rsize) {
    for (k = 0; k < li; k++) {
      rs = srcl + k;
      ls = destl + k;
      for (i=m=0; i<lim;i++,m+=linc) { /* step across selector elements */
	rrc = labs((integer)lex.np[m]);
	if (lex.np[m] > 0) {                    /* replicate */
	  ls = rmove(res,rex,typ,rrc,ls,rs,li);
	  rs += ri;
	} else if (lex.np[m] < 0) {             /* fill */
	  ls = rmove(res,chac,typ,rrc,ls, 0,li);
	} else rs += ri;                     /* drop */
      }
    }
    destl += lsize;
    srcl  += rsize;
  }
  return(result); // protected by sendres
}

static pointer expand(integer nargs, pointer parms) {
      /* expand  [along (AXIS)]  by (VECT) (ARR1) */
      /*                arg1           lent  rent   */
  pointer lent,rent,result,gong,res,chac;
  integer i,j,k,lim,axis,lnd,ldim,rep,rsize,lsize,destl,srcl,as;
  lent = argn(nargs-2,parms);
  rent = argn(nargs-1,parms);
  lnd  = dims(rent);
  ldim = nels(lent); 
  axis = (nargs == 2) ? lnd-1 : checkAxis(arg1(parms),lnd);
  rep  = rent.vr->vard[axis];
  for (lim=i=0; i<ldim; i++)  if (vadd(lent).np[i] != idadd) lim++;
  if (rep != lim) error(dom_range,"expand");
  else {
    li = 1;
    for (i = 0; i < lnd; i++) {
      r.s.lds.ip[i] = getDim(rent,i);
      if (i > axis) li *= getDim(rent,i);
    }
    r.s.lds.ip[axis] = ldim;
    result = allocv(vrtype(rent), lnd, r.s.lds);
    rsize  = li * rep;
    lsize  = li * ldim;
    gong   = vadd(rent);
    res    = vadd(result);
    chac   = vadd(lent);
    destl  = 0;
    srcl   = 0;
    for (j = 0; j < nels(result); j+=lsize) {
      for (k = 0; k < li; k++) {
	ls = srcl + k;
	as = destl + k;
	switch (vrtype(rent)) {
	case num:
	  for (i = 0; i < ldim; i++) {
	    if (chac.np[i] != idadd) {
	      res.np[as] = gong.np[ls];
	      ls += li;
	    } else     res.np[as] = idadd;
	    as += li;
	  }
	  break;
	case  chr:
	  for (i = 0; i < ldim; i++) {
	    if (chac.np[i] != idadd) {
	      res.sp[as] = gong.sp[ls];
	      ls += li;
	    } else     res.sp[as] = space;
	    as += li;
	  }
	  break;
	case  ref:
	  for (i = 0; i < ldim; i++) {
	    if (chac.np[i] != idadd) {
	      res.pt[as] = gong.pt[ls];
	      ls += li;
	    } else     res.pt[as] = pnil;
	    as += li;
	  }
	  break;
	default: error(pgmr_inc," argtype");
	} // case vrtype(rent)
      }
      destl += lsize;
      srcl  += rsize;
    }
  }
  return(result); // protected by sendres
}

static pointer take(int nargs, pointer parms) {
  integer i,j,k,flag,lnd,rels,noels,*abp,*rbp;
  pointer result,lex,rex,tex,res;
  vtype typ;

  lex = arg1(parms); /* selector */
  rex = arg2(parms); /* arg */
  lnd = dims(rex);
  abp = rex.vr->vard;
  typ =vrtype(rex);
  rbp = r.s.lds.ip;     /* short hand */
  if (lnd != nels(lex) && lnd != 0)
    error(dom_range,"(p (, selector)) must equal (rank arg)");
  if (!lnd) {lnd = 1; abp = r.s.ldx.ip; abp[0] =1;}
  makeBlock(lnd, &r.s.ldx);
  {integer rst[lnd],bas[lnd],ind[lnd],sel[lnd],lim[lnd],pdl[lnd];
    rels=0;
  for (i=0;i<lnd;i++) {
    k      = sel[i] = vadd(lex).np[i];
    rels  += labs(k);
    rbp[i] = labs(k);
    rst[i] = bas[i] = 0;
    lim[i] = pdl[i] = abp[i];
    if (k < 0)  /* select from end of dim */
      if (abp[i] + k < 0) rst[i] = abp[i] + k; /* negative pad */
      else                bas[i] = abp[i] + k;
    else  /*select from beginning of dim */
      if  (k > abp[i]) 	pdl[i] = k; /* positive pad */
      else              lim[i] = k;
    ind[i] = rst[i]; /* starting index */
  }
  if ((typ == chr) && (lnd == 1) && (rels == 1))  { /* singleton char case */
    return sstrtab[(sel[0]==1) ? rex.vr->varadd.sp[0] :
		   rex.vr->varadd.sp[nels(rex)-1]]; 
  }
  result = allocv(typ, lnd, r.s.lds);
  noels  = nels(result);
  if (noels==0) return result;
  tex = vadd(rex);
  res = vadd(result);
  i=k=0;
  while (true) {
    j = flag = 0;
    while (!flag && j < lnd) { /* lowest dimension has priority */
      if (ind[j] < 0)  flag =  1;              /* pre pad   */
      else if (ind[j] < bas[j])  flag = -1;    /* pre skip  */
      else if (ind[j] < lim[j])  flag =  0;    /* copy      */
      else if (ind[j] < abp[j])  flag = -1;    /* post skip */
      else if (ind[j] < pdl[j])  flag =  1;    /* post pad  */
      j++;
    }
    if (flag < 0) k++; /*skip*/
    else
      switch (typ) {
	case num: if (flag > 0) res.np[i++] = idadd; /* pad */
	          else          res.np[i++] = tex.np[k++];
                  break;
	case cpx: if (flag > 0) res.cp[i++] = idadd; /* pad */
	          else          res.cp[i++] = tex.cp[k++];
                  break;
        case chr: if (flag > 0) res.sp[i++] = space; /* pad */
	          else          res.sp[i++] = tex.sp[k++];
                  break;
        case ref: if (flag > 0) res.pt[i++] = pnil; /* pad */
	          else          res.pt[i++] = tex.pt[k++];
                  break;
      default: error(sys_cor,"take");
      }
    j = lnd - 1;
    ind[j]++;
    while (ind[j] >= pdl[j]) {
      ind[j] = rst[j];
      j--;
      if (j < 0) return(result);
      ind[j]++;
    }
  }}
  return(result);  // protected by sendres
}

static pointer drop(integer nargs, pointer parms) {
  integer i,j,k,flag,lnd,lns,*abp,*rbp;
  pointer result,lex,rex,tex,res;
  vtype thisType;

  lex = arg1(parms); /* selector */
  rex = arg2(parms); /* arg */
  lnd   = dims(rex);
  abp   = rex.vr->vard;
  rbp   = r.s.lds.ip;     /* short hand */
  lns   = nels(lex);
  integer bas[lnd],ind[lnd],lim[lnd],sel[lnd];
  if (lnd && lnd != lns) {
    if (sgltn(lex)) {
      for (i=0;i<lnd;i++) sel[i] = vadd(lex).np[0];
    }  else {
      error(dom_range,"(p (, selector)) must equal (rank arg)");
    }
  } else {
    for (i=0;i<lnd;i++) sel[i] = vadd(lex).np[i];
  }
  if (lnd > 0)  {/* (rank res) <=> (rank arg) */
    for (i=0;i<lnd;i++) {
      k      = sel[i];
      j      = abp[i] - labs(k);
      rbp[i] = (j > 0) ? j : 0;
      ind[i] = bas[i] = 0;
      lim[i] = abp[i];
      if (k < 0) lim[i] = j; /* drop from end of dim */
      else       bas[i] = k; /* drop from beginning of dim */
    }
  }  else { /* scalar arg */
    lnd = nels(lex); /* (rank res) <=> (p (cat selector)) */
    abp = rbp;
    for (i=0;i<lnd;i++) {
      k = vadd(lex).np[i];
      lim[i] = rbp[i] = (k != 0) ? 0 : 1;
      ind[i] = bas[i] = 0;
    }
  }
  result = allocv(thisType=vrtype(rex), lnd, r.s.lds);
  if (nels(result) == 0) return(result);
  tex = vadd(rex);
  res = vadd(result);
  i=k=0;
  while (true) {
    j = flag = 0;
    while (!flag && j < lnd) { /* lowest dimension has priority */
      if (ind[j] < bas[j])  flag = 1;         /* pre drop  */
      else if (ind[j] < lim[j])  flag = 0;    /* copy      */
      else if (ind[j] < abp[j])  flag = 1;    /* post drop */
      j++;
    }
    if (flag) k++; /*skip*/
    else
      switch (thisType) {
      case num: res.np[i++] = tex.np[k++];  break;
      case cpx: res.cp[i++] = tex.cp[k++];  break;
      case chr: res.sp[i++] = tex.sp[k++];  break;
      case ref: res.pt[i++] = tex.pt[k++];  break;
      default: error(sys_cor,"drop");
      }
    j = lnd - 1;
    ind[j]++;
    while (ind[j] >= abp[j]) {
      ind[j] = 0;
      j--;
      if (j < 0) return(result);
      ind[j]++;
    }
  }
  return(result); // protected by sendres
}

static pointer iota(integer nargs, pointer parms) {
  pointer rent,result;
  integer j,k,noels;
  number  inc, val, top, temn, tolerance;

  rent   = arg1(parms);
  noels = nels(rent);
  if (noels < 1 || noels > 3 || !isNum(rent)) error(dom_range,"iota");
  else {
    tolerance  = getNumVal(a.v.hct);
    switch (noels) {
    case 1:
      j   = (integer)(*vadd(rent).np+iround);
      val = getNumVal(a.v.hio);
      inc = 1;
      break;
    case 2:
      val  = vadd(rent).np[0];
      top  = vadd(rent).np[1];
      temn = fabs(top - val);
      inc  = val < top ? 1 : -1;
      j    = 1 + floor(temn);
      break;
    case 3:
      val = vadd(rent).np[0];
      inc = vadd(rent).np[1];
      top = vadd(rent).np[2];
      if (inc == 0) error(inv_funarg,"inc must be non-zero");
      if (inc > 0) temn =  (top + tolerance - val) / inc;
      else         temn =  (top - tolerance - val) / inc;
      if (temn < 0) j = 0;
      else j = 1 + trunc(temn);
      break;
    }
    r.s.lds.ip[0] = (j>=getNumVal(a.v.hio))?j:0;
    result    = allocv(num, 1, r.s.lds);
    for (k = 0; k < j; k++) {
      result.vr->varadd.np[k] =
	(fabs(val) < tolerance) ? idadd : val; /* nonzero >= tolerance */
      val += inc;
    }
  }
  return(result); // protected by sendres
} /* iota */

static pointer alpsIndex(integer nargs, pointer parms) {
  pointer lent,rent,result,lex,rex,res,temp;
  integer i,j,l,id,noels,lim;
  uchar   temc;
  number  temn,tolerance;
  complex temx;


  lent   = arg1(parms);
  rent   = arg2(parms);
  result = allocv(num, dims(rent), pvard(rent));
  id     = (integer)getNumVal(a.v.hio);  /* index origin */
  noels  = nels(rent);
  lim    = nels(lent);   /*ext vardim should be < 2*/
  l      = lim + id;
  rex    = vadd(rent);
  lex    = vadd(lent);
  res    = vadd(result);
  if (!typeComp(rent, lent)) {
    for (i = 0; i < noels; i++) res.np[i] = l;
  } else {
    switch (type(rent)) {
    case num:
      tolerance  = getNumVal(a.v.hct);
      for (i = 0; i < noels; i++) {
	temn = rex.np[i];
	for (j=0; j < lim ; j++ )
	  if (ctcomp(lex.np[j],temn,tolerance)==0) break;
	res.np[i] = (j == lim) ? l : j + id;
      }
      break;

    case cpx: 
      tolerance = getNumVal(a.v.hct);
      for (i = 0; i < noels; i++) {
	temx = rex.cp[i];
	for (j=0; j < lim; j++)
	  if (cpequal(lex.cp[j],temx,tolerance)) break;
	res.np[i] = (j == lim) ? l : j + id;
      }
      break;

    case chr:
    case sym:
    case cym:
      for (i = 0; i < noels; i++) {
	temc = rex.sp[i];
	for (j=0; j < lim ; j++) if (lex.sp[j] == temc) break;
	res.np[i] = (j == lim) ? l : j + id;
      }
      break;

    case ref:
      for (i = 0; i < noels; i++) {
	temp = rex.pt[i];
	for (j=0; j < lim ; j++) if (isEqual(lex.pt[j], temp)) break;
	res.np[i] = (j == lim) ? l : j + id;
      }
      break;
    default: error(sys_cor,"arg1 not one of num cpx chr sym ref");
    }/*case*/
  }
  return(result);
}

static pointer element(integer nargs, pointer parms) {
  pointer lent,lex,rent,rex,res,rev,temp;
  uchar   temc;
  integer lim,noels,i,j,ri=1;
  number temn,tolerance;
  complex temx;

  lent  = arg1(parms);
  rent  = arg2(parms);
  res   = allocv(num, dims(lent), pvard(lent)); /* (pp res) == (pp rarg) */
  noels = nels(lent);
  lim   = nels(rent);
  lex   = vadd(lent);
  rex   = vadd(rent);
  rev   = vadd(res);
  if (!isRef(lent) && !typeComp(lent, rent)) {
    for (i = 0; i < noels; i++)  rev.np[i] = idadd;
  } else {
    switch (type(lent)) {
    case num:
      tolerance  = getNumVal(a.v.hct);
      for (i = 0; i < noels; i++) {
	temn = lex.np[i];
	for (j=0; j < lim ; j++ ) 
	  if (ctcomp(rex.np[j],temn,tolerance)== 0) break;
	if (j == lim) rev.np[i] = idadd;
	else          rev.np[i] = idprod;
      }
      break;

    case cpx: 
      tolerance = getNumVal(a.v.hct);
      for (i = 0; i < noels; i++) {
	temx = lex.cp[i];
	for (j=0; j < lim; j++)
	  if (cpequal(rex.cp[j],temx,tolerance)) break;
	if (j == lim) rev.np[i] = idadd;
	else          rev.np[i] = idprod;
      }
      break;
 
    case sym:
    case cym:
    case chr:
      for (i = 0; i < noels; i++) {
	temc = lex.sp[i];
	for (j=0; j < lim ; j++) if (rex.sp[j] == temc) break;
	if (j == lim) rev.np[i] = idadd;
	else          rev.np[i] = idprod;
      }
      break;

    case ref:
      for (i = 0; i < noels; i++) {
	if (isSymbol(rent)) rev.np[i] = isEqq(rent ,lex.pt[i]) ? idprod : idadd;
	else {
	  for (j=0; j < lim ; j++) if (isEqual(rex.pt[j],lex.pt[i])) break;
	  if (j == lim) rev.np[i] = idadd;
	  else          rev.np[i] = idprod;
	}
      }
      break;
    default: error(sys_cor,"element");
    }
  }
  return(res); // sendres
}

static pointer reverse(integer n, pointer x) {
  pointer mex, rex, res,result;
  integer axis,lnd,ainc,binc,i,j,k,l,j1,j2,rep;
  vtype   typ;

  rex  = argn(n-1,x);
  lnd  = dims(rex);
  axis = (n==1) ? lnd-1 : checkAxis(arg1(x),lnd); // zero org now
  if (nels(rex) <= 1) return(rex);
  result = allocv(typ=vrtype(rex),lnd,pvard(rex));
  ainc = binc = 1;
  for (i=0;i<axis;i++)      binc *= getDim(rex,i);
  for (i=axis+1;i<lnd;i++)  ainc *= getDim(rex,i);
  rep = getDim(rex,axis);
  mex = vadd(rex);
  res = vadd(result);
  l = 0;
  for (i=0;i<binc;i++) {
    for (j=0;j<=rep/2;j++) {
      j1 = l + j*ainc;
      j2 = l + (rep - j - 1)*ainc;
    for (k=0;k<ainc;k++) {
      switch (typ) {
      case num: res.np[j1 + k] = mex.np[j2 + k];
 	        res.np[j2 + k] = mex.np[j1 + k];
		break;
      case cpx: res.cp[j1 + k] = mex.cp[j2 + k];
         	res.cp[j2 + k] = mex.cp[j1 + k];
		break;	
      case chr: res.sp[j1 + k] = mex.sp[j2 + k];
	        res.sp[j2 + k] = mex.sp[j1 + k];
		 break;
      case ref: res.pt[j1 + k] = mex.pt[j2 + k];
	        res.pt[j2 + k] = mex.pt[j1 + k];
		break;
      default:  error(pgmr_inc,"bad type");
      }}}
      l += rep * ainc;
  }
  return(result); // protected by sendres
}

static pointer rotate(integer n, pointer x, bool shft) { /* rotate/shift */
  pointer lex, rota, mex, rex,res,result;
  integer i,j,k,l,lnd,m,noels,amnt,axis,ainc,binc,cinc,rep,j1,j2,j3,rotel,na;
  vtype   thisType;
  bool    shftc;
  
  lex = argn(n-2,x);   /* rotate amount */
  rex = argn(n-1,x);   /* rotatee       */
  lnd   = dims(rex);
  axis  = (n==3) ? checkAxis(arg1(x),lnd) : lnd - 1; // zero org now
  noels = nels(rex);
  rotel = nels(lex);
  rota  = vadd(lex);
  rep   = (noels==0) ? 1 : getDim(rex,axis);
  if (dims(lex) != 0 && dims(lex) == (lnd - 1)) {
    for (i=j=0;i<lnd;i++) 
      if (i != axis && lex.vr->vard[j++] != getDim(rex,i)) goto extend;
    cinc = 1;
  } else { /* singleton extension */
  extend:
    if (!sgltn(lex)) error(dom_range,"amnt");
    cinc    = 0;
  }
  if (noels <= 1) return(rex); /* null, scalar or singleton */
  result = allocv(thisType=vrtype(rex),lnd,pvard(rex));
  ainc = binc = 1;
  for (i=0;i<axis;i++)      binc *= getDim(rex,i);
  for (i=axis+1;i<lnd;i++)  ainc *= getDim(rex,i);
  mex = vadd(rex);
  res = vadd(result);
  l = m = 0;
  for (i=0;i<binc;i++) {
    for (j=0;j<rep;j++) {
      j1 = l + j*ainc;
      for (k=0;k<ainc;k++) {
	j3 = m+cinc*k;
	ASSERT(j3<rotel);
	amnt = rota.np[j3];
	na = ((amnt < 0) ? amnt + rep : amnt)%rep;
	j2 = l + ((j + na)%rep)*ainc;
	if (shft) {
	  shftc = ((j + amnt) < 0 || (j + amnt >= rep));
	  switch (thisType) {
	  case num: res.np[j1 + k] = (shftc) ? 0    : mex.np[j2 + k]; break;
	  case cpx: res.cp[j1 + k] = (shftc) ? 0    : mex.cp[j2 + k]; break;
	  case chr: res.sp[j1 + k] = (shftc) ? ' '  : mex.sp[j2 + k]; break;
	  case ref: res.pt[j1 + k] = (shftc) ? pnil : mex.pt[j2 + k]; break;
	  default: error(pgmr_inc,"bad type");
	  }
	} else {
	  switch (thisType) {
	  case num: res.np[j1 + k] = mex.np[j2 + k]; break;
	  case cpx: res.cp[j1 + k] = mex.cp[j2 + k]; break;
	  case chr: res.sp[j1 + k] = mex.sp[j2 + k]; break;
	  case ref: res.pt[j1 + k] = mex.pt[j2 + k]; break;
	  default: error(pgmr_inc,"bad type");
	  }}}}
    l += rep * ainc;
    m += cinc* ainc;
  }
  return(result); // sendres
}

static pointer transpose(int nargs, pointer parms) { /* (tr arg [index]) */
  integer i,org,j,k,l,lnd,rnd,lim,inc,noels,s,carry,mini,maxi,fax;
  integer *argd,*pind,*resd;
  pointer lex,mex,rex,tex,res;
  vtype   thisType;

  org  = (integer)getNumVal(a.v.hio);
  rex  = (nargs==1) ? arg1(parm) : arg2(parm);  /* arg               */
  lnd  = dims(rex);                 /* number of dimensionsn in arg */
  makeBlock(lnd,&r.s.indad);
  pind = r.s.indad.ip;              /* local index vector based arg */
  argd = rex.vr->vard;              /* dimension vector for arg     */
  resd = r.s.lds.ip;                /* dimension vector for res     */
  if (nargs == 2) { /* dyadic case */
    lex = arg1(parms);               /* index  */
    if (dims(lex) != 1 || dims(rex) != nels(lex)) /* check conformity */
      error(inv_funarg,"Expected (eq (p index) (rank arg)) => t");
    for (mini=INT_MAX,i=maxi=fax=0;i<lnd;i++) { /* check indexes */
      pind[i] = k = (int)vadd(lex).np[i] - org;
      if (k < 0 || k >= lnd) error(dom_range,"index out of range");
      if (k < mini) mini = k; 
      if (k > maxi) {
	maxi = k;            /* highest dimension of arg in result */
	fax  = i;            /* dimension number of maxi in arg    */ 
      }
    }
    rnd = maxi + 1;          /* number of dimesions in result      */
    if (mini > 0) 
      error(dom_range,mkEmes(3,"Lowest index: ",itos(org)," not included"));
    for (i=0;i<rnd;i++) { /* check for completeness */
      for (j=0;j<lnd;j++) if (pind[j] == i) goto loop;
      error(dom_range,"indexes not complete");
    loop: continue;
    }
  } else { /* monadic case */
    if (lnd <= 1) return rex;
    for (i=0;i<lnd;i++)  pind[i] = lnd - i - 1; /* reverse the axes */
    //   if (lnd == 1) { /* we gratuitously make it transformable */
    //lnd++;
    // argd    = r.s.ldx.ip; /* the other block cause we inc'd num dims*/
    //  pind[0] = 1;      pind[1] = 0;
    //  argd[0] = 1;      argd[1] = nels(rex);
    //} 
    rnd  = lnd;
    fax  = 0;
    maxi = pind[0]; 
  }
  integer rax[rnd],rinc[rnd],index[rnd],incr[lnd];
  /* calculate increments for each index in arg */
  for (inc=1,i=lnd-1;i>=0;i--) { incr[i] = inc; inc *= argd[i]; }
  for (i=k=0;i<lnd;i++) { /* build dimension vector for res */
    for (j=0;j<k;j++) if (rax[j] == pind[i])  break;
    if (j==k) {                          /* new unique axis in result     */
      resd[k]  = argd[pind[i]];          /* kth dimension for result      */
      rinc[k]  = incr[pind[i]];
      index[k] = 0;
      rax[k++] = pind[i];                /* note its presence             */
    } 
  }
  if (rnd < lnd) { /* have duplicates redistribute */
    for (i=maxi;i>=0;i--) {
      rinc[i] = 0; k=INT_MAX;
      for (j=0;j<lnd;j++) 
	if (pind[j] == i) {
	  rinc[i] += incr[j];
	  if (argd[j] < k) k = argd[j];
	}
      resd[i] = k;
    }
  }
  ASSERT(resd == r.s.lds.ip); /* insanity check */
  res   = allocv(thisType=vrtype(rex), rnd, r.s.lds); 
  noels = nels(res);
  tex   = vadd(rex);  /* argument */
  mex   = vadd(res);  /* result   */
  lim   = resd[maxi]; /* dim in arg for fastest moving axis in res */
  inc   = rinc[maxi];
  for (i=j=k=0;i<noels;i++) {
    switch (thisType) {
    case num:  mex.np[i] = tex.np[k]; break;
    case cpx:  mex.cp[i] = tex.cp[k]; break;
    case chr:  mex.sp[i] = tex.sp[k]; break;
    case ref:  mex.pt[i] = tex.pt[k]; break;
    default: error(sys_cor,"transpose");
    }
    if ((i+1) == noels) break;
    k += inc; 
    if (++j == lim) { /* calculate nested loop indexes */
      j=k=0;  carry=1;
      for (s=maxi-1;s>=0;s--) { /* propagate carry up in arg index order */
	if (carry) {
	  index[s]++;
	  if (index[s]==resd[s]) {index[s]=0; carry = 1;}
	  else carry = 0;
	}
	k += index[s]*rinc[s];
      }
    }
  }
  return(res); // sendres
}

/* Matrix inversion using LU decomposition with backsubstitution.
   (see numerical recipes 1987 pp36-39 )
   Here we are solving for x in  a*x=b where
   A  = input Arg
   X  = eventual inverse
   B = identity matrix for monadic or set of columns for dyadic
   (m A) = inverse of A
   (m B A) = X such tha A.X = B i.e. (onep (r '^ (cat (= B (@ '+ '* A X)))))=>t
*/
static pointer minv(integer nargs, pointer parms) {
  integer i,j,k,n,nc;
  pointer result,rex,lex,res,tex;

  lex = arg1(parm);
  if (nargs==1) {
    if  (dims(lex) != 2 || getDim(lex,0) != getDim(lex,1))
      error(inv_funarg,"matrix not square");
    n = getDim(lex,0);
    tex = vadd(lex);
  } else {
    rex = arg2(parms); 
    if  (dims(rex) != 2 || getDim(rex,0) != getDim(rex,1))
      error(inv_funarg,"divisor matrix not square");
    n = getDim(rex,0);
    tex = vadd(rex);
    if ((nels(lex) != 1) && (dims(lex) == 1 && nels(lex) != n) &&
	(dims(lex) == 2 && getDim(lex,0) != n))
	error(inv_funarg,"dividend has bad shape");
  }
  { /* start with LU decomp */
    number a[n][n],ind[n],vv[n],aamax,dum,sum,epsilon=1e-20;
    integer ii,ll,imax=-1,d=1; /* even / odd row swap indicator */
    for (i=0;i<n;i++) { /* find row scale factor and copy arg into a[] */
      aamax = idadd;
      for (j=0;j<n;j++) {
	a[i][j] = tex.np[i*n+j];
	if (fabs(a[i][j]) > aamax) aamax = fabs(a[i][j]);
      }
      if (aamax == idadd) error(inv_funarg,"arg singular");
      vv[i] = idprod/aamax; /* row scale factor */
    }
    for (j=0;j<n;j++) { /* loop over columns with Crouts method */
      for (i=0;i<j;i++) {
	sum = a[i][j];
	for (k=0;k<i;k++) sum += -a[i][k]*a[k][j];
	a[i][j] = sum;
      }
      aamax = idadd;
      for (i=j;i<n;i++) { /* find largest element for pivot */
	sum = a[i][j];
	for (k=0;k<j;k++) sum += -a[i][k]*a[k][j];
	a[i][j] = sum;
	dum = vv[i]*fabs(sum);
	if (dum >= aamax) { imax = i; aamax = dum; }
      }
      if (j != imax) {/* swap rows ? */
	for (k=0;k<n;k++) { dum=a[imax][k]; a[imax][k]=a[j][k]; a[j][k]=dum; }
	d = -d; /* toggle row swap indicator */
	vv[imax] = vv[j]; /* swizzle scale factor too! */
      }
      ind[j] = imax;
      if (a[j][j] == 0) a[j][j] = epsilon;
      if (j != n-1) { /* divide through by pivot */
	dum = idprod/a[j][j];
	for (i=j+1;i<n;i++) a[i][j] *= dum;
      }
    }
    if (nargs==1) {
      result = allocv(num, dims(lex), pvard(lex));
      res = vadd(result);
      for (i=0;i<n;i++) {
	for (j=0;j<n;j++) res.np[i*n+j] = idadd;
	res.np[i*n+i] = idprod;
      }
      nc = n;
    } else {
      r.s.lds.ip[0] = n;
      r.s.lds.ip[1] = nc = (dims(lex) <= 1) ? 1 : getDim(lex,1);
      result = allocv(num, 2, r.s.lds);
      res = vadd(result);
      tex = vadd(lex);
      if (nels(lex)==1) 
	for (i=0;i<n;i++) res.np[i] = tex.np[0]; /* extend scalar */ 
      else
	for (i=0;i<nels(lex);i++) res.np[i] = tex.np[i]; /* move lex into res */
    }
    for (k=0;k<nc;k++) { /* iterate through columns of result */
      ii = -1; /* now hit the backsubstitution */
      for (i=0;i<n;i++) {
	ll              = ind[i];
	sum             = res.np[ll*nc+k];
	res.np[ll*nc+k] = res.np[i*nc+k];
	if (ii >= 0) for (j=ii;j<i;j++) sum += -a[i][j]*res.np[j*nc+k];
	else if (sum != 0) ii = i;
	res.np[i*nc+k] = sum;
      }
      for (i=n-1;i>=0;i--) {
	sum = res.np[i*nc+k];
	if (i<n-1) for (j=i+1;j<n;j++) sum += -a[i][j]*res.np[nc*j+k];
	res.np[i*nc+k] = sum/a[i][i];
      }
    }
  }
  return(result); // sendres
} /* minv */

/* decode base */
static pointer decod(integer nargs, pointer parms) {
  integer plex,prex,pres,lld,rfd,rowdim;
  pointer lex,rex,res;
  integer i,j,k,m,n,mm,li,ri;

  lex = arg1(parms);
  rex = arg2(parms);
  plex = dims(lex);
  prex = dims(rex);
  pres = max(plex,1) + max(prex,1) - 2;
  lld = (plex == 0) ? 1: getDim(lex,plex-1);
  rfd = (prex == 0) ? 1: getDim(rex,0);
  if ((lld != rfd) && (lld != 1) && (1 != rfd))
     error (dom_range,"dec args not conformable");
  makeBlock(pres,&r.s.lds);
  for (i=j=0;j<plex-1;j++) r.s.lds.ip[i++] = getDim(lex,j);
  for (j=1;j<prex;j++)     r.s.lds.ip[i++] = getDim(rex,j);
  res    = allocv(num, pres, r.s.lds);
  li     = (lld==1) ? 0 : 1;
  ri     = (rfd==1) ? 0 : 1;
  rowdim = (lld==1) ? rfd : lld ;
  {
    number cv[rowdim],lval,cval;
    integer lexrows=1,rexcols=1;
    bool flag = false;
    for(i=0;i<plex-1;i++) lexrows *= getDim(lex,i); /* calc num rows in lex */
    for(i=1;i<prex;i++)   rexcols *= getDim(rex,i); /* saynomoh */
    k = 0;
    for (j=0;j<lexrows;j++) { /* for each row in lex */
      k += rowdim;
      if (plex==0 && !flag) {
	lval = getNum(lex);
	cval = 1;
	flag = true; /* no point recalculating */
	for (i=rowdim-1;i>=0;i--) {cv[i] = cval; cval *= lval;}
      } else {
	cv[rowdim-1] = cval = 1;
        m = 1;
	for (i=rowdim-2;i>=0;i--) {
	  cval *= vadd(lex).np[k-m]; /*cruise thru lex in rev row major*/
	  m += li;
	  cv[i] = cval;
	}
      }
      for (m=0;m<rexcols;m++) { /* for each col in rex */
	cval = 0;
	mm   = 0;
	for (n=0;n<rowdim;n++) {
	  cval += cv[n]*vadd(rex).np[mm*rexcols+m];
	  mm += ri;
	}
	res.vr->varadd.np[j*rexcols+m] = cval;
      }
    }
  }
  return(res); // sendres
} /* decod */

static pointer encod(integer nargs, pointer parms) {
  integer i,j,k,m,n,lnd;
  pointer result,lex,rex;
  number *lp,*rp,*res,val,quo,tem;

  lex = arg1(parms);
  rex = arg2(parms);
  lnd = dims(lex)+dims(rex);
  makeBlock(lnd,&r.s.lds);
  for ( i=j=0;j<dims(lex);j++) r.s.lds.ip[i++] = getDim(lex,j);
  for ( j=0;j<dims(rex);j++)   r.s.lds.ip[i++] = getDim(rex,j);
  result = allocv(num, lnd, r.s.lds);
  lp  = vadd(lex).np;
  rp  = vadd(rex).np;
  res = vadd(result).np;
  n   = nels(lex)-1;
  m   = nels(rex);
  k   = 0;
  for (i=0;i<m;i++) {
    val = rp[i];
    for (j=n;j>=0;j--) {
      tem = lp[j];
      quo = (tem != idadd) ? floor(val/tem) : idadd;
      res[j*m+i] = val - quo*lp[j];
      val = quo;
    }
  }
  return(result);  // sendres
} /* encod */


static pointer redscan(lispTypes pc, integer nargs, pointer parms) {
/* (reduce by  (operation)  [along AXIS [by WINDOW]]    array)  */
/* op           sub_op             axis     wind        rent    */
/* (scan  with (operation)  [along AXIS [in DIRECTION]] array)  */
  pointer rent,res,arr,rslt;
  integer i,j,k,l,len,li,ri,ai,ls,rs,as,rep,fax,pax,axis,lnd,dimd;
  integer noels,runs,rlen,step,wind,win,roff;
  number  temp;
  vtype   ltype;
  lispTypes sub_op = arg1(parms).at->spl;
  
  rent  = argn(nargs-1,parms);
  ltype = vrtype(rent);
  noels = nels(rent);
  dimd  = dims(rent);
  if (!dimd)  return rent;
  lnd  = dimd - 1;
  axis = (nargs == 2) ? lnd : checkAxis(arg2(parms),dims(rent));
  rep  = getDim(rent,axis);
  wind = (nargs == 4) ? getInt(arg3(parms)) : (pc==s_scan) ? 1 : rep;
  step = labs(wind);
  if (pc == s_scan && step != 1)
    error(inv_funarg,"scan direction must be 1 or -1");
  if (nargs == 4 && wind == 0) {
    for (i=0;i<dimd;i++) r.s.lds.ip[i] = getDim(rent,i);
    r.s.lds.ip[axis]++;
    rslt = allocv(num, dimd, r.s.lds);
    res  = vadd(rslt);
    temp = getNum(idelem[sub_op - s_dist]);
    len  = nels(rslt);
    for (i=0;i<len;i++) res.np[i] = temp;
    return rslt;
  }
  if (step==1) step = rep; /* shorthand for (aref (p arg) axis) */
  runs = rep - (step - 1);
  if (step > rep) error(inv_funarg,"window cannot exceed dimension of axis");
  if (runs > 1)  lnd  = dimd;
  j   = 0;
  fax = 1;
  for (i = 0; i < dimd; i++) {
    if (i != axis) {
      r.s.lds.ip[j++] = getDim(rent,i);
      if (i > axis)  fax *= getDim(rent,i);
    } else if (runs > 1) r.s.lds.ip[j++] = runs;
  }
  if (pc == s_reduc) {
    rslt = allocv(ltype, lnd, r.s.lds);
    ai = li = 0;
    ri = fax;
  } else { /* scan */
    rslt = allocv(ltype, dimd, pvard(rent));
    ai = li = ri = fax;
    runs = 1;
    step = rep;
  }
  if (noels == 0) {
    res  = vadd(rslt);
    temp = getNum(idelem[sub_op - s_dist]);
    len  = nels(rslt);
    for (i=0;i<len;i++) res.np[i] = temp;
    return rslt;
  }
  /* theory of operation:
     Given rent,
     let rep  = dimension of the axis in argument
         runs = dimension of the axis in result for runs > 1
	 wind = window or run length
         fax  = the product of dimensions following axis
         pax  = the product of dimensions preceding axis

     Then for
         0 <= i < wind,             (varying fastest)
         0 <= l < runs, 
         0 <= k < fax,
         0 <= j < pax
     the index sequence a(i,l,k,j)= i*li + l*fax + k + j*fax*rep
     gives the appropriate traversal index for the
     argument (rs) for both reduce and scan as well as
     the result (as) for scan only.
     The index sequence a(i,l,k,j)= i*li + l*fax + k + j*fax*runs
     runs through the result for reduce.
     Note that pax*fax*rep  = nels(rent), 
     for scan nels(result) = nels(rent)
     and for reduce pax*fax*runs = nels(result) */

  len  = fax * rep;   /* temp */
  rlen = fax * ((pc==s_reduc) ? runs : rep);
  win  = step - 1;
  pax  = noels / len;
  res  = vadd(rslt);
  arr  = vadd(rent);
  if (wind > 0 || (pc==s_reduc && runs>1)) {
    if (wind < 0) { /* negative reduction */
      roff = win;
      ri   = -ri;
    } else {        /* positive reduction or scan */
      roff = 0;
    }
    for (j = 0; j < pax; j++) { /* pax is the no of back_tracks of ls */
      for (k = 0; k < fax; k++) {
	for (l=0;l<runs;l++) {
	  /* assume compiler can hoist invariants in rs and as calcs */
	  rs = len  * j + fax * (l + roff) + k;
	  as = rlen * j + fax * l + k;
	  if (isNum(rent))      res.np[as] = arr.np[rs];
	  else if (isCpx(rent)) res.cp[as] = arr.cp[rs];
	  else error(pgmr_inc,"redscan");
	  ls   = as;
	  rs  += ri;
	  as  += ai;
	  vexec(sub_op, ltype, res, arr, rslt, &ls, &rs, &as, li, ri, ai, win);
	}
      }
    }
  } else { /* ((full length reduce) and scan) with negative window */
    integer *lim;
    ri = -ri;
    if (pc==s_reduc) {
      roff = win;
      lim  = &win;
    } else {
	runs = rep;
	ai=li=roff=0;
	lim = &l;
    }
    for (j = 0; j < pax; j++) {
      for (k = 0; k < fax; k++) {
	for (l=0;l<runs;l++) {
	  rs = len  * j + fax * (l + roff) + k;
	  as = rlen * j + fax * l + k;
	  if (isNum(rent))      res.np[as] = arr.np[rs];
	  else if (isCpx(rent)) res.cp[as] = arr.cp[rs];
	  else error(pgmr_inc,"redscan");
	  ls   = as;
	  rs  += ri;
	  /* note operand order switch: res <-> arr , ls <-> rs */
	  /* first iteration when l==0 then rs==-1 but *lim==0 so vexec nop */
	  vexec(sub_op, ltype, arr, res, rslt, &rs, &ls, &as, ri, li, ai, *lim);
	}
      }
    }
  }
  return rslt; // sendres
}

static pointer innerp(integer nargs, pointer parms) {
      /*  inner product by (op1) and (op2)  of (ARR1) AND (ARR2) */
      /*  op                                   lent       rent  */
  pointer lent,rent,tent,res,rp,lp,tp,result;
  lispTypes op1,op2;
  integer i,j,k,l,lsize,rsize,ldim,lnd,rdim,rep,as,id,temi,rc;
  vtype restyp;
  op1  = arg1(parms).at->spl;
  op2  = arg2(parms).at->spl;
  lent = arg3(parms);
  ldim = dims(lent);
  rent = arg4(parms);
  rdim = dims(rent);
  if (!ck2Arg(op2, lent, rent)) error(inv_funarg,"Incompatible argument types");
  restyp = getResType(op2,lent,rent,false,type(rent),&rc);
  if (ldim==0) { i=0;  k=1;                      li=0; }
  else         { i=1;  k=getDim(lent,ldim - 1);  li=1; }
  if (rdim==0) { j=0;  l= 1;	           ri=0; }
  else         { j=1;  l=getDim(rent,0); ri= (l==0) ? 0 : nels(rent) / l; }
  if (k != l && i != 0 && j != 0){
    if (!sgltn(rent) && sgltn(lent))      { i=0; ldim=0; k=1; li=0; }
    else if (!sgltn(lent) && sgltn(rent)) { j=0; rdim=0; l=1; ri=0; }
    else error(dom_range,"inner product");
  }
  lnd = ldim - i + rdim - j;
  makeBlock(lnd, &r.s.lds);
  rep   = (k < l) ? l : k;
  r.s.ldx.ip[0] = rep;
  tent = allocv(restyp, 1, r.s.ldx);
  protect(tent);   
#if (COMPLEX)
  if (restyp==cpx || rc) {
    cpxAdjust(op2,&lent,&rent,NULL);
  }
#endif
  j = 0;
  for (i = 0; i <= ldim - 2; i++)  r.s.lds.ip[j++] = getDim(lent,i);
  for (i = 1; i < rdim;      i++)  r.s.lds.ip[j++] = getDim(rent,i);
  result = allocv(restyp, lnd, r.s.lds);
  res   = vadd(result);
  tp    = vadd(tent);
  rp    = vadd(rent);
  lp    = vadd(lent); 
  lsize = nels(lent) / k;
  rsize = nels(rent) / l;
  as  = 0;
  for (i = 0; i < lsize; i++) {
    for (j = 0; j < rsize; j++) {
      rs = j;
      ls = i * k;
      id = 0;
      vexec(op2, type(rent), lp, rp, tent, &ls, &rs, &id, li, ri, 1, rep);
      if (id > nels(tent)) alpsOutsn(alpserr,2,"id",itosn(id-nels(tent),12));
      if (restyp==cpx) res.cp[as] = isNum(tent) ? tp.np[0] : tp.cp[0];
      else             res.np[as] = tp.np[0];
      id = 1;
      temi = as;
      vexec(op1, restyp, tp, res, result, &id, &temi, &as, 1, 0, 0, rep - 1);
      as++;
    }
  }
  if (as > nels(result))
    alpsOutsn(alpserr,2,"as",itosn(as - nels(res),12));
  unprotall();
  return(result);  // sendres
}  /*inrp*/

static pointer outerp(integer nargs, pointer parms) {
      /* (o)uter product by (operation) OF (ARR1) AND (ARR2) */
      /*  op                  sub_op        lent       rent  */
  pointer lent,rent,res,result,lex,rex,lp,rp;
  lispTypes sub_op;
  integer i,ldim,rdim,lnd,as,lim,rim,rc;
  vtype restyp;
  sub_op = arg1(parms).at->spl;
  lent   = arg2(parms);
  rent   = arg3(parms);
  if (!ck2Arg(sub_op, lent, rent))
    error(inv_funarg,"Incompatible argument types");
  restyp = getResType(sub_op,lent,rent,true,type(rent),&rc);
  ldim   = dims(lent);
  rdim   = dims(rent);
  lim    = nels(lent);
  rim    = nels(rent);
#if (COMPLEX)
  if (restyp==cpx || rc) {
    if (isNum(lent)) {
      lex = allocv(cpx,ldim,pvard(lent));
      rp  = vadd(lent);
      lp  = vadd(lex);
      for (i=0;i<lim;i++) lp.cp[i] = rp.np[i];
      lent = lex;
    }
    if (isNum(rent)) {
      rex = allocv(cpx,rdim,pvard(rent));
      rp  = vadd(rent);
      lp  = vadd(rex);
      for (i=0;i<rim;i++) lp.cp[i] = rp.np[i];
      rent = rex;
    }
  }
#endif
  lnd    = ldim + rdim;
  makeBlock(lnd, &r.s.lds);
  for (i = 0; i < ldim; i++) r.s.lds.ip[i]        = getDim(lent,i);
  for (i = 0; i < rdim; i++) r.s.lds.ip[i + ldim] = getDim(rent,i);
  result = allocv(restyp, lnd, r.s.lds);
  lp  = vadd(lent);
  rp  = vadd(rent);
  as  = 0;
  for (i = 0; i < lim; i++) {
    rs = 0;
    ls = i;
    vexec(sub_op, type(rent), lp, rp, result, &ls, &rs, &as, 0, 1,
	  1, rim);
  }
  unprotall(); 
  return(result); // sendres
}  /*outerp*/

/* indexing functions */

/* iset sets entries in mat indexed by ind to val, returns val.
   The number of indeces is determined by the last dimension of index.
   The number of elements in value must be <= to the number of indeces.
   The number of dimensions of index must be 1 or 2.
   If it is 2 the the number of elements in the first dimension of
   index must agree with the number of dimensions of the target array.
   However if dims(ind)==1 and dims(mat) > 1 we repeat the index value for
   each dimension of mat ie if ind=(i N) then we set the diagonal
   elements of mat to sucessive elements of val.
*/
static pointer iset(integer nargs, pointer parms) {
  integer ndims, idims, i, j, k, noels, numind, dind,matels;
  integer vind, iind, org,it,mind, iinc;
  pointer mat, val, ind, lex, rex, vex;
  mat = arg1(parms);
  val = arg2(parms);
  ind = arg3(parms);
  if (isSymbol(mat)) { error(inv_funarg,"iset cannot modify symbol"); }  
   if (!isRef(mat) && (!isAtom(val) || !typeComp(mat,val))) {
    if (isCpx(mat) && isNum(val)) val = mkCpxFromNum(val);
    else error(inv_funarg,"iset arg types not compatible");
  }
  if (!isRef(mat) && (nels(val) == 0)) return val;
  ndims = dims(mat);
  idims = dims(ind);
  if (idims == 0) { numind = 1; dind = 1; iinc = 0;}
  else
    if (idims == 1) {numind = nels(ind); dind = 1; iinc = 0;}
    else {numind = getDim(ind,idims-1); dind = getDim(ind,0); iinc = numind;}
  if ((ndims < 1) || (idims > 2) || (ndims != ((idims <= 1) ? ndims : dind)))
    error(dom_range,"shape mismatch");
   /*calculate dimension skip sizes */
  makeBlock(ndims, &r.s.lds);
  r.s.lds.ip[ndims - 1] = 1;  /* skip size for last dim is 1 */
  for (j = ndims - 1; j >= 1; j--) {
    r.s.lds.ip[j - 1] = r.s.lds.ip[j] * getDim(mat,j);
  }
  noels = (isRef(mat) && !isRef(val)) ? 1 : nels(val);
  //  if (numind < noels) error(dom_range,"too many values");
  org = getNumVal(a.v.hio);
  if (org < 0 || org > 1) error(sys_par,"#IO");
  matels = nels(mat);
  lex  = vadd(mat);
  rex  = vadd(ind);
  vex  = vadd(val);
  vind = 0;

 /* Check index value ranges for no side effect in case of error */
  for (i = 0; i < numind; i++) {
    iind = i;
    for (j=0;j<ndims;j++) {
      it = ((k=rex.np[iind])>=0) ? k - org : getDim(mat,j) + k;
      if ((it < 0) || (it >= getDim(mat,j))) {
	error(dom_range, mkEmes(6,"index value ",itos(it+org),
				" position ",itos(i+1),
				" dimension ", itos(j+1)));
      }
      iind +=iinc;
    }
  }

  /* Now do it for real */
    for (i = 0; i < numind; i++) {
      iind = i;
      mind = 0;
      for (j=0;j<ndims;j++) {
	it = ((k=rex.np[iind])>=0) ? k - org : getDim(mat,j) + k;
	mind += it*r.s.lds.ip[j];
	iind +=iinc;
      }
      ASSERT((mind >=0) && (mind < matels));
      switch (type(mat)) {
      case num:      lex.np[mind] = vex.np[vind++];    break;
      case cpx:      lex.cp[mind] = vex.cp[vind++];    break;
      case chr:      lex.sp[mind] = vex.sp[vind++];    break;
      case ref:      lex.pt[mind] = (isRef(val)) ? vex.pt[vind++] : val;  break;
      default: error(pgmr_inc,"iset");
      }
      if (vind >= noels) vind = 0; /* wrap vals */
    }
  return(val);
}

#if 0
static pointer aset(integer nargs, pointer parms) {
  integer ndims = 0, i, j, k, m, len, ldim, noels, org, pindex, item, carry;
  pointer mat, val, lex, tex, p;
  div_t dind;
   /* Procedure aset; BEGIN */
  mat = arg1(parms);
  val = arg2(parms);
  lex = arg3(parms);
  if (isSymbol(mat)) { error(inv_funarg,"aset cannot modify symbol"); }  
  if (!isRef(mat) && (!isAtom(val) || !typeComp(mat,val))) {
    if (isCpx(mat) && isNum(val)) val = mkCpxFromNum(val);
    else error(inv_funarg,"aset arg types not compatible");
  }
  if (!isRef(mat) && (nels(val) == 0)) return val;
  if ((nargs == 3) && !isNum(lex) && !isNil(lex)) {
    if (isRef(lex)) {
      k     = nels(lex);
      tex   = vadd(lex);
      ndims = dims(mat);
      makeBlock(ndims, &r.s.indad); /* array pointing to index arguments */
      j      = 0;
      pindex = false;
      for (i=0;(i<k) && (j<ndims);i++) {
	if (isColon(tex.pt[i])) {
	  if ((!pindex) || (i==k-1)) r.s.indad.pt[j++] = pnil;
	  else pindex = false;
	} else if (isNum(tex.pt[i])) {
	r.s.indad.pt[j++] = tex.pt[i];
	pindex = true;
	} 
	else error(inv_funarg,"non numeric index");
      }
      if (!pindex) {
	if (j < ndims) r.s.indad.pt[j] = pnil; /* last index was colon */
	else error(dom_range,"index");
      }
      ndims += (k - i); /* in case bailed out on j < 0 */
    } else error(inv_funarg,"invalid index type");
  } else {
      ndims = nargs - 2;
      if (ndims < 1) { error(inv_funarg,"aset res val sub1[,2,3..]"); }
      makeBlock(ndims, &r.s.indad);
      for (i = 0; i < ndims; i++) {
	p = argn(i + 2, parms);
	if (!isNum(p) && !isNil(p)) error(inv_funarg,"non numeric/nil index");
	r.s.indad.pt[i] = p;
      }
    }
  if (ndims != dims(mat)) error(dom_range,"shape mismatch");
  { integer numinds[ndims];
    integer iref[ndims];
    integer oref[ndims];

    /*calculate dimension skip sizes */
    noels = 1;
    ldim  = ndims - 1;
    makeBlock(ndims, &r.s.lds);
    r.s.lds.ip[ldim] = 1;  /* skip size for last dim is 1 */
    for (j=ldim; j>=1; j--) r.s.lds.ip[j - 1] = r.s.lds.ip[j] * getDim(mat,j);
    for (i=j=0; j<ndims; j++) {
      lex = r.s.indad.pt[j];
      if (isNil(lex)) {
	len = getDim(mat,j);
	r.s.indad.ip[j] = 0;
      } else {
	len = nels(lex);
	r.s.indad.pt[j] = vadd(lex);
      }
      while (i < dims(val) && getDim(val,i)==1) i++;
      if (len > 1) {
	if (!sgltn(val)) {
	  if (i==dims(val)) error(dom_range,"value shape mismatch");
	  if (len < getDim(val,i) || len > getDim(val,i))
	    error(dom_range,"value length mismatch");
	}
	i++;
      }
      noels     *= len;
      numinds[j] = len;
      iref[j]    = 0;
    }  /*for j */
    if (noels == 0) return val;
    org = getNumVal(a.v.hio);
    if (org < 0 || org > 1) error(sys_par,"#IO");
    lex = vadd(mat);
    tex = vadd(val);
    /* initialise iref, oref and k */
    for (k=0,j=ldim;j>=0;j--) {
      if (r.s.indad.ip[j]==0) item = iref[j];
      else {
	item = r.s.indad.pt[j].np[iref[j]];
	item = (item>=0) ? item - org : mat.vr->vard[j] + item;
      }
      k += item * r.s.lds.ip[j];
      oref[j] = item;
    }
    i = 0;
    for (j=0;j<noels;j++) {
      if (k < 0 || k >= nels(mat)) error(dom_range,"index out of bounds");
      switch (type(mat)) {
      case num: lex.np[k] = tex.np[i++]; break;
      case cpx: lex.cp[k] = isCpx(val) ? tex.cp[i++] : tex.np[i++]; break;
      case chr:
      case cym:
      case sym: lex.sp[k] = tex.sp[i++]; break;
      case ref: lex.pt[k] = isRef(val) ? tex.pt[i++] : val; break;
      default: error(sys_cor,"aref");
      }
      if (i==nels(val)) i=0;
      for (carry=1,m=ldim;(carry && m>=0);m--) { /* update iref, oref and k */
	dind    = div((iref[m] + carry),numinds[m]);
	iref[m] = dind.rem;
	if (r.s.indad.ip[m]==0) item = iref[m];
	else {
	  item = r.s.indad.pt[m].np[iref[m]];
	  item = (item>=0) ? item - org : mat.vr->vard[m] + item;
	}
	k      += (item - oref[m])*r.s.lds.ip[m];
	oref[m] = item;
	carry   = dind.quot;
      }
    }
  }
  return(val);
}

static pointer aref(integer nargs, pointer parms) {
  integer i, j, k, m,  noels, argels, ndims=0, newdims, ldim, item, org;
  pointer rex,  lex, tex, mex, res;
  bool    pindex;
  integer carry, totinds = 1;
  number  ind;
  div_t   dind;
  
/* Procedure aref; BEGIN */
  rex = arg1(parms);
  lex = arg2(parms);
  if ((nargs == 2) && !isNum(lex)) {
    if (isRef(lex)) {
      k     = nels(lex);
      tex   = vadd(lex);
      ndims = dims(rex);
      makeBlock(ndims, &r.s.indad); /* array pointing to index arguments */
      j = 0;
      pindex = false;
      for (i=0;(i<k) && (j<ndims);i++) {
	if (isColon(tex.pt[i])) {
	  if ((!pindex) || (i==k-1)) r.s.indad.pt[j++] = pnil;
	  else pindex = false;
	} else if (isNum(tex.pt[i])) {
	  r.s.indad.pt[j++] = tex.pt[i];
	  pindex = true;
	}
	else error(inv_funarg,"non numeric index element");
      }
      if (!pindex) {
	if (j <ndims) r.s.indad.pt[j] = pnil; /* last index was colon */
	else error(dom_range,"index");
      }
      ndims += (k - i); /* in case bailed out on j < 0 */
    } else if (isNil(lex)) {
      ndims = 1;
      ldim  = 1;
      r.s.indad.pt[0] = pnil;
    } else error(inv_funarg,"invalid index type");
  ldim  = ndims - 1;             /* last dimension in argument        */
  } else {
    ndims = nargs - 1;             /* number of dimensions in argument  */
    ldim  = ndims - 1;             /* last dimension in argument        */
    makeBlock(ndims, &r.s.indad); /* array pointing to index arguments */
    for (i = ldim; i >= 0; i--) {
      tex = argn(i+1,parms);
      if (!isNum(tex) && !isNil(tex)) 
	error(inv_funarg,"non numeric / nil index element");
      r.s.indad.pt[i] = tex;
    }
  }
  if (ndims != dims(rex))  error(dom_range,"index");
  { integer numinds[ndims];
    integer iref[ndims];
    integer oref[ndims];
    /*calculate size & shape of new element*/
    newdims = 0;
    r.s.lds.ip[ldim] = 1; /* last arg index stride is one */
    for (j=ldim; j>=1; j--) r.s.lds.ip[j - 1] = r.s.lds.ip[j] * getDim(rex,j);
    for (j = 0; j < ndims; j++) { /* calculate newdims */
      lex        = r.s.indad.pt[j];
      newdims   += (!isNil(lex)) ? dims(lex) : 1;
      numinds[j] = (!isNil(lex)) ? nels(lex) : getDim(rex,j);
      totinds   *= numinds[j];
      iref[j]    = 0;              /* index into user indexes */
    }
    makeBlock(newdims, &r.s.ldx);  /* dimensions for result */
    /* assign result dimensions */
    for (m=0,j = 0; j < ndims; j++) {
      lex = r.s.indad.pt[j];
      if (!isNil(lex)) {   /* have actual index values for this rex dimension */
	/* add result dimensions as appropriate            */
	for (k=0; k<dims(lex); k++) r.s.ldx.ip[m++] = getDim(lex,k);
	r.s.indad.pt[j] = vadd(lex);
      } else { /* use all elements in this arg dimension in result */
	r.s.ldx.ip[m++] = getDim(rex,j);
	r.s.indad.ip[j] = 0;
      }
    }  /*for j */
    org = getNumVal(a.v.hio);
    if (org < 0 || org > 1) error(sys_par,"#IO");
    ASSERT(newdims==m);
    if (isaChr(rex) && (newdims==0)) { /* HACK ATTACK!!! for no char scalars */
      newdims = 1;
      r.s.ldx.ip[0] = 1;
    }
    res    = allocv(vrtype(rex), newdims, r.s.ldx);
    noels  = nels(res);
    if (noels==0) return res;
    ASSERT(totinds=noels);
    argels = nels(rex);
    lex    = vadd(res);
    tex    = vadd(rex);
    /* initialise iref, oref and k */
    for (k=0,j=ldim;j>=0;j--) {
      if (r.s.indad.ip[j]==0) item = iref[j];
      else {
	item = r.s.indad.pt[j].np[iref[j]];
	item = (item>=0) ? item - org : rex.vr->vard[j] + item;
      }
      k += item * r.s.lds.ip[j];
      oref[j] = item;
    }
    i = 0;
    do {
      if (k < 0 || k >= argels) error(dom_range,"index out of bounds");
      switch (type(rex)) {
      case num: lex.np[i++] = tex.np[k]; break;
      case cpx: lex.cp[i++] = tex.cp[k]; break;
      case chr:
      case cym:
      case sym: lex.sp[i++] = tex.sp[k]; break;
      case ref: lex.pt[i++] = tex.pt[k]; break;
      default: error(sys_cor,"aref");
      }
      if (i==noels) break;
      for (m=ldim;m>=0;m--) { /* update iref, oref and k */
	iref[m]++;
	if (iref[m] < numinds[m]) {
	  if ((mex.it = r.s.indad.ip[m])) {
	    item = (mex.np[iref[m]] >= 0) ? mex.np[iref[m]] - org :
	                                    getDim(rex,m) + mex.np[iref[m]];
	  } else item = iref[m];
	  k += (item - oref[m])*r.s.lds.ip[m];
	  oref[m] = item;
	  break;
	} else {
	  iref[m] = 0;
	  if ((mex.it = r.s.indad.ip[m])) {
	    item = (mex.np[iref[m]] >= 0) ? mex.np[iref[m]] - org :
	                                    getDim(rex,m) + mex.np[iref[m]];
	  } else item = 0;
	  k += (item - oref[m])*r.s.lds.ip[m];
	  oref[m] = item;
	}
      }
    } while (true);
  }
  /* FIXME: avoid allocv for singleton ref, for now flip to content  */
  if (isRef(rex) && (dims(res) == 0)) return(res.vr->varadd.pt[0]);
  else  return(res); // sendres
} /* aref */

#else

static pointer aset(integer nargs, pointer parms) {
  integer ndims = 0, i, j, k, len, noels, pindex;
  pointer mat, val, lex, tex, p;
/*  variables for asetLoop: */
  pointer asetRes, asetRex;
  integer asetNdims, asetK, asetOrigin, item;

  void asetLoop(integer dims, integer index) {
    integer l;
    pointer tex;

    if (dims >=asetNdims) {
      register pointer lex = vadd(asetRes);
      register pointer rex = vadd(asetRex);
      if (index < 0  || index  >= nels(asetRes)) error(dom_range,"index");
      if (asetK >= nels(asetRex)) asetK = 0;
      switch (type(asetRes)) {
      case num: lex.np[index] = rex.np[asetK++]; return;
      case cpx: lex.cp[index] = 
	  (isCpx(asetRex)) ? rex.cp[asetK++] : rex.np[asetK++]; return;
      case chr: lex.sp[index] = rex.sp[asetK++]; return;
      case ref: lex.pt[index] =
	  isRef(asetRex) ? rex.pt[asetK++]:asetRex; return;
      default: error(sys_cor,"aset");
      }
    }
    tex.wh = r.s.indad.ip[asetNdims - dims - 1];
    if (isNil(tex)) {
      for (l = 0; l < getDim(asetRes,dims); l++) {
	asetLoop(dims + 1, index + l * r.s.lds.ip[dims]);
      }
      return;
    }
    for (l = 0; l < nels(tex); l++) {
      item = (vadd(tex).np[l] >= 0) ?
	(integer)vadd(tex).np[l] - asetOrigin :
	getDim(asetRes,dims) + (integer)vadd(tex).np[l];
      asetLoop(dims + 1, index + item *  r.s.lds.ip[dims]);
    }
  }
  
   /* Procedure aset; BEGIN */
  mat = arg1(parms);
  val = arg2(parms);
  lex = arg3(parms);
  if (isSymbol(mat)) { error(inv_funarg,"aset cannot modify symbol"); }  
  if (!isRef(mat) && (!isAtom(val) || !typeComp(mat,val))) {
    if (isCpx(mat) && isNum(val)) val = mkCpxFromNum(val);
    else error(inv_funarg,"aset arg types not compatible");
  }
  if (!isRef(mat) && (nels(val) == 0)) return val;
  if ((nargs == 3) && !isNum(lex) && !isNil(lex)) {
    if (isRef(lex)) {
      k     = nels(lex);
      tex   = vadd(lex);
      ndims = dims(mat);
      makeBlock(ndims, &r.s.indad); /* array pointing to index arguments */
      j     = ndims - 1;
      pindex = false;
      for (i=0;(i<k) && (j>=0);i++) {
	if (isColon(tex.pt[i])) {
	  if ((!pindex) || (i==k-1)) r.s.indad.pt[j--] = pnil;
	  else pindex = false;
	} else if (isNum(tex.pt[i])) {
	r.s.indad.pt[j--] = tex.pt[i];
	pindex = true;
	} 
	else error(inv_funarg,"non numeric index");
      }
      if (!pindex) {
	if (j == 0) r.s.indad.pt[j--] = pnil; /* last index was colon */
	else error(dom_range,"index");
      }
      ndims += (k - i); /* in case bailed out on j < 0 */
    } else error(inv_funarg,"invalid index type");
  } else {
      ndims = nargs - 2;
      if (ndims < 1) { error(inv_funarg,"aset res val sub1[,2,3..]"); }
      makeBlock(ndims, &r.s.indad);
      for (i = ndims - 1; i >= 0; i--) {
	p = argn(ndims - i + 1, parms);
	if (!isNum(p) && !isNil(p)) error(inv_funarg,"non numeric/nil index");
	r.s.indad.pt[i] = p;
      }
    }
  if (ndims != dims(mat)) error(dom_range,"shape mismatch");
  noels = 1;
  /*calculate dimension skip sizes */
  makeBlock(ndims, &r.s.lds);
  r.s.lds.ip[ndims - 1] = 1;  /* skip size for last dim is 1 */
  for (j = ndims - 1; j >= 1; j--)
    r.s.lds.ip[j - 1] = r.s.lds.ip[j] * getDim(mat,j);
  for (i=j=0; j < ndims; j++) {
    lex.wh = r.s.indad.wp[ndims - j - 1];
    len    = (isNil(lex)) ? getDim(mat,j) : nels(lex);
    while (i < dims(val) && getDim(val,i)==1) i++;
     if (len > 1) {
      if (!sgltn(val)) {
	if (i==dims(val)) error(dom_range,"value shape mismatch");
	if (len < getDim(val,i) || len > getDim(val,i))
	  error(dom_range,"value length mismatch");
      }
      i++;
    }
    noels *= len;
  }  /*for j */
  if (noels == 0) return val;
  asetOrigin = getNumVal(a.v.hio);
  if (asetOrigin < 0 || asetOrigin > 1) error(sys_par,"#IO");
  asetRes    = mat;
  asetRex    = val;
  asetNdims  = ndims;
  asetK      = 0;
  asetLoop(0, 0);
  return(val);
}

static pointer aref(integer nargs, pointer parms) {
  integer i, j, k, m,  ndims=0, newdims, ldim, item;
  pointer rex,  lex, tex;
  bool    pindex;
  /* variables for arefLoop */
  pointer aref_arg,aref_res;
  integer aref_ndims, aref_k, aref_origin;

  void arefLoop(integer dims, integer index) {
    integer l;
    pointer tex;
    
    if (dims >= aref_ndims) {
      register pointer lex = vadd(aref_res);
      register pointer rex = vadd(aref_arg);
      if (index  < 0 || index  >= nels(aref_arg)) error(dom_range,"index");
      if (aref_k < 0 || aref_k >= nels(aref_res)) error(pgmr_inc,"index");
      switch (type(aref_arg)) {
      case num: lex.np[aref_k++] = rex.np[index]; return;
      case cpx: lex.cp[aref_k++] = rex.cp[index]; return;
      case chr:
      case cym:
      case sym: lex.sp[aref_k++] = rex.sp[index]; return;
      case ref: lex.pt[aref_k++] = rex.pt[index]; return;
      default: error(sys_cor,"aref");
      }
    } /* NOT REACHED */
    tex.wh = r.s.indad.ip[aref_ndims - dims - 1];
    if (isNil(tex)) {
      for (l = 0; l < getDim(aref_arg,dims); l++) {
	arefLoop(dims + 1, index + l * r.s.lds.ip[dims]);
      }
    } else {
      for (l = 0; l < nels(tex); l++) { /* for each index val */
	item = (vadd(tex).np[l] >= 0) ?
	  (integer)vadd(tex).np[l] - aref_origin :
	  getDim(aref_arg,dims) + (integer)vadd(tex).np[l];
	arefLoop(dims + 1, index + item * r.s.lds.ip[dims]);
      }
    }
  }

/* Procedure aref; BEGIN */
  rex = arg1(parms);
  lex = arg2(parms);
  if ((nargs == 2) && !isNum(lex)) {
    if (isRef(lex)) {
      k     = nels(lex);
      tex   = vadd(lex);
      ndims = dims(rex);
      makeBlock(ndims, &r.s.indad); /* array pointing to index arguments */
      j = ndims - 1;
      pindex = false;
      for (i=0;(i<k) && (j>=0);i++) {
	if (isColon(tex.pt[i])) {
	  if ((!pindex) || (i==k-1)) r.s.indad.pt[j--] = pnil;
	  else pindex = false;
	} else if (isNum(tex.pt[i])) {
	  r.s.indad.pt[j--] = tex.pt[i];
	  pindex = true;
	}
	else error(inv_funarg,"non numeric index element");
      }
      if (!pindex) {
	if (j == 0) r.s.indad.pt[j--] = pnil; /* last index was colon */
	else error(dom_range,"index");
      }
      ndims += (k - i); /* in case bailed out on j < 0 */
    } else if (isNil(lex)) {
      ndims = 1;
      ldim  = 1;
      r.s.indad.pt[0] = pnil;
    } else error(inv_funarg,"invalid index type");
  ldim  = ndims - 1;             /* last dimension in argument        */
  } else {
    ndims = nargs - 1;             /* number of dimensions in argument  */
    ldim  = ndims - 1;             /* last dimension in argument        */
    makeBlock(ndims, &r.s.indad); /* array pointing to index arguments */
    for (i = ldim; i >= 0; i--) {
      tex = argn(ndims - i,parms);
      if (!isNum(tex) && !isNil(tex)) 
	error(inv_funarg,"non numeric / nil index element");
      r.s.indad.pt[i] = tex;
    }
  }
  if (ndims != dims(rex))  error(dom_range,"index");
  makeBlock(ndims, &r.s.lds);  /* index strides         */
  /*calculate size & shape of new element*/
  newdims = 0;
  r.s.lds.ip[ldim] = 1; /* last arg index stride is one */
  for (j = ldim; j >= 1; j--) r.s.lds.ip[j - 1] = r.s.lds.ip[j] * getDim(rex,j);
  for (j = 0; j < ndims; j++) { /* calculate newdims */
    lex = r.s.indad.pt[j];
    newdims += (!isNil(lex)) ? dims(lex) : 1;
  }
  makeBlock(newdims, &r.s.ldx);  /* dimensions for result */
  /* assign result dimensions */
  for (m=0,j = 1; j <= ndims; j++) {
    lex = r.s.indad.pt[ndims - j];
      if (!isNil(lex)) {   /* have actual index values for this rex dimension */
	/* add result dimensions as appropriate            */
	for (k=0; k<dims(lex); k++) r.s.ldx.ip[m++] = getDim(lex,k);
    } else { /* use all elements in this arg dimension in result */
      r.s.ldx.ip[m++]  = getDim(rex,j - 1);
    }
  }  /*for j */
  aref_k      = 0;
  aref_ndims  = ndims;
  aref_arg    = rex;
  aref_origin = getNumVal(a.v.hio);
  if (aref_origin < 0 || aref_origin > 1) error(sys_par,"#IO");
  ASSERT(newdims==m);
  if (isaChr(rex)) {
    if (newdims==0) { /* HACK ATTACK!!! for no char scalars */
      newdims = 1;
      r.s.ldx.ip[0] = 1;
    }
    /* code to avoid allocating chr singleton */
    if ((newdims == 1) &&  (r.s.ldx.ip[0] == 1)) { /* we have a singleton */
      aref_res = sstrtab[256]; /* dummy target */
      arefLoop(0, 0);
      return sstrtab[aref_res.vr->varadd.sp[0]];
    }
  }
  aref_res    =  allocv(vrtype(rex), newdims, r.s.ldx);
  arefLoop(0, 0);
  if (isRef(rex) && (dims(aref_res) == 0)) {
    tex = aref_res.vr->varadd.pt[0];
    retv(aref_res);
    return(tex);
  }
  return(aref_res); // sendres
} /* aref */
#endif


static pointer search(integer nargs, pointer parms) { /* string search */
  pointer lent,rent,result,res,start;
  integer i,j,k,m,pl,rs,si,sl,ss,id,adj,len,dimd,axis,blim,resl,item;
  char *s,*p;
  int c,delta1[256];
  
  lent = arg1(parms);
  rent = arg2(parms);
  dimd   = dims(rent);
  axis =  (nargs >= 3)  ? checkAxis(arg3(parms),dimd) : dimd-1;
  // TODO: factor out construction of result dims sans axis
  si = 1;  /* string increment */
  for (i=j=0; i < dimd; i++) {
    if (i != axis) {
      r.s.lds.ip[j++] = getDim(rent,i);
      if (i > axis)  si *= getDim(rent,i);
    }
  }
  dimd = (dimd <= 1) ? 0 : dimd - 1;
  result = allocv(num, dimd, r.s.lds);
  for (i=0;i<nels(result);i++) vadd(result).np[i] = 0.0;
  if (nargs == 4) {
    start = arg4(parms);
    if (!sameShape(start,result)) 
      error(dom_range,"(p start) == (p target/axis)"); // sorry for the garbage
  } else start = pnil;
  id   = (integer)getNumVal(a.v.hio);
  sl   = (dimd) ? rent.vr->vard[axis] : nels(rent); // target string len
  pl   = nels(lent);   /*ext vardim should be < 2*/
  p    = (char *)vadd(lent).sp;
  s    = (char *)vadd(rent).sp;
  res  = vadd(result);
  resl = nels(result);
  if (sl==0) return(result);
  if (pl==0) { 
    for (i=0;i<resl;i++) res.np[i] = id; 
    return(result);
  }
  /* build delta1 ( boyer moore fast string search algorithm) */
  for (j=0;j<256;j++) delta1[j] = -1;
  for (k=pl-1;k>=0;k--)
    if (delta1[(int)p[k]] < 0) delta1[(int)p[k]] = pl - k - 1;
  {
    int delta2[pl]; /* build delta2 (is this really worth it ?) */
    for (j=pl-1;j>=0;j--) { // index of rightmost partially matched substring
      // pat[j-1] does not match string corresponding char in string
      len = pl - j - 1;     // length of partially matched string
      if (len==0) k=j;
      else {
	for (k=j; k>=j-pl+1;k--) { // 1st index of candidate rpr (start at j?)
	  adj =  (k < 0) ? k : 0;
	  if ((memcmp(&p[k-adj],&p[j+1-adj],len+adj)==0) &&
	      ((k < 1) || (p[k-1] != p[j]))) break;
	}
      }
      delta2[j] = pl-k;
    }
    len  = sl * si;
    blim = nels(rent) / len;
    for (j = 0; j < blim; j++) { /* blim is the no of back_tracks of ss */
      for (k = 0; k < si; k++) {
	ss = len * j + k;  /* string start linear index   */
        rs = si  * j + k;  /* result linear index         */
	i  = pl - 1;  /* string axis dimension index */
	if (!isNil(start)) {
	  item = vadd(start).np[rs];
	  if (item <id) goto snofound; // don't bother
	  i += item + 1; // start from the next char
	}
      rcont:
	while (i < sl) {
	  m = pl - 1;
	  do {
	    c = s[ss+i*si]; /* character in string being looked at */
	    if (c==p[m]) {i--; m--;}
	    else {
	      i += max(delta1[c],delta2[m]);
	      goto rcont;
	    }
	  } while (m >=0);
	  res.np[rs] = id + i + 1;	/* found */
	  goto snext;
	}
      snofound:
	res.np[rs] = id - 1;    /* not found */
      snext: continue;
      }
    }
  }
  return(result);  // sendres
} /* search */

static pointer fourier(lispTypes op, integer nargs, pointer parms) {
  pointer rent,result,res,rex;
  integer i,j,k,l,m,dimd,lnd,noels,rep,isign,nxfrm,xfrm,base;
  number theta;
  complex tem,w,wt,wi;
  /* Hey Joe, what you doing with that gun in your hand ? */
  rent = arg1(parms);
  dimd  = dims(rent);
  if (dimd < 1) error(dom_range,"argument cannot be scalar");
  noels = getDim(rent,dimd-1);
  for (k = noels,i=0;k>1;k>>=1) i += k & 1;
  if (i != 0) error(dom_range,"size of last dim must be power of 2");
  /* Calculate the number of  transforms to perform */
  for (i=0,nxfrm=1;i<dimd-1;i++) nxfrm *= getDim(rent,i);
  /*let's do it*/
  result = allocv(cpx, dimd, pvard(rent));
  res    = vadd(result);
  rex    = vadd(rent);
  isign  = (op == s_four) ? 1 : -1; /* fwd / inv */
  /*move bit reversal of rex to res */
  rep = noels/2;
  {int ind[rep];
    if (isNum(rent)) {
      for (xfrm=0;xfrm<nxfrm;xfrm++) {
	base = xfrm*noels;
	k=rep;
	ind[0]=0;
 	res.cp[base] = rex.np[base];
	for (i=0,l=1; i< rep-1;l=i+1,k/=2) {
	  for (j = 0; j < l; j++) {
	    i++; m=ind[i] = ind[j]+k;
	    res.cp[base + i]     = rex.np[base + m];
	    res.cp[base + i+rep] = rex.np[base + m+1];
	  }
	}
	res.cp[base + rep] = rex.np[base + 1]; /* last element */
      }
    } else { /* must be complex */
      ASSERT(isCpx(rent));
      for (xfrm=0;xfrm<nxfrm;xfrm++) {
	base = xfrm*noels;
	k=rep;
	ind[0]=0;
	res.cp[base] = rex.cp[base];
	for (i=0,l=1; i< rep-1;l=i+1,k/=2) {
	  for (j = 0; j < l; j++) {
	    i++; m=ind[i] = ind[j]+k;
	    res.cp[base + i]     = rex.cp[base + m];
	    res.cp[base + i+rep] = rex.cp[base + m+1];
	  }
	}
	res.cp[base + rep] = rex.cp[base + 1]; /* last element */
      }
    }
  }
  /* res is correct order for mult with roots of unity */
  for (xfrm=0;xfrm<nxfrm;xfrm++) {
    base = xfrm*noels;
    for (k=1;k<noels;k=l) {
      theta = isign*pi/k;
      w     = -2.0*sqr(sin(theta/2)) + I*sin(theta);
      wt    = idprod;
      l     = k*2;
      for (m=0;m < k; m++) {
	for (j=m;j<noels;j+=l) { /* Daniel-Lanczos formula */
	  i = j+k;
	  tem        = wt*res.cp[base + i];
	  res.cp[base + i]  = res.cp[base + j] - tem;
	  res.cp[base + j] += tem;
	}
	wt = wt * w + wt;
      }
    }
    if (op==s_ifour) for (i=0;i<noels;i++) res.cp[base + i] /= noels;
  }
  return(result);  // sendres
} /* fourier */

static pointer incr(pointer lex) {
  pointer rex,sex,tex;
  integer i,lim;
  if (isNum(lex)) {
    rex = lex;
  } else {
    if (!isSym(lex)) error(inv_funarg,"incr");
    sex = slookup(lex); /* get var value slot */
    rex = *sex.pt;
    if (!rex.it) error(inv_funarg,"unbound symbol");
    if (!isNum(rex)) error(inv_funarg,"incr"); /* check for constant ? */
  }
  tex = allocv(num, dims(rex), pvard(rex));
  lim = nels(rex);
  rex = vadd(rex);
  for (i = 0; i < lim; i++) tex.vr->varadd.np[i] = rex.np[i] + idprod;
  if (isSym(lex)) {
    *sex.pt = tex;    /* set value of variable */
#if (BINDING==DEEPC)
    cacheSymVal(lex,tex);
#endif
  }
  return(tex);  // sendres
}

static pointer decr(pointer lex) {
  pointer rex,sex,tex;
  integer i,lim;
  if (!isSym(lex)) error(inv_funarg,"decr");
  sex = slookup(lex); /* get var value slot */
  rex = *sex.pt;
  if (!rex.it) error(inv_funarg,"unbound symbol");
  if (!isNum(rex)) error(inv_funarg,"decr"); /* check for constant ? */
  tex = allocv(num, dims(rex), pvard(rex));
  lim = nels(rex);
  rex = vadd(rex);
  for (i = 0; i < lim; i++) tex.vr->varadd.np[i] = rex.np[i] - idprod;
  *sex.pt = tex;    /* set value of variable */
#if (BINDING==DEEPC)
  cacheSymVal(lex,tex);
#endif
  return(tex); // sendres
}

static pointer alps_sqrt(pointer lex) {
  pointer res;
  integer i,lim;
  vtype restyp = type(lex);
  if (!isaNum(lex)) error(inv_funarg,"sqrt");
  lim = nels(lex);
  if (restyp==num) {
    for (i=0;i<lim;i++) if (vadd(lex).np[i] < 0) {restyp=cpx;break;}
  }
  res = allocv(restyp, dims(lex), pvard(lex));
  if (restyp==num) {
    for (i = 0; i < lim; i++) res.vr->varadd.np[i] = sqrt(vadd(lex).np[i]);
  } else if (isNum(lex)) {
    for (i = 0; i < lim; i++) res.vr->varadd.cp[i] = csqrt(vadd(lex).np[i]);
  } else {
    for (i = 0; i < lim; i++) res.vr->varadd.cp[i] = csqrt(vadd(lex).cp[i]);
  }
  return(res); // sendres
}


static pointer alpsInt(pointer lex) {
  pointer res,rex,mex;
  integer i,lim;

  if (!isaNum(lex)) error(inv_funarg,"needs numeric argument");
  lim = nels(lex);
  res = allocv(num, dims(lex), pvard(lex));
  rex = vadd(res);
  mex = vadd(lex);
  if (isNum(lex)) {
    for (i = 0; i < lim; i++) rex.np[i] = trunc(mex.np[i]);
  } else {
    for (i = 0; i < lim; i++) rex.np[i] = trunc(creal(mex.cp[i]));
  }
  return res; // sendres
}
static pointer alpsFrc(pointer lex) {
  pointer res,rex,mex;
  integer i,lim;
  number w;

  if (!isaNum(lex)) error(inv_funarg,"needs numeric argument");
  lim = nels(lex);
  res = allocv(num, dims(lex), pvard(lex));
  rex = vadd(res);
  mex = vadd(lex);
  if (isNum(lex)) {
    for (i = 0; i < lim; i++) rex.np[i] = modf(mex.np[i],&w);
  } else {
    for (i = 0; i < lim; i++) rex.np[i] = modf(creal(mex.cp[i]),&w);
  }
  return res; // sendres
}

static inline bool isInt(number n) {number w; return (modf(n,&w) == 0);} 

static pointer alpsIsInt(pointer lex) {
  pointer res,rex,mex;
  integer i,lim;

  if (!isaNum(lex)) error(inv_funarg,"needs numeric argument");
  lim = nels(lex);
  res = allocv(num, dims(lex), pvard(lex));
  rex = vadd(res);
  mex = vadd(lex);
  if (isNum(lex)) {
    for (i = 0; i < lim; i++) rex.np[i] = isInt(mex.np[i]) ? idprod : idadd;
  } else {
    for (i = 0; i < lim; i++)
      rex.np[i] = (isInt(creal(mex.cp[i])) && (cimag(mex.np[i]) == idadd)) ?
	idprod : idadd;
  }
  return res; // sendres
}

static number modExp (number b, unsigned long long n, 
		      unsigned long long k) { // (b^n) mod k
  number res = 1.0;
  while (n > 0) {
    if (n & 1) res = fmod(res*b,k);
    n = n >> 1;
    b = fmod(sqr(b),k);
  }
  return res;
}

static pointer alps_modExp(integer nargs, pointer parms) {
  pointer resp,basep, exponentp, modulusp,expv;
  number  base, modulus;
  integer i,lim;

  basep     = arg1(parms);
  exponentp = arg2(parms);
  modulusp  = arg3(parms);
  base      = getNum(basep);
  modulus   = getNum(modulusp);
  if (!isInt(base) || !isInt(modulus)) 
    error(inv_funarg,"modexp needs integral base and modulus");
  resp = allocv(num, dims(exponentp), pvard(exponentp));
  lim  = nels(exponentp);
  expv = vadd(exponentp);
  for (i = 0; i < lim; i++) 
    resp.vr->varadd.np[i] = modExp(base, expv.np[i], modulus);
  return(resp); // sendres
}

static pointer tally(pointer lex) {
  number n;
  if (dims(lex)==0) return(getVal(a.v.hmi));
  if (nels(lex)==0) return(getVal(a.v.hai));
  return mkNum((number)getDim(lex,0));
}

/*a brief respite:*/

/*  ************************* LISP SECTION *********************  */

#if (BINDING==SHALLOW)
static void swap(pointer e) {
  integer i,lim;
  pointer tex;
  lim = nargs(e);
  for (i = 0; i < lim; i++) {
    tex                        = e.ev->args[i].dr;
    e.ev->args[i].dr           = e.ev->args[i].ar.at->value;
    e.ev->args[i].ar.at->value = tex;
  }
}
#else
#define swap(X)
#endif

static pointer find_active_env(pointer env) { 
  /* find first active env in hades */
  while (!isNil(env) && !isActive(env)) env = nenv(env);
#if ((SYSTRACE & ENVONLY) && DEBUG)
  alpsOutsn(alpserr,3,"find_active_env => ",itox(env.wh),"\n");
#endif
  return(env);
}

static void reserveChain(pointer env) {
  while (!isNil(env) && !isRsvd(env)) { reserve(env); env = nenv(env); }
}

static integer envDepth(pointer env) {
  integer depth;
  for (depth=0;!isNil(env);depth++) env = nenv(env);
  return(depth);
}

#if (BINDING==SHALLOW)
static void swap_down(pointer fenv, pointer tenv) {
  pointer tmp;
  integer i,lim;

#if (SYSTRACE & ENVONLY)
  alpsOutsn(alpserr,5,"swap_down from ",itox(fenv.wh),
	    " => ",itox(tenv.wh),":\n");
#endif
  while (!isEqq(fenv,tenv)) {
    ASSERT(isEnv(fenv));
    lim = (fenv.ev->argp.wh) ? warg(fenv) : nargs(fenv);/*partially bound env?*/
#if ((SYSTRACE & ENVONLY) && DEBUG)
    alpsOutsn(alpserr,9,"(nargs: ",itos(nargs(fenv))," actlim: ",itos(lim),
	      " [",isActive(fenv) ? "a" : "i","] @", itox(fenv.wh),")\n");
#endif
    for (i = lim-1; i >=0; i--) { /* backwards in case of duplicate symbols */
      tmp                           = fenv.ev->args[i].ar.at->value;
      fenv.ev->args[i].ar.at->value = fenv.ev->args[i].dr;
      fenv.ev->args[i].dr           = tmp;
    } /* lim */
    setInactive(fenv);
    fenv = nenv(fenv);
  }
#if (SYSTRACE & ENVONLY)
alpsOutln(alpserr);
#endif
}

static void swap_up(pointer fenv, pointer tenv) {
  pointer rex,orex,trex,tmp;
  integer i,lim;
  rex = tenv;   /* start at the top for downhill run */
#if (SYSTRACE & ENVONLY)
  alpsOutsn(alpserr,5,"swap_up     to ",itox(tenv.wh),
	    " <= ",itox(tenv.wh),"\n");
#endif
  if (isEqq(rex,fenv))  return;
  orex         = nenv(rex);
  while (!isEqq(orex,fenv)) { /* turn the env chain upside down */
    trex = nenv(orex);
    snenv(orex,rex);           /* reverse link */
    rex  = orex;
    orex = trex;
  }
  /* orex==fenv initial value for uphill trek */
  while (!isEqq(orex,tenv)) {
    lim = (rex.ev->argp.wh) ? warg(rex) : nargs(rex); /* partially bound env? */
#if ((SYSTRACE & ENVONLY) && DEBUG)
    alpsOutsn(alpserr,7,"(nargs: ",itos(nargs(rex))," actlim: ",itos(lim),
	      isActive(rex) ? " [a] @" : " [i] @", itox(rex.wh),")\n");
#endif
    setActive(rex);
    for (i = 0; i < lim; i++) {
      tmp                          = rex.ev->args[i].ar.at->value;
      rex.ev->args[i].ar.at->value = rex.ev->args[i].dr;
      rex.ev->args[i].dr           = tmp;
    }
    trex = rex;
    rex  = nenv(rex);   /* back up the reverse link   */
    snenv(trex,orex);   /* reverse the reverse link   */
    orex = trex;        /* orex initial value is fenv */
  }
#if (SYSTRACE & ENVONLY)
    alpsOutln(alpserr);
#endif
}

static void restoreEnv(pointer oldenv, pointer newenv) {
  pointer lex;
  if (isNil(newenv) || isActive(newenv)) { /* new env still bound */
    swap_down(oldenv, newenv);   /* visit Persiphone */
  } else {
    lex = find_active_env(newenv); /* find intersection with active chain  */
    swap_down(oldenv,lex); /* unbind active chain to point of intersection */
    swap_up(lex,newenv);   /* restore ancient bindings of new candidate    */
  }
}
#else  // deep binding
#define swap_down(x,y)
#define swap_up(x,y)
#define restoreEnv(x,y)
#endif

/* called only from bigEval and runTcb */
static void crank_down(pointer from, pointer to) {
  pointer p1,p2;
  ASSERT(from.wh >= to.wh);
  for (p1 = from; !isEqq(p1,to); p1.wp--) {
    if (*p1.wp == s_trailer) { /* restore dest */
      dest = *--p1.pt;
      p1.wp--;
      ASSERT(isEqq(*p1.pt,dest.ev->argp)); // just checking
    }
    if (*p1.wp == s_eval1e) { /* must be compatible with s_eval1e!          */
      p1.wp -= 2;             /* skip opcd and saved new env                */
      p2 = *p1.pt--;          /* point of intersection with active chain    */
      swap_down(envp,p2);     /* swap down to it                            */
      setEnv(*p1.pt);         /* old env going into the eval                */
      swap_up(p2,envp);       /* restore to state when eval/funarg invoked  */
    } else if (*p1.wp == s_sys_eval1 || *p1.wp == s_teval1) evald--;
  }
}

static void crank_up(pointer from, pointer to) {
  pointer p1,p2,p3;
  ASSERT(from.wh <= to.wh);
  for (p1 = from; !isEqq(p1,to); p1.wp++) {
    if (*p1.wp == s_trailer) { /* restore dest */
      dest = p1.pt[-1];
      ASSERT(p1.pt[-2].pt == dest.ev->argp.pt); // just checking
    }
    if (*p1.wp == s_eval1e) {    /* must be compatible with s_eval1e!       */
      p2 = p1.pt[-1];            /* new env                                 */
      p3 = find_active_env(p2);  /* find where we meet current active chain */
      if (isEqq(p3,p1.pt[-2])) { /* same as saved POI  ?                    */
	swap_down(envp,p3);      /* there are bindings to be undone         */
	swap_up(p3,p2);          /* activate bindings of saved closure      */
	setEnv(p2);              /* new top of chain                        */
      } /* if POI not the same p2 will anyway be on the path from above     */
    } else if (*p1.wp == s_sys_eval1 || *p1.wp == s_teval1) evald++;
  }
}

static pointer findCtl(tcbTp t,integer k, lispTypes event) {
  /* search down t's stack looking for the kth occurence of event */
  integer i=0;
  pointer lex = stkp;
  if (k < 0) return pnil; /* not found */
  while (lex.pt != t->base.pt) {
    if (*lex.wp == event) {
      if (i == k) return lex;
      else i++;
    }
    lex.wp--;
  }
  return pnil;
}

static pointer findLastCtl(tcbTp t,integer *k, lispTypes event) {
  /* search down t's stack looking for last occurence of event */
  integer i=0;
  pointer lex,rex = pnil;
  for (lex=stkp; !isEqq(lex,t->base); lex.wp--) {
    if (*lex.wp == event) { i++; rex = lex; }
  }
  *k = i-1;
  return rex;
}

static pointer ldest;   /* last destination for debug output */

static void restore_last_eval_frame(tcbTp t, pointer stk) {
  ASSERT(*stk.wp==s_teval1);
  stk.pt--;            /* decrement stk */
  t->iexp = *stk.pt--;
  t->dest = *stk.pt--;
  t->envp = *stk.pt--; /* this is the to environment         */
  setTcbStack(t,stk);  /* move stack here  */
}

static dispstate bigEval(const tcbTp t) { /* Mr. Big */
  //  pointer iexp,parm,stkp,envp;
  //  lispTypes opcd;
  //  integer evald;
  pointer p1,p2,p3,p4,p5;
  integer i, j, len, lnd, tchar,temi;
  lispTypes topcd;
  fibrec *inp,*out=cout();
  number ntemp;
  dispstate disposition;
  char bpb[80];

/* control stack management and access (watch out for hardcoded variants)*/
#if (FAST)
#define CHECKST
#else
#if (DEBUG)
#define CHECKST if (stkp.wh >= t->slim.wh)  {			    \
  alpsMsg(5,"stk_ovfl at depth ",itos((stkp.wh-t->base.wh)/indlen), \
	  " in ",evtab[opcd].lispName,"\n");			    \
        error(stk_ovfl," recovering");                              \
  }
#else
#define CHECKST if (stkp.wh >= t->slim.wh)  error(stk_ovfl," recovering");
#endif
#endif

  inline void goreto(lispTypes go, lispTypes ret) {
    CHECKST;
    *++stkp.wp = ret;
    opcd       = go;
  }

  inline  void stkret(lispTypes lab) {
    CHECKST;
    *++stkp.wp = lab;
  }

  inline  lispTypes retstk() { return((lispTypes)*stkp.wp--); }

  inline  void save(pointer lex) {
    CHECKST;
    *++stkp.pt = lex;
  }

  inline void saveF(pointer lex) {
    if (!isaFil(lex)) error(inv_funarg,"expected <file object>");
    save(lex);
  }

  inline  void save2(pointer *ref, pointer nval) {
    CHECKST;
    *++stkp.pt = *ref;
    *ref = nval;
  }

  inline pointer restore()                 {return *stkp.pt--; }
  inline pointer getTos ()                 {return *stkp.pt;   }
  inline void    setTos (pointer p)        {*stkp.pt = p; }
  inline pointer getStk (int i)            {return  stkp.pt[i];}
  inline void    setStk (int i, pointer p) {stkp.pt[i] = p;}
  inline void    pushStk(int n) {
#if (!FAST)
    if ((stkp.wh + (n*indlen)) > t->slim.wh) error(stk_ovfl," recovering");
#endif
    stkp.pt += n;
  }
  inline void    popStk (int n)            {stkp.pt -= n;}
  inline void    pop1   ()                 {stkp.pt--;}
  inline void    pop2   ()                 {stkp.pt -= 2;}

/* specials */
  inline  void saveDest() {


    /*
stkp +--------------+
  2  |   s_trailer  |  0
     +--------------+
  1  |     dest     | -1
     +--------------+
  0  |dest.ev->argp | -2
     +--------------+

    */

#if (MMDBUG==High)
    checkDenv(dest);
#endif
    save(dest.ev->argp);
    save(dest);
    stkret(s_trailer); // special marker
  }

  inline  void restoreDest() {
    lispTypes m;
    m              = retstk();
    ASSERT(s_trailer == m);
    dest           = restore();
#if (MMDBUG==High)
    checkDenv(dest);
#endif
    dest.ev->argp  = restore();
  }

  inline void saveCtx() { /* save global context on stack */
    save(iexp);
    save(parm);
    save(envp);
    saveDest();
  }

  inline void restoreCtx() { /* restore global context from stack */
    restoreDest();
    setEnv(restore());
    parm = restore();
    iexp = restore();
  }

  pointer saveStack() { /* make a copy of current stack */
    pointer lex;
    integer len;
    tcbFlush(t);
    len  = stkSize(t->cntptr.ct);
    lex  = allocCnt(len,0);
    memcpy(lex.ct->stack.pt,t->cntptr.ct->stack.pt,len); /* tos included  */
    lex.ct->ctlsp.wh = lex.ct->stack.wh + len - indlen;  /* points to tos */
    ASSERT((t->cntptr.ct->stack.wh + len - indlen) == stkp.wh);
    lex.ct->evaldb = evald; /* save depth */
    return lex;
  }

  void restoreStack(pointer lex) { /* copy back saved stack */
    integer len;
    len = stkSize(lex.ct);
    ASSERT(t->cntptr.ct->stksiz >= len);
    memcpy(t->cntptr.ct->stack.pt,lex.ct->stack.pt,len);
    /* yes virginia that was an expensive trick */
    stkp.wh =  t->cntptr.ct->stack.wh + len - indlen; /* must point to tos */
    /* base and slim unchanged */
    evald   = lex.ct->evaldb; /* restore depth   */
  }

  /* Destination handling */
  inline pointer getRes  ()           { return *dest.ev->argp.pt; }
  inline void    sendRes(pointer lex) { *dest.ev->argp.pt = lex;  }
 /* parm setup completed || uninitialised dest kills*/
  inline  void zap_dest() {dest.ev->argp.pt = 0; }

  inline  void save_res   () {save(getRes());}
  inline  void restore_res() {*dest.ev->argp.pt = *stkp.pt--;}

  inline  pointer setDest(pointer lex) { return dest.ev->argp = lex; }
  inline  pointer getDest()            { return dest.ev->argp; }
  
  inline  void saveStoreDest(pointer lex) { save(dest.ev->argp); setDest(lex); }

  inline  void next_dest() {
    dest.ev->argp.rg++;
#if (RANGE)
    whole alp =  (dest.wh + (env_rgohd + dest.ev->nargs)*rg_len);
    if (dest.ev->argp.wh >= alp) error(inv_funarg,"arg overflow");
#endif
    sendRes(pnil); /* boogey will be marking him */
  }

  inline  void bumpDest() { dest.ev->argp.pt++;}

  inline  pointer fstep_dest() { return setDest(dest.ev->argp.rg->dr);}

#if (BINDING==SHALLOW)
  inline  void swap_dest() {
    pointer  lex,tex;
    lex                  = dest.ev->argp;
    lex.pt--;
    tex                  = lex.rg->ar.at->value;
    lex.rg->ar.at->value = lex.rg->dr;
    lex.rg->dr           = tex;
  }
#else
#define swap_dest()
#endif

  inline void sendToSymSlot(pointer lex) { 
    pointer tex = dest.ev->argp;    /* Send to symbol slot of curent dest */
    *--tex.pt   = lex;              /* symbol slot 8-(                    */
  }

/* environmental handling */

  inline void alps_link(pointer lex) {
    ASSERT(isEnv(lex));
    snenv(lex,envp);
    setEnv(lex);
    setActive(lex);
#ifdef LINKTRACE
    alpsOutsn(alpserr,5,"link ",itox(lex)," in ",evtab[pc].lispName,"\n");
#endif
  }

  inline void alps_unlink() {
    ASSERT(isEnv(envp));
    setInactive(envp);
#ifdef LINKTRACE
    alpsOutsn(alpserr,5,"unlink ",itox(envp)," in ",evtab[pc].lispName,"\n");
#endif
    setEnv(nenv(envp));
    ASSERT(isNil(envp) || isEnv(envp));
  }

  void alps_bind(pointer symbols, pointer e) {
    integer i,lim;
    pointer tex;
   
    lim = nargs(e);
#if (BINDING==DEEPC)
    envcnt++;  /* need to crank here so that cacheVal refers to this binding */
#endif
    for (i = 0; (i < lim) && !isNil(symbols); i++) {
      e.ev->args[i].ar = symbols.rg->ar;
#if (BINDING==DEEPC)
      cacheSymVal(symbols.rg->ar,e.ev->args[i].dr);
#endif
      symbols          = symbols.rg->dr;
    }
    if ((i < lim) || !isNil(symbols)) {
      //    dprtln(alpserr,symbols,72);
      error(inv_numarg, mkEmes(4,"function expects ",itos(i+listLen(symbols)),
			       " but got ",itos(lim)));
    }
#if (BINDING==SHALLOW)
#if (SYSTRACE & BINDONLY)
    alpsOutsn(alpserr,7,"bind in ",evtab[opcd].lispName," ",
	      itos(lim)," @",itox(e.wh),"\n");
#endif
    for (i = 0; i < lim; i++) {
#if (SYSTRACE & BINDONLY)
      dprt(alpserr,e.ev->args[i].ar,20); 
      alpsOutc(alpserr,'}'); dprtln(alpserr,e.ev->args[i].dr,20);
#endif
      tex                        = e.ev->args[i].dr;
      e.ev->args[i].dr           = e.ev->args[i].ar.at->value;
      e.ev->args[i].ar.at->value = tex;
    }
#endif //BINDING==SHALLOW
    /* inline link */
    snenv(e,envp);
    envp = e;
    setActive(e);
  }
 
  void alps_bind1(pointer sym, pointer val) {
    pointer lex = allocEnv(1);  /* binding with argp==0           */
    ssym1(lex,sym);             /* send symbol                    */
#if (BINDING==SHALLOW)
    sarg1(lex,getVal(sym));     /* save value                     */
    setVal(sym,val);            /* set symbol value               */
#else
    sarg1(lex,val);             /* set symbol value               */
    envcnt++;                   /* must occur before cache op     */
    cacheSymVal(sym,val);
#endif
    /* link to active environment  - inline */
    snenv(lex,envp);  
    envp = lex;
    setActive(lex);
  }

  void unbind() {
    pointer lex = envp;
  
#if (BINDING==SHALLOW)
    pointer tex;
    integer i,j;
    ASSERT(isEnv(lex));
    integer lim = nargs(lex);
#if (SYSTRACE & BINDONLY)
    alpsOutsn(alpserr,7,"unbind in",evtab[opcd].lispName," " ,
	      itos(lim + 1)," @",itox(lex.wh),"\n");
#endif
    if (lex.ev->argp.wh) { /* partial bind: calc actual lim */
      lim = warg(lex);
      alpsOutsn(alpserr,5,"unbind(): partial bind lim was ",
		itos(nargs(lex))," => ",itos(lim),":\n");
    }
    for (i = lim -1 ; i >= 0; i--) { /* Backwards for duplicate symbols */
      tex = lex.ev->args[i].ar.at->value;
      lex.ev->args[i].ar.at->value = lex.ev->args[i].dr;
      lex.ev->args[i].dr = tex;
#if (SYSTRACE & BINDONLY)
      dprt(alpserr,lex.ev->args[i].ar,20); alpsOutc(alpserr,'}');
      dprtln(alpserr,lex.ev->args[i].dr,20);
#endif
    } /* for i */
#endif /* SHALLOW */
    alps_unlink();
    if (!isBusy(lex)) {
      INCCT(envct,2);
      retEnv(lex);
    }
  }

  void gypo_bind(pointer args, pointer env) {
    integer i;
    ASSERT(nargs(args)==nargs(env)); 
#if (BINDING==SHALLOW)
    for (i=0;i<nargs(args);i++) setVal(symn(i,env),argn(i,args));
#else
    for (i=0;i<nargs(args);i++) {
      sargn(env,i,argn(i,args));
      cacheSymVal(symn(i,env),argn(i,args));
    }
#endif
    if (!isBusy(args)) {
      INCCT(envct,3);
      retEnv(args);
    }
  }

  void abind(pointer alist) {
    /* bind from assoc list */
    pointer tenv, symbol,value;
    integer i,len = listLen(alist);

    ASSERT(isList(alist));
    tenv = allocEnv(len);
#if (BINDING==DEEPC)
    envcnt++;  /* need to crank here so that cacheVal refers to this binding */
#endif  
    for (i=0;!isNil(alist);alist = alist.rg->dr,i++) {
      tenv.ev->args[i].ar = alist.rg->ar.rg->ar;
      tenv.ev->args[i].dr = alist.rg->ar.rg->dr;
    }
#if (BINDING!=DEEP)
    for (i=(len-1);i>=0;i--) { /* got to go backwards for SHALLOW */
      symbol              = tenv.ev->args[i].ar;
      value               = tenv.ev->args[i].dr;
#if (BINDING==SHALLOW)
      tenv.ev->args[i].dr = symbol.at->value;    /* save current binding */
      symbol.at->value    = value;               /* new binding          */
#else
      cacheSymVal(symbol, value);
#endif
      
    }
#endif
    /* link to active environment  - inline */
    snenv(tenv,envp);  
    envp = tenv;
    setActive(tenv);
  }

  void unbinda(pointer alist) {
    /* opportunity for good compilers to show their stuff */
    pointer lex,symbol;
    integer i,lim;

    lex = envp;
    lim = nargs(lex);
    /* if the guys want to shunt alist during eval we'll need to restore
       symbols by assoc'ing them in alist. For now we assume order & number
       remain intact i.e. alist's virginity has been preserved. */

    ASSERT(lim==listLen(alist));
    i = 0;
    while (!isNil(alist)) {
      symbol = alist.rg->ar.rg->ar;
      ASSERT(symbol.pt == lex.ev->args[i].ar.pt);    /* same name ?          */
#if (BINDING==SHALLOW)
      alist.rg->ar.rg->dr = symbol.at->value;        /* OOOH! patching alist */
      symbol.at->value    = lex.ev->args[i].dr;      /* restore old val      */
#else
      alist.rg->ar.rg->dr = lex.ev->args[i].dr;      /* restore old val      */
#endif
      alist               = alist.rg->dr;
      i++;
    }
    alps_unlink();
  }

 void map_error(errors erc, lispTypes op) {
   error(erc,mkEmes(3,"(",evtab[op].lispName, " F <list>+)"));
 }

 pointer mkApplyEnvParm(pointer p1) {
   integer ilen,slen;
   pointer lex,res;
   /* as usual p1 is original (s_apply) parm block */
   ilen = nargs(p1)-2;
   lex  = argn(ilen+1,p1); /* last apply arg is list */
   slen = listLen(lex);
   res  = allocEnv(ilen + slen); /* all args minus first and last */
   for (i=0;i<ilen;i++) sargn(res,i,argn(i+1,p1));
   for (j=0;j<slen;j++,lex = cdr(lex)) sargn(res,i++,car(lex));
   return res;
 } 

 pointer mkApplyListParm(pointer p1) { 
   integer ilen;
   pointer lex,mex,rex,res;
   ilen = nargs(p1)-2;             /* as usual p1 is original  parm block */
   lex  = argn(ilen+1,p1);         /* last apply arg is list              */
   if (ilen == 0) res = lex;       /* last arg is only arg                */
   else {
     res  = getfs(ilen);           /* all except first and last           */
     for (i=0,rex=res;i<ilen;i++,mex=rex,rex=cdr(rex)) 
       rex.rg->ar = argn(i+1,p1);  /* copy args into list                 */
     mex.rg->dr = lex;             /* nconc list from last arg            */
   }
     return res;
}

/**********************************/
/*** Procedure bigEval(tcbTp);  ***/
/*** BEGIN                      ***/
/**********************************/

 /* one way in */
  goto alcnt; /* by default continue */

 alret:  opcd = retstk();  /*return*/

 alcnt:   /*continue*/

  ASSERT(opcd<s_trailer);
  
#if (TDEBUG)
   if (rtflgs) {
     if (isfSet(ctlFlag) && isfClr(dbgFlag)) {
       fClr(ctlFlag);
       error(user_int, NULL);
     }
     if (isfSet(trcFlag)) {
       if (isfSet(dbgFlag)) {
	if (!isEqq(dest.ev->argp,ldest)) {
	  ldest = dest.ev->argp;
	  alpsOutln(alpserr);
	  alpsOutsn(alpserr,6,evtab[(integer)opcd].lispName,":",
		    itox(dest.wh)," ",itos(warg(dest))," ");
	} else {
	  if (opcd!=s_ratx  && opcd!=s_rhead && 
	      opcd!=s_rtail && opcd!=s_rhaux) {
	    alpsOuts(alpserr,evtab[(integer)opcd].lispName); 
	    alpsOutc(alpserr,' ');
	  }
	}
      }
      if (opcd == eval) {
	alpsOutln(alpserr);
	alpsOutsn(alpserr,2,itos(evald)," eval:");
	dprtln(alpserr,iexp,72);
      } else
	if (opcd == eval1) {
	  alpsOutln(alpserr);
	  alpsOutsn(alpserr,2,itos(evald-1),"=>");
	  dprtln(alpserr,getRes(),72);
	}
      alpsFlush(alpserr);
    }
  }
#endif

#if (PREEMPT)
   if (isfSet(pmtFlag))  {
     fClr(pmtFlag);
     /* if we are pre-emptible slink out the back door */
     if (runtcb.tb->priority > TCB_SYS_PRI) goreto(s_dpret,opcd); 
  }
#endif

#if (STATS)
  starr[opcd]++;
#endif

#if (DEBUG)
  iexctr++;
  runtcb.tb->pcring[runtcb.tb->pcrind++] = opcd; /* trace ring */
  if (runtcb.tb->pcrind == BB_PCDEPTH) runtcb.tb->pcrind = 0;
  //if (
      //opcd==s_eval0     || 
  //opcd==s_sys_eval  || 
      //opcd==s_evargs    ||
      //opcd==s_eva_do    ||
      //opcd==s_eva_end   ||
      //opcd==s_sys_apply ||
      //opcd==s_app_ret0  ||
      //opcd==s_app_ret1  ||
      //opcd==s_app_ret2  ||
      //opcd==s_iset      ||
      //opcd==s_apply     || 
      //opcd==s_apply1    ||
      //opcd==s_ratx    ||
      //opcd==s_ratx1   ||
      //opcd==s_dqaux   ||
      //opcd==s_trailer
      //) {
  //alpsOutsn(alpsout,3,"=>s_",evtab[(integer)opcd].lispName,"\n");   
    //dprtln(alpsout,iexp,40);  
    //alpsOuts(alpsout,"; parm="); 
    //if (isEnv(parm)) showEnv(alpsout,parm); else dprtln(alpsout,parm,20); 
    //alpsOuts(alpsout,"; dest="); showEnv(alpsout,dest); 
    //tcbFlush(t); showCtl(alpsout,t->cntptr); 
    //alpsFlush(alpsout);
  //}
#endif

  switch (opcd) {

  case s_con:    error(inv_fun,"const");          break;
  case s_quote:  sendRes(parm.rg->ar);            goto alret;

  case s_ratom:
    if (nargs(parm)==1) saveF(arg1(parm)); /* check arg & save on stack     */
    else save(lookup(a.v.hrf)); /* save current input filfob                */
    p1 = restore();          /* it is the filfob for the input file         */
    inp = getFil(p1);
    inp->qqdepth = 0;        /* quasi quote depth                           */
    p2 = getfs(1);           /* allocate a dummy FS a dest                  */
    sendRes(p2);             /* for saving in saveDest()                    */
    saveDest();              /* with trailer and all                        */
    dest = allocDest(0);     /* snark avoidance trick                       */
    setDest(p2);             /* initial dest is car of p2                   */
    stkret(s_rendres);       /* remember to restore dest  and post result   */
    save(p1);                /* fobbit for the rat & co                     */
    stkret(s_rend);          /* remember to drop the fob                    */
    stkret(s_nop);
    /* drop through */  

  case s_ratx:
    p1 = getStk(-2);
    if(!isaFil(p1)) {
      alpsOuts(alpserr," ratx stack confusion\n");
      tcbFlush(t);
      showCtl(alpserr,t->cntptr);
      ASSERT(false); /* crash */
    }
    inp = getFil(p1);      /* by arrangement  */
    chow_blanks(inp,0);    /* get to the meat */
    if (inp->bufstat == ldvs_eof) { /* got eof */
      sendRes(pnil);
      if (inp == alpsin) { /* was it from primary input file*/
        chuck_line(inp);   /* yes: hurl rest of line into bit bucket */
	opcd = s_exit;     /* go away */
      } else opcd = retstk();
      goto alcnt;
    } 
    if (inp->bufstat==ldvs_nodata) {
      save(inp->fob);       /* scheduler expects this          */
      disposition = winp;   /* reschedule me my file is on tos */
      goto alout;
    }
    ASSERT(inp->bufstat==ldvs_ok);
    opcd = s_ratx0;
    // drop through
    
#define CHECKBUF \
    if ((inp->fob.fb->type == filt) && (inp->ilpos>=inp->ecci)) { \
      putbackChar(inp,tchar);	     \
      save(inp->fob);		     \
      disposition = winp;            \
      goto alout; }

  case s_ratx0:
    inp = getFil(getStk(-2));
    temi = nextChar(inp);
    if (isDigit(temi)) {
      resetFilNState(inp);
      inp->alen = 0;
      inp->radix = (int)getNumVal(a.v.hib);
      if (temi == '0') {
	tchar = getChar(inp);
	CHECKBUF;
	inp->aline[inp->alen++] = '0';
	temi = nextChar(inp);
	if (temi == 'x') {
	  getChar(inp);
	  inp->radix = 16;
	  inp->aline[inp->alen++] = 'x';
	  inp->nstart = 2;
	} else {
	  inp->ndigs[inp->nind]++; // putbackChar(inp, tchar);
	}
      }
      opcd = s_numaux;
      goto alcnt;
    }
  ratx1:
    opcd = s_ratx1;
    // drop through 

  case s_ratx1:
    inp = getFil(getStk(-2));
    p1  = pnil;  // norav
    tchar = getChar(inp);
    switch (tchar) {
    case dollar:   
      CHECKBUF;
      temi = nextChar(inp);
      if (temi!=lsbrc && temi!=lcbrc) { sendRes(chartab[dollar]); goto alret; }
      p1    = ptru;
      tchar = getChar(inp); // consume and drop through
    case lsbrc: 
    case lcbrc: 
      save(p1);       // noravel option for implode
      save(inp->fob); // Set things up for going into ratx again
      stkret(s_nop); 
      goreto(s_ratx,(tchar==lsbrc) ? s_rvhead : s_revhd);   
      break;
    case lpar:    
    case rpar:    
    case rsbrc:   
    case rcbrc:   
    case colon:   sendRes(chartab[tchar]);   goto alret;
    case comment: goreto(s_chuckle,s_ratx);  goto alcnt; 
    case dquot:
      save(pnil); // list of buffers for res
      inp->alen = 0;
      opcd = s_dqaux;
      break;
    case excape:
      chuck_line(inp);
      error(inv_sym,"Unexpected escape character");
      break;
    case quote:  
      save(inp->fob);  /* dup'ed here popped in rqaux */
      stkret(s_rqaux); 
      goreto(s_ratx, s_rhead);
      break;
    case qquot: // create quasi quote tag list
    case comma:
      if (tchar == qquot)      { 
	p1 = s[s_qquot]; 
	inp->qqdepth++;
      } else {
	CHECKBUF;
	if (nextChar(inp) != spiral)        p1 = chartab[comma];
	else         { temi = getChar(inp); p1 = s[s_comat]; }
	if (inp->qqdepth <= 0) {
	  sendRes(p1);
	  goto alret;
	}
      }
      save(p1);
      save(inp->fob);  /* dup'ed here popped in rqqexp */
      stkret(s_rqqexp); 
      goreto(s_ratx, s_rhead);
      break;
    case point: /* could this be the start of a num */
      CHECKBUF;
      temi = nextChar(inp);
      inp->radix = (int)getNumVal(a.v.hib);
      if (temi > 0 && !isNumDigit(temi, inp->radix)) {
	if (isWord(temi)) goto ratxit;
	sendRes(chartab[point]); /* no it is a bona fide dot */
	goto alret;
      }
      /* else drop through to plus/neg */
    case plus:
    case neg:
      CHECKBUF;
      temi = nextChar(inp);
      inp->radix = (int)getNumVal(a.v.hib);
      if (isNumDigit(temi, inp->radix) || (temi == point)) {
	resetFilNState(inp);
	inp->radix = (int)getNumVal(a.v.hib);
	inp->alen = 0;
	inp->hadneg = (tchar==neg);
	if (tchar==point) {inp->hadpoint=1; inp->nind=1;} /* index point+1 */
	inp->aline[inp->alen++] = tchar;
	opcd = s_numaux;
	break;
      } 
      if (tchar == point) { sendRes(chartab[point]); goto alret; }
      /* else don't be cute, just go to default and atm */
      goto ratxit;
    default:
    ratxit:
      if (!isSymSt(tchar)) {
	chuck_line(inp);
	error(inv_sym,mkEmes(2,"Unexpected character ",itos(tchar)));
      }
      inp->alen = 0; inp->aline[inp->alen++] = tchar; opcd = s_rataux; 
      break;
    }/*case*/
    break;  /*s_ratx1*/

  case s_chuckle: /* chuck to line end */
    p1  = getStk(-3);
    inp = getFil(p1); /* by arrangement */
    if (inp->bufstat == ldvs_eof) {
      opcd = retstk();
    } else if (inp->bufstat==ldvs_nodata) {
      save(inp->fob); /* scheduler expects this          */
      disposition = winp;   /* reschedule me my file in on tos */
      goto alout;
    } else if (!chuck_line(inp)) opcd = retstk();
    break;

  case s_rataux:
    inp   = getFil(getStk(-2));
    while ((temi=nextChar(inp)) && (inp->bufstat == ldvs_ok) && isWord(temi)) {
      inp->aline[inp->alen++] = getChar(inp);
    }
    if (inp->bufstat == ldvs_nodata) {
      save(inp->fob);    /* scheduler needs to know who we are waiting for */
      disposition = winp;/* come back to rataux with fob popped from stack */
      goto alout;
    }
    p1 = makeAtom(inp->aline,inp->alen, s_var);
#if (EXP)
    if ((temi==lsbrc) || (temi==lcbrc)) {
      getChar(inp);
      save(p1);           /* aref symbol   */ 
      stkret(s_arend);    /* pop & cleanup */
      save(ptru);         /* no rav option */
      save(inp->fob);     /* back to ratx  */
      stkret(s_nop); 
      goreto(s_ratx,(temi==lsbrc) ? s_rvhead : s_revhd);
      goto alcnt;
    }
#endif
    sendRes(p1);
    goto alret;
    break;

#if (EXP)
  case s_arend:
    p1 = restore(); /* symbol of aref */
    sendRes(list3(s[s_aref], p1, getRes()));
    goto alret;
    break;
 #endif
    
  case s_numaux: /* on first entry leading point or sign already eaten */
    inp = getFil(getStk(-2)); /* filfob still @ -2 */
    while ((temi = nextChar(inp)) && (inp->bufstat == ldvs_ok)) {
      if (isNumDigit(temi, inp->radix)) inp->ndigs[inp->nind]++;
      else if (temi == point) {
	if (inp->hadpoint || inp->hadexp) goto ratty;
	inp->hadpoint = inp->alen+1;
	inp->nind = 1;
      } else if (temi == dexp) {
	if (inp->hadexp) goto ratty;
	inp->hadexp = inp->alen+1;
	inp->nind = 2;
      } else if (temi == neg) {
	if (!inp->hadexp || (inp->ndigs[2] > 0)) goto ratty;
	else { inp->hadeneg = true; inp->hadexp++; }/* step over the minus */
      } else if (temi == plus) {
	if (!inp->hadexp || (inp->ndigs[2] > 0)) goto ratty;
	else inp->hadexp++; /* step over the plus */
      } else if ((temi == 'j')|| isWhite(temi) || isSpec(temi)) goto rcompy;
      else goto ratty; /* not digit, j, white or spec */
      tchar = getChar(inp);
      if (inp->alen < max_line_len) inp->aline[inp->alen++] = tchar;
    } /* while ((temi..)) */
    if (inp->bufstat == ldvs_nodata) {
      save(inp->fob);
      disposition = winp; /* come back to numaux, filfob on tos for scheduler */
      goto alout;
    }
  rcompy: /* check for end / start of complex or just end of real */
    if (inp->hadexp && !inp->ndigs[2]) goto ratty;
    if (inp->hadcpx) {    /* end of complex number conversion */
      if (temi == 'j') goto ratty;   /* naughty double j      */
      sendRes(cvtCpx(inp, inp->radix, getRes())); /* getRes() is real part */
      goto alret;                    /* exit                  */
    }
    if (temi == 'j') {
      sendRes(cvtCpx(inp, inp->radix, pnil));
      inp->aline[inp->alen++] = getChar(inp);  /* eat the imaginary symbol */
      opcd = s_cpxaux;
      goto alcnt;
    }
    sendRes(cvtNum(inp,inp->radix)); /* otherwise deal with single real value */   
    goto alret;               /* and exit                              */
  ratty: /* supposed number turned out to be a symbol in the end */
    opcd = s_rataux;
    goto alcnt;
    break;

  case s_cpxaux: /* deal with start of imaginary number */
    inp = getFil(getStk(-2)); /* filfob still @ -2 */
    if (inp->bufstat == ldvs_nodata) {
      save(inp->fob);
      disposition = winp; /* come back to cpxaux, filfob on tos for scheduler */
      goto alout;
    }
    resetFilNState(inp);
    CHECKBUF;
    temi = nextChar(inp);
    if (isWhite(temi) || isSpec(temi)) goto alret; /* we are done */
    inp->radix = (int)getNumVal(a.v.hib); /* no 0 for imag radix >10 */
    /* 0AjB => 10j11 */
    if (isNumDigit(temi, inp->radix)) {
      if (temi == '0') {
	tchar = getChar(inp);
	CHECKBUF;
	temi = nextChar(inp);
	if (temi == 'x') {
	  inp->radix = 16;
	  inp->aline[inp->alen++] = '0';
	  inp->aline[inp->alen++] = 'x';
	  getChar(inp);
	}
      }
      goto cpxauxout;
    }
    if ((temi != point) && (temi != plus) && (temi != neg)) goto ratty;
    tchar = getChar(inp);
    CHECKBUF;
    temi = nextChar(inp);
    /* need digit after leading point */
    if ((tchar == point) && !isDigit(temi)) goto ratty;
    /* need point or digs after leading sign */
    if (((tchar == plus) || (tchar == neg)) &&
	(!isDigit(temi) && (temi != point))) goto ratty;
    inp->hadneg = (tchar==neg);
    inp->aline[inp->alen++] = tchar;  /* eat leading point or sign   */
    if (tchar==point) {inp->hadpoint = inp->alen; inp->nind=1; }
  cpxauxout:
    inp->nstart = inp->alen; /* start of the imaginary dude flag in rcompy */
    inp->hadcpx = 1;
    opcd = s_numaux;  /* get number and exit */
    goto alcnt;

  case s_dqaux: /* string reader */
    p1 = restore(); // result list
    inp = getFil(getStk(-2));
    while ((inp->bufstat == ldvs_ok) && (0 < (temi = getChar(inp)))) {
      tchar = temi;
    dqaux_cont:
      if (tchar == excape) {
	temi = getChar(inp);
	if (temi < 0) {
	  opcd = s_dqexc;
	  save(p1);
	  save(inp->fob);
	  disposition = winp; /* come back to dqexc */
	  goto alout;
	} else if (temi != eol) {
	   if (inp->alen < max_line_len) inp->aline[inp->alen++] = excape;
	   if (inp->alen < max_line_len) inp->aline[inp->alen++] = temi;
	}
      } else if (tchar == dquot)  {
	p2 = buildChr((char *)inp->aline,
		      unEscLineLen((char *)inp->aline,inp->alen));
	if (isNil(p1)) sendRes(p2);
	else {
	  save(p2);
	  p1 = cons(p2,p1);
	  sendRes(implode(nrevList(p1),pnil,chr,pnil));
	  pop1();
	}
	goto alret;
      } else {
	if (inp->alen >= max_line_len) {
	  p2 = buildChr((char *)inp->aline,
			unEscLineLen((char *)inp->aline,inp->alen));
	  save(p2);
	  p1 = cons(p2,p1);
	  pop1();
	  inp->alen = 0;
	}
	inp->aline[inp->alen++] = tchar;
      }
    }
    if (inp->bufstat == ldvs_nodata) {
      save(p1);
      save(inp->fob);     /* scheduler needs to know who we are waiting for */
      disposition = winp; /* come back to dqaux */
      goto alout;
    } 
    error(bad_str,mkFmes("No closing quote in file: ",inp));
    break;
    
  case s_dqexc:
    p1  = restore();
    inp = getFil(getStk(-2));
    if (inp->bufstat != ldvs_eof) {
      tchar = excape;
      goto dqaux_cont;
    }    
    error(bad_str,mkFmes("No closing quote in file: ",inp));
    break;

  case s_rqqexp:
    p2 = restore(); /* filfob */
    ASSERT(isaFil(p2));
    inp = getFil(p2);
    p1 = restore(); /* quasiquote tag symbol */
    p3 = getRes();
    if (isQquot(p1)) {  /* expand quasiquote */
      pointer p4 = rlookup(p1);
      inp->qqdepth--;   /* de-nesting from qq */
      if (inp->qqdepth == 0) {
	if (p4.wh && !isNil(p4)) { 
	  /* user has own version of qquote */
#if (DEBUG)
	  alpsOuts(alpserr,"rqqexp: ");
	  alpsPrint(alpserr,p3,true,s_print);
	  alpsOutln(alpserr);
	  alpsFlush(alpserr);
#endif
	  iexp = p4;
	  parm = allocEnv(1);
	  sarg1(parm,p3);
	  opcd = s_sys_apply;	
	  goto alcnt;
	} else {
	  parm = p3;
	  goreto(s_rbqcnt1,s_rqqend);
	  goto alcnt;
	}
      }
    }
    sendRes(list(p1, p3));
    goto alret;
    
  case s_rqqend:
    iexp = lookup(a.v.hqq); /* quasiquote simplify hook */
    if (!isNil(iexp)) {
      parm = allocEnv(1);
      sarg1(parm,getRes());
      opcd = s_sys_apply;
      goto alcnt;
    }
    goto alret;

  case s_rqaux:
    sendRes(list(s[s_quote], getRes()));
    restore(); /* pop dup'ed filfob */
    goto alret;

  case s_read:
    if (nargs(parm)==1) saveF(arg1(parm)); /* check arg & save on stack */
    else save(lookup(a.v.hrf));            /* save current input filfob */
    /* drop through quickly */

  case s_sys_read:
    p1 = restore();          /* it is the filfob for the input file         */
    inp = getFil(p1);
    inp->qqdepth = 0;
    p2 = getfs(1);           /* allocate a dummy FS as dest                 */
    sendRes(p2);             /* for saving in saveDest()                    */
    saveDest();              /* with trailer and all                        */
    dest = allocDest(0);     /* snark avoidance trick                       */
    setDest(p2);             /* initial dest is car of p2                   */
    stkret(s_rendres);       /* remember to restore dest  and post result   */
    save(p1);                /* fobbit for the rat & co                     */
    stkret(s_rend);          /* remember to drop the fob                    */
    goreto(s_ratx, s_rhead); /* send ganesha off for a ride on his familiar */
    break;

  case s_rend:
    p1 = restore();    /* pop filfob used by readr */ 
    ASSERT(isaFil(p1)); /* circumspection */
    goto alret;

  case s_rendres: 
    p1 = dest; 
    restoreDest(); 
    sendRes(car(getRes())); 
    if (!isBusy(p1)) {
      INCCT(envct,4);
      retEnv(p1); 
    }
    goto alret;

  case s_rclr:
    inp = cin();
    chow_blanks(inp,eol); /* eat trailing blanks up to and including newline */
    if (nextChar(inp)==eol) getChar(inp);
    goto alret;

  case s_rhead: /* tos = ret, tos-1 = filfob */
    if (isLpar(getRes())) { goreto(s_ratx, s_rhaux);  goto alcnt;  }
    goto alret;
    break;

  case s_rhaux: /* on entry tos = ret opcd , tos-1 = filfob */
    p1 = getRes();
    p2 = getStk(-1); /* filfob */
    if (isRpar(p1)) { sendRes(pnil);  goto alret; } /* case () => nil */
    if (isLpar(p1)) {
      sendRes(p1 = getfs(1)); /* start list for dest  */
      save(p2);               /* filfob food for ratx */
      saveStoreDest(p1);      /* save p1 and make it the new dest */
      bumpDest();             /* come back to cdr */
      stkret(s_rtail);
      stkret(s_ratx);
      save(p2);          /* ratx gets hungry with lists */
      saveStoreDest(p1);
      goreto(s_ratx, s_rtail);
    } else {
      sendRes(p1 = cons(p1,pnil));
      save(p2);
      saveStoreDest(p1);
      bumpDest();       /* next dest is cdr */
      goreto(s_ratx, s_rtail);
    }
    break;

  case s_rtailx: popStk(1); /* and drop through */

  case s_rtail: /* on entry tos = dest, tos-1 = filfob */
    p1 = getRes();
    p2 = getStk(-1); /* filfob */
    if (isEof(p2)) {
      error(inv_exp,mkFmes(" (..) unexpected EOF in : ",getFil(p2)));
    } else if (isRpar(p1)) {
      sendRes(pnil); setDest(restore()); restore(); goto alret;
    } else if (isLpar(p1)) { 
      save(getStk(-1)); stkret(s_rtailx); goreto(s_ratx, s_rhaux);
    } else if (!isDot(p1)) {
      sendRes(cons(p1,pnil)); /* Tack cons onto list under construction    */
      setDest(getRes());      /* make new cons the destintation            */
      bumpDest();             /* set sendRes() target to cdr of above cons */
      goreto(s_ratx, s_rtail);
    } else {
      goreto(s_ratx, s_rdot); /* read post dot */
    }
    break;

  case s_rdot: /* on entry tos = dest tos-1 = filfob */
    if (isRpar(getRes())) { /* got dot before rpar -> send (.) */
      //error(inv_exp,"read: unexpected rpar");
      sendRes(cons(chartab[point],pnil));
      setDest(restore());
      popStk(1);            /* other filfob */
      goto alret;           /* s_rend */
    }
    if (isLpar(getRes())) {
      save(getStk(-1));
      stkret(s_rpaux); /* goto come back here */
      goreto(s_ratx, s_rhaux);
    } else {
      save(getStk(-1));
      save(getRes());
      goreto(s_ratx, s_rrpar); /* expect rpar */
    }
    break;

  case s_rpaux: /* on entry tos = filfob */
    save(getRes());
    goreto(s_ratx,s_rrpar);
    break;

  case s_rrpar:/* on entry tos = saved res, tos-1 = filfob */
    if (!isRpar(getRes()))  error(inv_exp,"dot expected closing parenthesis");
    sendRes(restore());   /* resend dotted cdr */
    popStk(1);            /* drop filfob */
    setDest(restore());
    popStk(1);            /* other filfob */
    goto alret;           /* s_rend */
    break;

  case s_rbqcnt1: /* qqexp */
    if (isAtom(parm)) sendRes(list(s[s_quote],parm));
    else {
      p1 = car(parm);
      if (isComma(p1))  sendRes(cadr(parm));
      else if (isComAt(p1)) error(inv_exp,"Can't have ,@ here");
      else if (isQquot(p1)) { // (qqexp (qqexp (cadr X))))
	parm = cadr(parm);
	stkret(s_rbqcnt1);            // stack up next call to qqexp
	goreto(s_rbqcnt1,s_rbqcnt5); // (qqexp (cadr X)) come back to 5
	goto alcnt;
      } else {   // (list 'append (qqexpl (car X)) (qqexp (cdr x)))
	stkret(s_rbqcnt4);         // returned to in 3
	save(parm);               // popped in 3 for cdr
	parm = p1;                // parm is now (car X)
	goreto(s_rbqcnt2,s_rbqcnt3);  // (qqexpl (car X)) come back to 3
	goto alcnt;
      }
    }
    goto alret;

  case s_rbqcnt2:  /* qqexpl */
    if (isAtom(parm)) { /* (list 'quote (list X)) */
      save(p1 = cons(parm,pnil));
      sendRes(list(s[s_quote],p1));
      pop1();
    } else {
      p1 = car(parm);
      if (isComma(p1))  sendRes(list(s[s_list],cadr(parm)));
      else if (isComAt(p1)) sendRes(cadr(parm));
      else if (isQquot(p1)) {
	parm = cadr(parm);
	stkret(s_rbqcnt2);            // stack up next call to qqexpl
	goreto(s_rbqcnt1,s_rbqcnt5); // (qqexp (cadr X)) come back to 5
	goto alcnt;
      } else { // (list 'list (list 'append  (qqexpl (car X)) (qqexp (cdr x))))
	stkret(s_rbqcnt6);          // returned to in 3
	save(parm);                // popped in 3 for cdr
	parm = p1;                 // parm is now (car X)
	goreto(s_rbqcnt2,s_rbqcnt3);   // (qqexpl (car X)) come back to 3
	goto alcnt;
      }
    }
    goto alret;

  case s_rbqcnt3:             // parm on tos, eval (qqexp (cdr parm))
    p1    = restore();        // parm saved in 1 and 2 
    parm  = cdr(p1);          // next parm
    topcd = retstk();         // this would be 4 or 6 from 1 and 2 respectively
    save(getRes());           // save result from (qqexpl (car X))
    goreto(s_rbqcnt1,topcd);  // (qqexp (cdr X)) come back to 4 or 6
    break;

  case s_rbqcnt4:   // (list 'append (qqexpl (car X)) (qqexp (cdr X)))
    p1 = restore(); // restore result from (qqexpl (car X))
    p2 = list3(s[s_append],p1,getRes());  // current res is (qqexp (cdr X))
    sendRes(p2);
    goto alret;

  case s_rbqcnt5:
    parm = getRes();  // result from (qqexp (cadr X))
    goto alret;       // this will be qqexp/1 from 1 or qqexpl/2 from2

  case s_rbqcnt6: //(list 'list (list 'append (qqexpl (car X)) (qqexp (cdr x))))
    p1 = restore(); // result from (qqexpl (car X))
    p2 = list3(s[s_append],p1,getRes());  // current res is (qqexp (cdr X))
    save(p2);
    sendRes(list(s[s_list],p2));
    pop1();
    goto alret;

  case s_rvhead:   /* read the vector within [...]         */
  case s_revhd:    /* read the stuff within {...}          */
    p1 = getRes(); /* on entry tos = s_nop, tos-1 = filbob */
    popStk(1);     /* lose the nop  , filfob ontos         */
    if (opcd==s_rvhead) {
      topcd = s_rvcont;
      if (isRsbrc(p1)) goto rvexit;
    } else {
      topcd = s_revcnt;
      if (isRcbrc(p1)) { 
      rvexit:
	popStk(1); 
	sendRes(isNil(restore()) ? r.s.nonum : r.s.noref); // testing norav
	goto alret; 
      } 
    }  
    if (isLpar(p1))  {
      sendRes(cons(pnil,pnil));
      p1 = getRes();
      p2 = getTos();          /* filfob                     */
      saveStoreDest(p1);      /* filfob now @ -1            */
      bumpDest();             /* come back to cdr in rvcont */
      stkret(topcd);          /* filfob @ -2 for the rat    */
      stkret(s_ratx);
      save(p2);                /* the rat is hungry         */
      saveStoreDest(p1);       /* next ratx dest is car     */
      goreto(s_ratx, s_rtail);
    } else { /* arrrg! */
      sendRes(cons(p1,pnil));
      saveStoreDest(getRes());  /* to be restored for implod */
      bumpDest();               /* next dest is cdr          */
      goreto(s_ratx, topcd);
    }
    break;

  case s_rvcont:     /* continue reading vector literal []  */
  case s_revcnt:     /*                              or {}  */
    p1 = getRes();   /* on entry tos = dest, tos-1 = filfob */
    p2 = getStk(-1);
    if (isEof(p2))
      error(bad_vec,mkFmes(" {/[..]/} unexpected EOF in : ",getFil(p2)));
    if (((opcd==s_rvcont) && (isRsbrc(p1))) ||
	((opcd==s_revcnt) && (isRcbrc(p1)))) {
      sendRes(pnil);
      setDest(restore());  /* re-instate earlier res */
      popStk(1); /* lose filfob */
      if (opcd==s_rvcont) {
	sendRes(implode(getRes(),pnil,num,restore()));/* restore norav option */
      } else {
	save(p3 = list3(s[s_implod],pnil,restore()));
	save(p4 = cons(s[s_list], getRes()));
	sendRes(nconc2(p3,cons(p4,pnil))); // (implod nil norav (list getRes()))
	pop2();
      }
      goto alret;
    }
    if (isLpar(p1)) {
      stkret(opcd); 
      save(p2);
      stkret(s_rend); /* will peel off saved filfob for re-entry to opcd */
      goreto(s_ratx, s_rhaux);
    } else  {
      sendRes(cons(p1,pnil));
      setDest(getRes());
      bumpDest(); /* target is cdr of above cons */
      goreto(s_ratx, opcd);
    }
    break;

  case s_readl:
    if (nargs(parm) == 1) saveF(arg1(parm)); /* check and save on stack   */
    else save(lookup(a.v.hrf));              /* save current input filfob */
    inp = getFil(getTos());
    switch (inp->bufstat) {
    case ldvs_nodata:
    case ldvs_ok:                    opcd = s_readla; break;
    case ldvs_eof:    sendRes(pnil); opcd = s_rend;   break;
    }
    break;

  case s_readla:
    inp = getFil(getTos());
    chow_blanks(inp,eol);
    inp->qqdepth = 0;
    if (inp->bufstat == ldvs_nodata) {
      save(getTos()); 
      disposition = winp;
      goto alout;
    }
    if (nextChar(inp) == comment) {
      if (chuck_line(inp)) {
	putbackChar(inp,comment); /* sorry, this is ugly */
	goto alcnt; /* come back to *-2 and find eol */
      }
    }
    if ((inp->bufstat == ldvs_eof) || (nextChar(inp) == eol))  {
      getChar(inp); /* eat the eol */
      sendRes(pnil);
      popStk(1);     /* fob */
      goto alret;
    } else {
      p1 = restore();        /* fobbit */
      p2 = cons(pnil, pnil); /* will be sending res into this list */
      sendRes(p2);           /* we will pick him up on the way out */
      saveDest();            /* antisnark save of current dest     */
      dest = allocDest(0);   /* let's have a new dest for a while  */
      setDest(p2);           /* initial dest is car of p2          */
      save(p1);              /* put fobbit  back on top            */
      stkret(s_nop);         /* ratx reaches down over this        */
      goreto(s_ratx, s_readlx);
    }
    break;

    case s_readlx: 
      inp = getFil(getStk(-1));
      chow_blanks(inp,eol); 
      inp->qqdepth = 0;
      if (inp->bufstat == ldvs_nodata) {
	save(inp->fob);
	disposition = winp;
	goto alout;
      }
      if (nextChar(inp) == comment) {
	if (chuck_line(inp)) {
	  putbackChar(inp,comment); /* sorry again  */
	  goto alcnt; /* come back to *-2 and find eol */
	}
      }
      if ((inp->bufstat == ldvs_eof) || (nextChar(inp) == eol))  {
	getChar(inp);              /* eat eol if there is one          */
	popStk(2);                 /* drop the  s_nop and fob          */
        restoreDest();             /* back to the original dest retEnv?*/
	goto alret;                /* result is in original dest too   */
      } else {
	bumpDest();                /* dest now pointing to cdr         */
	p1 = cons(pnil, pnil);     /* new link for the chain           */
	sendRes(p1);               /* tack it onto the end of chain    */
	setDest(p1);               /* car of new link is new dest      */
	goreto(s_ratx, s_readlx);  /* ganesha is off again on his rat  */
      }
      break;
      
  case s_getc:
    if (nargs(parm) == 1) saveF(arg1(parm)); /* check arg and save on stack */
    else save(lookup(a.v.hrf));              /* save current input filfob   */
    opcd = s_getc1;
    /* drop through */
    
  case s_getc1:
    inp = getFil(restore());
    nextChar(inp);
    switch (inp->bufstat) {

    case ldvs_ok:
      if (0 <= (temi = getChar(inp))) {
	ASSERT(temi<256);
	sendRes(sstrtab[temi]);
	clrRawMode(inp);
	opcd = retstk();
	break;
      }
      /* else fall through to nodata story */

    case ldvs_nodata:
      save(inp->fob); /* once for us and    */
      save(inp->fob); /* once for scheduler */
      setRawMode(inp);
      disposition = winp;
      goto alout;

    case ldvs_eof:
      sendRes(pnil);
      clrRawMode(inp);
      opcd = retstk();
      break;
    }
    break;

  case s_getl:
    if (((nargs(parm) > 0)  && (!isaFil(arg1(parm)))) || (nargs(parm) > 2))
      error(inv_funarg,"(getl [[<file object>] <eol-char>]");
    if (nargs(parm) == 2) {
	p1 = arg2(parm);
	if (!isaChr(p1) || nels(p1) != 1) error(inv_funarg,"Invalid eol character");
    } else p1 = sstrtab['\n'];
    save(p1);
    saveF((nargs(parm) > 0) ? arg1(parm) : lookup(a.v.hrf));
    inp       = getFil(getTos());
    inp->alen = 0;       // length of accumulated input line
    opcd      = s_getl1; // drop through quickly now
    
  case s_getl1:
    inp = getFil(restore());
    p1  = getTos();
    tchar = *p1.vr->varadd.sp;
  getl1_somemore:
    if (inp->bufstat == ldvs_nodata) {
      save(inp->fob);    
      save(inp->fob);    
      disposition = winp; 
      goto alout;
    }
    temi = 0;  // gcc optimizer needs this
    while ((inp->bufstat == ldvs_ok)        &&
	   (tchar != (temi = getChar(inp))) &&
	   (temi >= 0)) {
      // Note: line is truncated after max_line_len chars
      if (inp->alen < max_line_len) inp->aline[inp->alen++] = temi;
    }
    if (temi == tchar) sendRes(mkChr(inp->aline,inp->alen));
    else if (inp->bufstat == ldvs_eof) /* eof only set after read on nodata */
      sendRes(inp->alen ? mkChr(inp->aline,inp->alen) : pnil);
    else  goto getl1_somemore;
    popStk(1); /* drop eol character */
    goto alret;
  
  case s_getb:
    p1   = arg1(parm);
    p2   = arg2(parm);
    if (!isNum(p2) || nels(p2) != 1) 
      error(inv_funarg,"(getb <file object> <scalar>)");
    inp  = getFil(p1);
    temi = getInt(p2); /* requested buffer length */
    if (temi <= 0) {
      sendRes(r.s.nostr);
      goto alret;
    }
    inp->alen = 0;  // accumulated buffer len, naughty could be preempted
    if (p1.fb->type == buft) {/* buffer case */
      if (temi <= (inp->ecci - inp->ifpos)) {
	p2 = allocCV(temi);
      } else {
	if (inp->bufstat != ldvs_ok) {
	  p2 = pnil;
	  temi = 0;
	} else {
	  temi = inp->ecci - inp->ifpos;
	  ASSERT(temi > 0);
	  p2 = allocCV(temi);
	}
      }
      if (temi > 0) {
	memcpy(getCadd(p2,0),getCadd(inp->ibuf,inp->ifpos),temi);
	inp->ifpos += temi;
	if (inp->ifpos >= inp->ecci) inp->bufstat = ldvs_eof;
      }
      sendRes(p2);
      goto alret;
    } /* buffer case */
    p2 = allocCV(temi); /* allocate whole buffer, patch len for short read */
    save(p2);
    save(p1);
    opcd = s_getb1; // drop through quickly now
    
  case s_getb1:
    inp  = getFil(restore());
    temi = getInt(arg2(parm));
  getb1_somemore:
    if (inp->bufstat == ldvs_nodata) {
      save(inp->fob);    
      save(inp->fob);    
      disposition = winp; 
      goto alout;
    }
    if (inp->bufstat == ldvs_ok) {
      p2   = getTos();
      temi = nels(p2);
      i    = inp->ecci - inp->ilpos; // number of fresh octets in read buffer
      len  = ((inp->alen + i) > temi) ? temi - inp->alen : i;
      memcpy(getCadd(p2,inp->alen),getCadd(inp->ibuf,inp->ilpos),len);
      inp->alen  += len;
      inp->ilpos += len;
      if (inp->ilpos >= inp->ecci) {
	inp->bufstat = ldvs_nodata;
	inp->ilpos = inp->ecci = 0;
      }
    }
    if ((inp->alen == temi) || (inp->bufstat == ldvs_eof)) { 
      p1 = restore();
      if (inp->alen < temi) {
	if (inp->alen == 0) p1 = pnil;
	else p1.vr->varels = p1.vr->vard[0] = inp->alen; // DANGER (patching)
      }
      sendRes(p1);
      goto alret;
    }
    goto getb1_somemore;

  case s_putb: /* put buffer in file */
    p1  = arg1(parm);  // buffer
    p2  = arg2(parm);  // file
    if (!isFil(p2)) error(inv_funarg,"File argument not a file");
    out = getFil(p2);
    alpsFlush(out);
    len = write(out->fd,getCadd(p1,0),nels(p1));
    sendRes((len == nels(p1)) ? ptru : pnil);
    goto alret;

#if (INSTCON)
    /* HPIB instrument control sort of backward compatible with
       9816 Pascal workstation deb7 / DLISP6                     */
  case s_oio: sendRes(alpsOpenInst(parm)); goto srend; 
  case s_rio: sendRes(alpsReadInst(parm)); goto srend; 
  case s_wio: sendRes(alpsWriteInst(parm)); goto srend; 
  case s_cio: sendRes(alpsCloseInst(parm)); goto srend; 
#else
  case s_oio: case s_rio:  case s_wio: case s_cio: error(not_imp,"INSTCON");
#endif

  case s_terpri: /* subr */
    if (nargs(parm)==0) {
      i   = 1;
      out = cout();
    } else if (nargs(parm)==1) {
      if (isNum(arg1(parm))) {
	i    = getWhole(arg1(parm));
	out = cout();
      } else if (isaFil(arg1(parm))) {
	i   = 1;
	out = getFil(arg1(parm));
      }
    } else {
      ASSERT(nargs(parm)==2);
      i   =  getWhole(arg1(parm));
      out =  getFil(arg2(parm));
    }
    sendRes(terpri(out,i));
    goto srend;

  case s_prompt: /* subr */
    alpsPrompt(cin(),cout());                           goto srend;

  case s_lpos: /* subr */
    out = (nargs(parm) == 1) ? getFil(arg1(parm)) : cout();
    sendRes(mkNum(out->olpos+1));
    goto srend;

  case s_tab: sendRes(do_tab(parm));                    goto srend;

  case s_eof:
    p1 = arg1(parm);
    inp = getFil(p1);
    if (nextChar(inp) >= 0)       { sendRes(pnil); goto srend; }
    if (inp->bufstat == ldvs_eof) { sendRes(ptru); goto srend; }
    opcd = s_eofx;
    save(p1);
    save(p1);
    disposition = winp;
    goto alout;
    
  case s_eofx:
    p1 = restore();
    inp = getFil(p1);
    sendRes((inp->bufstat == ldvs_eof) ? ptru : pnil);
    goto srend;
    
  case s_dump: /* subr */
    save(alpsOpenF(parm));
    opcd = s_dumpx;
    /* drop through */

  case s_dumpx:
    p1  = restore();
    inp = getFil(p1);
    switch (inp->bufstat) {

    case ldvs_ok:
      alpsOutb(cout(),vadd(inp->ibuf).sp,inp->ecci);
    case ldvs_nodata:
      save(p1);
      save(p1);
      disposition = winp;
      goto alout;

    case ldvs_eof:
      alpsTab(cout(),0); /* linefeedf if needed */
      alpsFlush(cout());
      sendRes(alpsCloseF(p1)); 
      goto srend;
    }
    break;
    
  case s_exec: /* subr */
    p1 = alpsOpenB(parm);
    save(p1);
    stkret(s_exec1);
    save(p1); /* gets popped in sys_read */ 
    opcd = s_sys_read;
    break;

  case s_exec1:
    iexp = getRes();
    p1   = restore(); 
    alpsCloseF(p1);
    opcd = eval;
    break;
      
  case s_load: /* subr */
    p1 = alpsOpenLoadF(parm);  /* filfob of load file        */
    p2 = p1.fb->name;          /* filename to load           */
    p3 = pnil;                 /* list of files being loaded */
    save(iexp);                /* not sure this is needed anymore           */
    p3 = findCtl(t,0,s_load3); /* See what other loads are in progress      */
    if (!isNil(p3)) {          /* check that we are not already loading p2  */
	p3 = p3.pt[-1];         
	if (!isNil(memql(p2,p3))) { /* already loading p2 */
	  iexp = restore();
	  sendRes(ptru);
	  goto alret;
	}
    }
    p3 = cons(p2,p3);         /* add p2 to list */
    save(p3);                   /* load list */
    stkret(s_load3);
    save(p1);                  /* filfob   */
    stkret(s_load1);
    save(p1); /* gets popped in sys_read */ 
    opcd = s_sys_read;
    if (!no_notice) {
      alpsOuts(cout(),"loading ");
      alpsOutb(cout(),p2.vr->varadd.sp,p2.vr->varels);
      alpsOutln(cout());
      alpsFlush(cout());
    }
    break;

  case s_load1:
    if (!isEof(getTos())) {
      iexp = getRes();
      goreto(eval, s_load2);
    } else{
      p1 = restore();  /* pop anchor filfob      */
      p2 = lookup(a.v.hlh);
      p3 = p1.fb->name;
      /* TODO: reorder so that latest loaded is first */
      if (isNil(member(p3,p2))) setSym(a.v.hlh,cons(p3,p2));
      if (isfSet(dbgFlag)) {
	alpsOuts(cout(),"loaded ");
	dprtln(cout(),p3,80);
      }
      sendRes(alpsCloseF(p1));
      goto alret; /* goes to load3 for exit */
    }
    break;

  case s_load2:
    if (isfSet(dbgFlag)) print(cout(),getRes(),s_print); 
    p1 = getTos();
    stkret(s_load1);
    save(p1); /* gets popped in sys_read */ 
    opcd = s_sys_read; 
    break;

  case s_load3:
    p3 = restore();                   /* Throw away loadlist */
    iexp = restore();
    goto alret;
    
  case s_require: /* subr */
    p1  = arg1(parm);
    len = nels(p1);
    if ((len+3) >= max_line_len) error(inv_funarg,"File name too long");
    memcpy(tline,p1.vr->varadd.sp,len);
    memcpy(&tline[len],alps_suffix,strlen(alps_suffix)); /* append suffix    */
    p2 = mkChr(tline,len+3);      /* would be file name being required       */
    if (isNil(member(p2,lookup(a.v.hlh)))) { /* feature has not been loaded  */
      if (nargs(parm) == 2) p2 = arg2(parm); /* use user supplied file name  */
      save(p2);
      parm = allocEnv(1);
      restore();
      sarg1(parm,p2);
      opcd = s_load;
    } else {
      sendRes(p1);                /* return feature */
      goto srend;
    }
    break;

#if (WSPSAV)
  case s_save: /* subr */
    if (nargs(parm) ==1) {
      p1 = arg1(parm);
      setSym(a.v.hws,p1);
    }
    opcd        = s_saveaux;
    disposition = tyld;
    goto alout;

  case s_restore: /* subr */
    if (nargs(parm) ==1) {
      p1 = arg1(parm);
      setSym(a.v.hws,p1);
    }
    opcd        = s_rstraux;
    disposition = tyld;
    goto alout;
#else
  case s_save: case s_restore: error(not_imp,"WSPSAV");
#endif

  case s_saveaux:
    p1 = restore();
    if (isNil(p1)) error(inv_funarg,"Can't save workspace");
    else sendRes(ptru);
    goto alret;

  case s_rstraux:
    p1 = restore();
    if (isNil(p1)) error(inv_funarg,"Can't restore workspace");
    else sendRes(ptru);
    goto alret; /* NOT REACHED */

  case s_print: /* subrs */
  case s_prin0:
  case s_prinl: 
  case s_princ:
    out = (nargs(parm) == 2) ? getFil(arg2(parm)) : cout();
    sendRes(print(out,arg1(parm),opcd));
    goto srend;
    /* subrs */ 
  case s_beep:  sendRes(dobeep(cout(),cargn(0,parm),cargn(1,parm))); goto srend;
  case s_pp:    sendRes(pp(cout(),parm,opcd));                       goto srend;
  case s_plen:  sendRes(mkNum(plen0(arg1(parm),opcd)));              goto srend;
  case s_open:  sendRes(alpsOpenF(parm));                            goto srend;
  case s_ioctl: sendRes(alpsIOCtl(parm));                            goto srend;
  case s_close: sendRes(alpsCloseF(arg1(parm)));                     goto srend;
  case s_rewind:sendRes(alpsRewindF(arg1(parm)));                    goto srend;
  case s_seek:  sendRes(alpsSeekF(arg1(parm),arg2(parm)));           goto srend;
  case s_buf:   sendRes(alpsOpenB(parm));                            goto srend;
  case s_prat:  sendRes(prat (cout(),parm,opcd));                    goto srend;
  case s_pfree: sendRes(doPrintFree(cout(),parm));                   goto srend;
  case s_kill:  sendRes(doKill(nargs(parm),parm));                   goto srend;
    /* fsubrs */;
  case s_whzat: sendRes(mkNum(alpsWhzat(cout())));                   goto alret;
  case s_gc:    boogeyMan(anon); sendRes(ptru);                      goto alret;
  case s_clear: sendRes(do_clear(iexp));                             goto alret;
#if (GRAF)
     /* fsubrs */
  case s_glinit:    sendRes(glinit());                     goto alret;
  case s_gclear:    sendRes(gclear());                     goto alret;
    /* subrs */
  case s_genable:   sendRes( genable(nargs(parm),parm));   goto srend;
  case s_gdisable:  sendRes(gdisable(nargs(parm),parm));   goto srend;
  case s_proj:      sendRes( proj(!isNil(arg1(parm))));    goto srend;
  case s_graf:      sendRes(plot(nargs(parm),parm));       goto srend;
  case s_gmat:      sendRes(gmaterial(nargs(parm),parm));  goto srend;
  case s_light:     sendRes(glight(nargs(parm),parm));     goto srend;
  case s_glmodel:   sendRes(glightmod(nargs(parm),parm));  goto srend;
  case s_gsmodel:   sendRes(gshademod(nargs(parm),parm));  goto srend;
  case s_mkwdgt:    sendRes(gmkwdgt(nargs(parm),parm));    goto srend;
  case s_cfgwdgt:   sendRes(gcfgwdgt(nargs(parm),parm));   goto srend;
  case s_dispwdgt:  sendRes(gdispwdgt(nargs(parm),parm));  goto srend;
  case s_hidewdgt:  sendRes(ghidewdgt(nargs(parm),parm));  goto srend;
  case s_wdgtdata:  sendRes(wdgtdata(nargs(parm),parm));   goto srend;
  case s_gsldrval:  sendRes(gsldrval(nargs(parm),parm));   goto srend;
  case s_gdialval:  sendRes(gdialval(nargs(parm),parm));   goto srend;
  case s_gfsval:    sendRes(gfsval(nargs(parm),parm));     goto srend;
  case s_gtxtval:   sendRes(gtxtval(nargs(parm),parm));    goto srend;
  case s_gtxtclr:   sendRes(gtxtclr(nargs(parm),parm));    goto srend;
  case s_gtxtset:   sendRes(gtxtset(nargs(parm),parm));    goto srend;
  case s_gcanvas:   sendRes(gcanvas(nargs(parm),parm));    goto srend; 
  case s_gupdate:   sendRes(gupdate(nargs(parm),parm));    goto srend;
  case s_gsave:     sendRes(gsave(nargs(parm),parm));      goto srend; 
  case s_gload:     sendRes(gload(nargs(parm),parm));      goto srend; 
  case s_gwar:      sendRes(gwar(nargs(parm),parm));       goto srend; 
  case s_gresize:   sendRes(gresize(nargs(parm),parm));    goto srend; 
  case s_loc:       error(not_imp,"loc");                  goto srend;
#else
  case s_gclear:   case s_gupdate:  case s_glinit:   case s_gsave:
  case s_genable:  case s_gdisable: case s_proj:     case s_graf: 
  case s_gmat:     case s_light:    case s_glmodel:  case s_gsmodel:
  case s_mkwdgt:   case s_cfgwdgt:  case s_dispwdgt: case s_hidewdgt: 
  case s_gsldrval: case s_gdialval: case s_gfsval:   case s_gtxtval: 
  case s_gtxtclr:  case s_gtxtset:  case s_gcanvas:  case s_wdgtdata:
  case s_gload:    case s_gwar:     case s_gresize:  case s_loc:   
    error(not_imp,"GRAF");
#endif
#if (SOUND)
  case s_sopen:    sendRes(sound_init());                 goto alret;
  case s_rec:      sendRes(alps_rec(nargs(parm),parm));   goto srend;
  case s_play:     sendRes(alps_play(nargs(parm),parm));  goto srend;
  case s_sclose:   sound_close(); sendRes(ptru);          goto alret;
#else
  case s_sopen: case s_rec: case s_play: case s_sclose: error(not_imp,"SOUND");
#endif
  case s_oblis:    sendRes(r.s.ospc);                     goto alret;
  case s_gensym:   sendRes(gensym());                     goto alret;

  case s_break:
    freeze(t);  /* lock dispatcher (may have been locked by error already)  */
    saveDest();
    save(getVal(a.v.hpr)); /* save currrent user prompt                     */
    stkret(s_brkout);      /* clean-up routine when exiting break           */
    save_res();            /* save current result value                     */
    save(getVal(a.v.hex)); /* save orginal #EX                              */
    save(iexp);            /* save iexp as we will be using it              */
    save(envp);            /* we never tweak ctlstk except on abnormal exit */
    save(envp);            /* saved again: Videbimus aut viderimus          */
    save(pnil);            /* level is nil to show we are at the top level  */
  brk_again:
    dprtln(alpsout,iexp,80);     /* show current expression      */
    r.s.bprompt = r.s.brkprmpt;  /* set default bprompt "break: "*/
    setVal(a.v.hex,iexp);        /* set #EX so user can edit it  */
  brk_cont:                      /* prompt and read expression   */
    alpsSetPrompt(r.s.bprompt);  /* set current break prompt     */
    alpsPrompt(alpsin,alpsout);  /* prompt it                    */
    t->errflg = false;           /* let's make some more errors  */
    fClr(stpFlag);               /* clear stepping flag          */
    stkret(s_brkx);              /* return through break exit    */
    save(filin);                 /* Needed for ratx and friends  */
    stkret(s_brke);              /* come back to eval user input */ 
    goreto(s_ratx, s_rhead);     /* read what the user has to say*/
    break;

  case s_brke:                       /* evaluate break expression     */
    popStk(1);                       /* pop filin                     */
    iexp = getRes();                 /* iexp <- user break expression */
    if (isfSet(dbgFlag) &&           /* is it a valid dbg command ?   */
	(isDot(iexp)   || isDash(iexp)  || 
	 isRsbrc(iexp) || isRcbrc(iexp) || 
	 isNil(iexp)   || isNum(iexp)   || isRpar(iexp)))
      goto alret;                    /* yes, go deal with it in  brkx       */
    if (isNum(iexp) && (getInt(iexp) < 0)) { /* bail                        */
      alpsOutsln(alpserr,"User break exit to main...");
      opcd = s_bail;                 /* bail exits on done, dispacher will  */
      goto alcnt;                    /*  "thaw" us and reset the stack.     */
    } else if (isNil(iexp)) {        /* continue                            */
      popStk(7);                     /* tos == s_brkout                     */
      goto alret;
    }
    goreto(eval, s_brkc);            /* no, eval iexp and come back to brkc  */
    break;                  

  case s_brkc:                       /* continue break after eval user input */
    print(alpsout,getRes(),opcd);    /* print result                         */
    setVal(a.v.hrs,getRes());        /* set #RS so user can play with it     */ 
    p1 = restore();                  /* pop off break exit return op code    */
    ASSERT(s_brkx == p1.wh);         /* am i still sane ?                    */
    goto brk_cont;                   /* and continue break processing        */

  case s_brkx:                         /* exit break maybe                    */
    if (isNum(iexp)) {                 /* change of break level (teval)       */
      pointer tenvp;
      if (nels(iexp) == 0) {           /* [] -> back to original break level  */
	p1   = restore();              /* current break level                 */
	p2   = restore();              /* current environment                 */
	p3   = restore();              /* original environment                */
	if (!isNil(p1)) {              /* presently not at the top ?          */
	  ASSERT(isNum(p1));
	  i  = getInt(p1);             /* integer val of current level        */
	  p1 = findCtl(t,i,s_teval1);  /* find eval stack frame this level    */
	  ASSERT(isEqq(p2,p1.pt[-3]));
	  ASSERT(isEqq(p2,envp));
	  crank_up(p1,stkp);           /* restore dests & envs up the stack   */
	  swap_up(envp,p3);            /* to the original bindings            */
	} else ASSERT(isEqq(envp,p3)); /* should be on top                    */
	setEnv(p3);                    /* back to original environment        */
	iexp = getTos();               /* top break iexp on top of stack      */
	setVal(a.v.hex,iexp);          /* give user a handle on it (#EX)      */
	save(envp);                    /* original env                        */
	save(envp);                    /* current env                         */
	save(pnil);                    /* current level is top break level    */
	goto brk_again;
      }
      i = getInt(iexp);                   /* new desired level                */
      if (i < 0) { /* user wants out now so we abort the mission and bail     */
	/* We move the stack to the current level and bail from there meaning */
	/* that only unwind-protect forms *below* the current level will be   */
	/* evaluated as we bail.                                              */
	p1 = findCtl(t,0,s_brkout);       /* find exit point                  */
	ASSERT(!isNil(p1));               /* if we are here we must find it   */
	alpsSetPrompt(p1.pt[-1]);         /* restore user prompt              */
	p1 = restore();                   /* current level                    */
	if (!isNil(p1)) { /* not on top: (same stack sync code as slinky exit)*/
	  p1 = findCtl(t,getInt(p1),s_teval1); /*find current eval stack frame*/
	  if (isNil(p1)) error(pgmr_inc,"break level inconsistency");
	  p3 = p1.pt[-3];                 /* env of eval stack frame at level */
	  ASSERT(isEqq(envp,p3));         /* check that envp is correct here  */
	  p2 = restore();                 /* saved envp at this level         */
	  ASSERT(isEqq(envp,p2));         /* check save envp is also correct  */
	  t->tflags = rtflgs & ~stpFlag;  /* save flags & clr stpFlag in tcb  */
	  restore_last_eval_frame(t,p1);  /* set tcb values from stack frame  */
	  tcbCache(t);                    /* set runtime context from tcb     */
	}
	alpsOutsln(alpserr,"User break exit to main...");
	opcd = s_bail;                    /* bail out completely              */
	goto alcnt;
      }
      /* Level i sought >= 0; move stack and env to appropriate level         */
      p1 = restore();                     /* get old level                    */
      j  = (isNil(p1)) ? -1 : getInt(p1); /* top level is -1                  */
      p2 = findCtl(t,j,s_teval1);         /* eval frame at old level          */
      p4 = findCtl(t,i,s_teval1);         /* eval frame at new level          */
      if (isNil(p4)) {                    /* Can't find new level (too low) ? */
	p4 = findLastCtl(t,&i,s_teval1);  /* Yes, goto the bottom of the class*/
	if (isNil(p4)) {                  /* nothing lower? stay where you are*/
	  save(p1); goto brk_cont; }      /* restore old level                */
	iexp = mkNum(i);                  /* use level returned by findLastCtl*/
      }
      p3 = p4.pt[-3];                     /* environment ptr at desired level */
      if (j >= 0) tenvp = p2.pt[-3];      /* j>=0 -> current level is not top */
      else      { tenvp = envp;      p2 = stkp; } /* current level is on top  */
      setEnv(restore( ));                         /* current saved environment*/
      ASSERT(isEqq(envp,tenvp));/*must be the same as in the eval stack frame */
      if (i < j) { crank_up(p2,p4);    swap_up(envp,p3);   } /* going up      */
      else       { crank_down(p2,p4);  swap_down(envp,p3); } /* going down    */
      setEnv(p3);                     /* Note crank operations may set envp ! */
      save(envp);                     /* new current environment              */
      save(iexp);                     /* current level flag from user         */
      strcpy(bpb,"brk");              /* build prompt for current level       */
      strcat(bpb,(char *)itos(i));    /* "brkNN#: " where NN is current level */
      strcat(bpb,"#: ");              
      r.s.bprompt = alps_make_prompt(bpb); /* set break prompt to level prompt*/
      p2 = p4.pt[-1];                      /* iexp at current level           */
      setVal(a.v.hex,p2);                  /* set #EX to iexp at current level*/
      alpsPrint(alpsout,p2,true,opcd);     /* show it to the user             */
      alpsOutc(alpsout,eol);
      goto brk_cont;
    } //iexp is num
    if (isNil(iexp) || isDot(iexp) || isDash(iexp)) { /* is break over ?     */
      p1 = restore();                   /* break level flag                  */
      if (isDot(iexp)) fSet(stpFlag);   /* on our way out  we are stepping   */
      if (isDash(iexp)) fSet(stpFlag | resFlag); /* or stepping with result  */
      if (isNil(p1)) { /* at top level => step or  continue normally         */
	p1   = restore();               /* env once                          */
	ASSERT(isEqq(envp,p1));         /* videmus                           */
	p1   = restore();               /* env twice                         */
	ASSERT(isEqq(envp,p1));         /* current env better equal top env  */
	iexp = restore();               /* current expression saved on entry */
	setVal(a.v.hex,restore());      /* restore original #EX              */
	restore_res();                  /* restore original result value     */
	goto alret;                     /* leaving through s_brkout          */
      } else { /* abnormal exit: slink out at current level                  */
	/* Note that all unwind-protect forms between the top level and the  */
	/* current level at which we resume execution will be ignored!       */
	ASSERT(isNum(p1) && isfSet(dbgFlag)); /* brk level corruption test   */
	setVal(a.v.hex,getStk(-3));     /* restore original #EX              */
	i  = getInt(p1);                /* integer version of level          */
	p1 = findCtl(t,0,s_brkout);     /* find exit point                   */
	ASSERT(!isNil(p1));             /* if we are here we must find it    */
	alpsSetPrompt(p1.pt[-1]);       /* restore user prompt               */
        p1 = findCtl(t,i,s_teval1);     /*find corresponding eval stack frame*/
	if (isNil(p1)) error(pgmr_inc,"break level inconsistency");
	p3 = p1.pt[-3];                 /* env of eval stack frame at level  */
        ASSERT(isEqq(envp,p3));         /* check that envp is correct here   */
	p2 = restore();                 /* saved envp at this level          */
	ASSERT(isEqq(envp,p3));         /* check save envp is also correct   */
	t->tflags =  rtflgs & ~stpFlag; /* save rtflags & clr stpFlag in tcb */
	restore_last_eval_frame(t,p1);  /* restore tcb runtime context       */
	tcbCache(t);                    /* set runtime context from tcb      */
	opcd  = s_teval;                /* go and re-evaluate iexp in frame  */
	thaw();                         /* everybody is back in the game     */
	goto alcnt;
      }
    }
    if (isRsbrc(iexp))      { showEnvStk(alpsout,envp); }
    else if (isRpar(iexp))  { tcbFlush(t); showCtl(alpsout,t->cntptr); }
    else if (isRcbrc(iexp)) { 
      alpsOutsn(alpsout,3,"tid : ",itos(t->tid),"\n");
      /* Some tcb stuff buried in stk */
      alpsOutsn(alpsout,3,"envp: ",itox(getStk(-2).wh),"\n");
      alpsOutsn(alpsout,3,"envb: ",itox(t->envbase.wh),"\n");
      alpsOuts(alpsout,"iexp: "); dprtln(alpsout,getStk(-3),72);
      alpsOuts(alpsout,"parm: ");
      if (isList(t->parm))   dprtln(alpsout,t->parm,72);
      else                   showEnv(alpsout,t->parm);
      alpsFlush(alpsout);
      alpsOuts(alpsout,"res : "); dprtln(alpsout,getStk(-5),72);
    }
    goto brk_cont;

  case s_brkout:
    alpsSetPrompt(restore());    /* restore user prompt                    */
    restoreDest();
    thaw();                      /* send all players back on to the field  */
    goto alret;

  case s_clock: /* fsubr wall time */
    p1  = allocv(num, 0, r.s.lds);
    *p1.vr->varadd.np = mkrtime();
    sendRes(p1);
    goto alret;

  case s_date: /* subr */
    {struct tm *expt;
     time_t now;
    if (nargs(parm) == 1)
      if (nels(arg1(parm)) != 1) error(inv_funarg,"(date [num:1])");
      else now = arg1(parm).vr->varadd.np[0];
    else now = time(NULL);
    expt = localtime(&now);
    lnd = 1;
    *r.s.lds.ip = 10;
    p1 = allocv(num, lnd, r.s.lds);
    p2 = p1.vr->varadd;
    p2.np[0] = expt->tm_sec;      /* Seconds.     [0-60]   */
    p2.np[1] = expt->tm_min;      /* Minutes.     [0-59]   */
    p2.np[2] = expt->tm_hour;     /* Hours.       [0-23]   */
    p2.np[3] = expt->tm_mday;     /* Day.         [1-31]   */
    p2.np[4] = expt->tm_mon;      /* Month.       [0-11]   */
    p2.np[5] = expt->tm_year;     /* Year - 1900.          */
    p2.np[6] = expt->tm_wday;     /* Day of week. [0-6]    */
    p2.np[7] = expt->tm_yday;     /* Days in year.[0-365]  */
    p2.np[8] = expt->tm_isdst;    /* DST.         [-1/0/1] */
#ifdef LINUX
    p2.np[9] = expt->tm_gmtoff;   /* Seconds east of UTC.  */
#else
    p2.np[9] = idadd;
#endif
    }
    sendRes(p1);
    goto srend;

  case s_time:  /* fsubr process times */
    { struct rusage rv;
      *r.s.lds.ip = 5;
      p1          = allocv(num, 1, r.s.lds);
      p2          = p1.vr->varadd;
#ifdef NO_RT
      struct timeval mtime;
      if (gettimeofday(&mtime,NULL)) alpsError("gettimeofday fail");
      p2.np[0] = 
	mtime.tv_sec + (number)mtime.tv_usec * 1e-6; /* includes sys */
      p2.np[1] = p2.np[2] = p2.np[3] = p2.np[4] = idadd;
#else
      struct timespec mtime;
      if (clock_gettime(MYCLOCK_ID,&mtime)) alpsError("gettime fail");
      p2.np[0] = 
	mtime.tv_sec + (number)mtime.tv_nsec * 1e-9; /* includes sys */
#endif
      if (getrusage(RUSAGE_SELF,&rv)) error(ios_err,"rusage");
      p2.np[1] = 
	rv.ru_utime.tv_sec + (number)rv.ru_utime.tv_usec / 1000000.0;
      p2.np[2] = 
	rv.ru_stime.tv_sec + (number)rv.ru_stime.tv_usec / 1000000.0;
      if (getrusage(RUSAGE_CHILDREN,&rv)) error(ios_err,"rusage");
      p2.np[3] = 
	rv.ru_utime.tv_sec + (number)rv.ru_utime.tv_usec / 1000000.0;
      p2.np[4] = 
	rv.ru_stime.tv_sec + (number)rv.ru_stime.tv_usec / 1000000.0;
      sendRes(p1);
    }
    goto alret;

  case s_stats:
#if (STATS)
    if (nargs(parm) == 0) sendRes(make_stats());
    else if (isNil(arg1(parm))) stats_stop();
    else stats_start();
#else
    error(not_imp,"stats");
#endif
    goto alret;

  case s_wait:
    { pointer lex,tt;
      int lfd,nfound,nfds = 0;
      fd_set fdsel[3];
      struct timeval timeout;

      p1 = arg1(parm);     /* time to wait, <0 => don't wait on time  */
      p2 = cargn(1,parm);  /* tcb's to wait for                       */
      p3 = cargn(2,parm);  /* no fils to wait for                     */
      t->stime = 0.0;      /* no timer wait by default                */
      ntemp    = getNum(p1);
      /* scan done list in order of increasing completion time */
      for (tt=disptab[done];!isNil(tt); tt=tt.tb->next) {
	for (lex=p2;!isNil(lex);lex=cdr(lex)) { /* scan tcb's */
	  p4 = car(lex);
	  if (!isTcb(p4)) error(inv_funarg,"Expected task in task list\n");
	  if (isEqq(p4,tt)) { /* found earliest completer we are waiting for */
	      sendRes(cons(p4,p4.tb->fres));
	      goto srend; /* no point in waiting */
	  }
	}
      }

      if (!isNil(p3)) { /* quick check whether there areany files to wait on */
	bzero(fdsel,sizeof(fdsel)); 
	for (lex=p3;!isNil(lex);lex=cdr(lex)) { /* scan fils */
	  p4 = car(lex);
	  if (!isFil(p4)) error(inv_funarg,"Expected file in file list\n");
	  lfd = p4.fb->fobpvt.fi->fd;
	  if (lfd < 0) error(inv_funarg,"File not open\n");
	  FD_SET(lfd,&fdsel[0]);
	  if (lfd > nfds) nfds = lfd;
	  if (exceptcb[lfd]) FD_SET(lfd,&fdsel[2]);
	}
	timeout.tv_usec = 0;
	timeout.tv_sec  = 0;
	nfound = select(nfds+1,
			(fd_set *)(&fdsel[0]),
			(fd_set *)(&fdsel[1]),
			(fd_set *)(&fdsel[2]),
			&timeout);
	if (nfound > 0) {
	  for (lex=p3;!isNil(lex);lex=cdr(lex)) { /* scan fils */
	    p4 = car(lex);
	    if (FD_ISSET(p4.fb->fobpvt.fi->fd,&fdsel[0]) ||
		FD_ISSET(p4.fb->fobpvt.fi->fd,&fdsel[2])) {
	      sendRes(cons(p4,pnil));
	      goto srend; /* gotta get outta here */
	    }
	  }
	}
      }
    }
    if (ntemp == 0.0) { /* no wait option */
      if (!isNil(p2) || !isNil(p3)) { /* no requested tcb's or fil's ready */
	sendRes(pnil);
	goto srend;
      } else { /* no list, just yield */
	sendRes(getVal(a.v.hai)); /* result is zero */
	disposition = tyld;
      }
    } else {
    /* if time>0 wait for time and tcbs/fils, time<0 only wait on tcbs/fils */
      if (ntemp > 0.0) t->stime  = ntemp + mkrtime(); /* set timer wait  */
      else if (isNil(p2) && isNil(p3)) { /* time<0 & no tcbs/fils to wait for */
	sendRes(pnil);
	goto srend;
      }
      t->tcbwq    = p2;     /* tcb list if any */
      t->filwq    = p3;     /* fil list if any */
      disposition = swait;
    }
    opcd = s_nop;
    goto alout;
    
  case s_dpret:  /*return to dispatcher from pre_empt*/
    opcd      = retstk();   /* yield at point of suspension         */
#if (DEBUG)
    runtcb.tb->lpope  = opcd;   /* last point of pre-emption          */
    runtcb.tb->lpictr = iexctr; /* where in the stream is the pope  ? */
#if (SYSTRACE & TASKONLY)
    alpsOutsn(alpserr,5,"P",itos(runtcb.tb->tid),":",evtab[opcd].lispName," ");
#endif
#endif
     disposition = tyld;
     goto alout;

  case s_freeze:   freeze(t);   goto alret; /* fsubr */
  case s_thaw:     thaw();      goto alret; /* fsubr */
 
  case s_pv: /* fsubr */
    alpsPrintVersion();
    sendRes(ptru);
    goto alret;

  case s_error: /* subr */ 
    if (nargs(parm)!=1) error(inv_numarg,"(error <string>)");
    else error(user_err,(char *)mkString(arg1(parm),tline,"User defined"));
    break; 

  case s_bye: /* fsubr */
    shutting_down = true;
    savewspc      = true; /* force save */
    opcd          = s_nop;
    disposition   = tyld;
    sendRes(ptru);
    goto alout;  

  case s_quit: /* subr */
    alpsExitCode   = (nargs(parm) == 1) ? getWhole(arg1(parm)) : 0;
    end_of_program = true;
    savewspc       = false; /* force no-save */
    opcd           = s_nop;
    disposition    = tyld;
    sendRes(ptru);
    goto alout;  

  case s_exit: /* aux */
    end_of_program = true;
   /* default save wspc flag */
    savewspc    = (pnil.pt == getVal(a.v.hsw).pt)? false : true;
    opcd        = s_nop;
    disposition = tyld;
    goto alout;  

  case s_dbg: /* fsubr */
    len = listLen(iexp);
    if (len==1) {
      save((isfSet(dbgFlag)) ? ptru : pnil);
      setDbg();
      iexp = car(iexp);
      goreto(eval,s_dbgout);
      goto alcnt;
    } else  error(inv_numarg,"(dbg <sexp>)");
    break;

  case s_dbgout:
    fClr(stpFlag | resFlag);
    if (isNil(restore())) { /* old state was dbgFlag clear  */
      clrDbg();             /* Tell Sukey to it off again   */
    } else ;                /* were and still are debugging */
    goto alret;
    
  case s_trace: sendRes(fFlp(trcFlag) ? ptru : pnil);   goto alret;

    /********** LISP Control exotica **********/
  case s_if: /* fsubr */
    if (!isList(iexp) || (listLen(iexp) < 2))
      error(inv_funarg,"(if COND THEN [ELSE])");
    save(iexp);
    iexp = car(iexp);
    goreto(eval, s_if1); 
    break;

  case s_if1: /* aux */
    iexp = cdr(restore());
    if (isNil(getRes())) {
      iexp = mcdr(iexp);
      if (isNil(iexp)) { sendRes(pnil); goto alret; }
      opcd = s_evlis;
    } else {
      iexp = car(iexp);
      opcd = eval;
    }
    break;

  case s_when: /* fsubr */
    if (!isList(iexp) || (listLen(iexp) < 2))
      error(inv_funarg,"(when COND BODY)");
    save(iexp);
    iexp = car(iexp);
    goreto(eval, s_when1); 
    break;

  case s_when1: /* aux */
    iexp = cdr(restore());
    if (isNil(getRes())) { sendRes(pnil);  goto alret; }
    opcd = s_evlis;
    break;

  case s_unless: /* fsubr */
    if (!isList(iexp) || (listLen(iexp) < 2))
      error(inv_funarg,"(unless COND BODY)");
    save(iexp);
    iexp = car(iexp);
    goreto(eval, s_unless1); 
    break;

  case s_unless1: /* aux */
    iexp = cdr(restore());
    if (!isNil(getRes())) { sendRes(pnil);  goto alret; }
    opcd = s_evlis;
    break;

  case s_while: /* fsubr */
    if (!isList(iexp) || (listLen(iexp) < 1))
      error(inv_funarg,"(while <cond> <exp>*)");
    save(iexp); 
    save(pnil);             /* result of last exp        */
    iexp = car(iexp);
    goreto(eval, s_while1); /* eval condition first time */
    break;

  case s_while1: /* aux */
    if (isNil(getRes())) { restore_res(); iexp = restore();  goto alret; }
    restore_res();
    iexp = cdr(getTos());
    goreto(s_evlis, s_while2);
    break;

  case s_while2: /* aux */
    iexp = car(getTos());   /* condition exp                   */
    save_res();             /* save result of last exp in body */
    goreto(eval, s_while1); /* eval condition again            */
    break;

  case s_repeat: /* fsubr */
    if (!isList(iexp) || (listLen(iexp) < 1))
      error(inv_funarg,"(repeat <cond> <exp>*)");
    save(iexp);
    iexp = cdr(iexp);
    goreto(s_evlis, s_repeat1); /* eval body first time */
    break;

  case s_repeat1: /* aux */
    iexp = car(getTos()); /* condition exp                   */
    save_res();           /* save result of last exp in body */
    goreto(eval, s_repeat2);
    break;

  case s_repeat2: /* aux */
    if (!isNil(getRes())) { restore_res(); iexp = restore(); goto alret; }
    restore_res();
    iexp = cdr(getTos());
    goreto(s_evlis, s_repeat1); /* eval body again */
    break;
    
     /********** List accessors ********** all subr's */
  case s_car:   sendRes          (mcar(arg1(parm)));                goto srend;
  case s_cdr:   sendRes          (mcdr(arg1(parm)));                goto srend;
  case s_caar:  sendRes     (mcar(mcar(arg1(parm))));               goto srend;
  case s_cadr:  sendRes     (mcar(mcdr(arg1(parm))));               goto srend;
  case s_cdar:  sendRes     (mcdr(mcar(arg1(parm))));               goto srend;
  case s_cddr:  sendRes     (mcdr(mcdr(arg1(parm))));               goto srend;
  case s_caaar: sendRes(mcar(mcar(mcar(arg1(parm)))));              goto srend;
  case s_caadr: sendRes(mcar(mcar(mcdr(arg1(parm)))));              goto srend;
  case s_cadar: sendRes(mcar(mcdr(mcar(arg1(parm)))));              goto srend;
  case s_caddr: sendRes(mcar(mcdr(mcdr(arg1(parm)))));              goto srend;
  case s_cdaar: sendRes(mcdr(mcar(mcar(arg1(parm)))));              goto srend;
  case s_cdadr: sendRes(mcdr(mcar(mcdr(arg1(parm)))));              goto srend;
  case s_cddar: sendRes(mcdr(mcdr(mcar(arg1(parm)))));              goto srend;
  case s_cdddr: sendRes(mcdr(mcdr(mcdr(arg1(parm)))));              goto srend;
  case s_nth:   sendRes(getNth(arg1(parm),arg2(parm)));             goto srend;
  case s_nthcdr:sendRes(getNthCdr(arg1(parm),arg2(parm)));          goto srend;
     /********** LISP monadic predicates **********/
  case s_atom:  sendRes(isAtom(arg1(parm))         ? ptru : pnil);  goto srend;
  case s_consp: sendRes(isCons(arg1(parm))         ? ptru : pnil);  goto srend;
  case s_listp: sendRes(isList(arg1(parm))         ? ptru : pnil);  goto srend;
  case s_nlistp:sendRes(isList(arg1(parm))         ? pnil : ptru);  goto srend;
  case s_null:  sendRes( isNil(arg1(parm))         ? ptru : pnil);  goto srend;
  case s_chrp:  sendRes(isaChr(arg1(parm))         ? ptru : pnil);  goto srend;
  case s_nump:  sendRes( isNum(arg1(parm))         ? ptru : pnil);  goto srend;
  case s_cpxp:  sendRes( isCpx(arg1(parm))         ? ptru : pnil);  goto srend;
  case s_refp:  sendRes( isRef(arg1(parm))         ? ptru : pnil);  goto srend;
  case s_symp:  sendRes(isSymbol(arg1(parm))       ? ptru : pnil);  goto srend;
  case s_primp: sendRes(isPrim(arg1(parm))         ? ptru : pnil);  goto srend;
  case s_boundp:sendRes(getVal(arg1(parm)).it      ? ptru : pnil);  goto srend;
  case s_zerop: sendRes(ltest(arg1(parm),idadd));                   goto srend;
  case s_onep:  sendRes(ltest(arg1(parm),idprod));                  goto srend;
  case s_oddp:  sendRes(ptest(arg1(parm),idprod));                  goto srend;
  case s_evenp: sendRes(ptest(arg1(parm),idadd));                   goto srend;
    // lisp dyadic predicats
  case s_lt:     sendRes(isLt(arg1(parm),arg2(parm))? ptru : pnil); goto srend;
  case s_le:     sendRes(isLe(arg1(parm),arg2(parm))? ptru : pnil); goto srend;
  case s_eq:     sendRes(isEq(arg1(parm),arg2(parm))? ptru : pnil); goto srend;
  case s_ne:     sendRes(isEq(arg1(parm),arg2(parm))? pnil : ptru); goto srend;
  case s_ge:     sendRes(isGe(arg1(parm),arg2(parm))? ptru : pnil); goto srend;
  case s_gt:     sendRes(isGt(arg1(parm),arg2(parm))? ptru : pnil); goto srend;
  case s_eql:    sendRes(isEql(arg1(parm),arg2(parm))?ptru:pnil);   goto srend;
  case s_neql:   sendRes(isEql(arg1(parm),arg2(parm))?pnil:ptru);   goto srend;
  case s_equal:  sendRes(isEqual(arg1(parm),arg2(parm))?ptru:pnil); goto srend;
  case s_nequal: sendRes(isEqual(arg1(parm),arg2(parm))?pnil:ptru); goto srend;
  case s_memq:   sendRes(memq(arg1(parm), arg2(parm)));             goto srend;
  case s_member: sendRes(member(arg1(parm), arg2(parm)));           goto srend;
  case s_subset: sendRes(subset(arg1(parm), arg2(parm)));           goto srend;

    // list constructors
  case s_cons:  sendRes(cons(arg1(parm), arg2(parm)));              goto srend;
  case s_acons: sendRes(acons(parm));                               goto srend;
  case s_list:
    p1 = getfs(len=nargs(parm));
    sendRes(p1);
    for (i=0;i<len;i++) {p1.rg->ar = argn(i,parm); p1 = p1.rg->dr; }
    goto srend;
  case s_liststar:
    len = nargs(parm);
    if (len == 1) sendRes(arg1(parm));
    else {
      p1 = getfs(len - 1);
      sendRes(p1);
      for (i=0;i<len-1;i++) {p1.rg->ar = argn(i,parm); p2 = p1; p1 = p1.rg->dr;}
      p2.rg->dr = argn(len-1,parm);
    }
    goto srend;
  case s_append: sendRes(append(nargs(parm), parm));                 goto srend;
  case s_pairlis:sendRes(pairlis(nargs(parm),parm));                 goto srend;

     /********** LISP logical connectives **********/

  case s_and:     sendRes(ptru);                                     goto and1;
  case s_or:      sendRes(pnil);                                     goto or1;
  case s_nand:    goreto(s_and,s_negres);                            goto alcnt;
  case s_nor:     goreto(s_or, s_negres);                            goto alcnt;
  case s_not:     sendRes(isNil(arg1(parm)) ? ptru : pnil);          goto srend;
  case s_negres:  sendRes(isNil(getRes())   ? ptru : pnil);          goto alret;

  case s_and1:
    iexp = cdr(restore());
    if (isNil(getRes()))  goto alret;
  and1:
    if (isNil(iexp))  goto alret;
    save(iexp);
    iexp = car(iexp);
    goreto(eval, s_and1);
    goto alcnt;

  case s_or1:
    iexp = cdr(restore());
    if (!isNil(getRes())) goto alret;
  or1:
    if (isNil(iexp)) goto alret; 
    save(iexp);
    iexp = car(iexp);
    goreto(eval, s_or1);
    break;

  
     /********** Property list primitives **********/

  case s_put:     sendRes(putp(arg1(parm), arg2(parm), arg3(parm))); goto srend;
  case s_get:     sendRes(getp(arg1(parm), arg2(parm)));             goto srend;
  case s_remprop: sendRes(remp(arg1(parm), arg2(parm)));             goto srend;

     /********** LISP push and pop  **********/

  case s_pop:     sendRes(popit (arg1(parm)));                       goto srend;
  case s_push:    sendRes(pushit(arg1(parm),arg2(parm)));            goto srend;

     /********** LISP mapping functions **********/

#if (EXP)
#define CKPARM								\
    if (isRsvd(parm)) {   /*  old bindings have been captured   */	\
      release(restore()); /* release hold on reserved parm      */	\
      INCCT(envct,6);							\
      parm = allocEnv(len);						\
      hold(parm);							\
      save(parm);                                                       \
    }
#else
#define CKPARM								\
    release(restore()); /* unconditionally release hold on parm      */	\
    parm = allocEnv(len);						\
    hold(parm);								\
    save(parm);
#endif

  case s_find:
  case s_findc:
    sendRes(pnil); /* default result */
    if (nargs(parm) != 3) error(inv_numarg,"(find[c] <fun> <list> <res>)");
    iexp = arg1(parm);     /* <fun> function */ 
    p1   = arg2(parm);     /* <list> */
    p2   = arg3(parm);     /* <res> comparison value */
    if (isNil(p1)) goto alret;
    if (!isCons(p1)) error(inv_funarg,"(find[c] <fun> <list> <res>)");
    parm = allocEnv(1);
    hold(parm);
    save(parm);            /* new parm block   */
    save(p2);              /* comparison value */
    save(p1);              /* list */ 
    save(iexp);            /* fun  */ 
    if (opcd == s_find) {sarg1(parm, p1);      goreto(s_sys_apply, s_find1);  }
    else                {sarg1(parm, car(p1)); goreto(s_sys_apply, s_findc1); }
    break;

  case s_find1:
  case s_findc1:
    iexp = restore();      /* fun  */
    p1   = restore();      /* list */
    if (isEq(getRes(), getTos())) { sendRes(p1);    goto findout; }
    if (isNil(cdr(p1)))           { sendRes(pnil);  goto findout; }
    if (!isCons(p1)) error(inv_funarg,"(find[c] <fun> <list> <res>)");
    p1 = cdr(p1);
    parm = getStk(-1); // lurking just below the surface
    ASSERT(!isRsvd(parm));
    sarg1(parm,(opcd == s_find1) ? p1 : car(p1));
    save(p1);
    save(iexp);
    goreto(s_sys_apply, opcd);
    break;
  findout:
    popStk(2);           /* comparison value and new parm block */
    goto alret;
    
  case s_gen:
  case s_genc: /* chase arg list(s) applying arg1 iexp while res [not] nil*/
  case s_ngen:
  case s_ngenc:
    sendRes(pnil);
    if ((len = nargs(parm)) < 2) map_error(inv_numarg,opcd);
    for (i=1;i<len;i++) {
      p1   = argn(i,parm);
      if (isNil(p1)) goto alret;
      if (!isCons(p1)) map_error(inv_funarg,opcd);
    }
    p1   = parm;  /* arg2-len of lists */
    parm = allocEnv(len-1);
    pushStk(len-1);                /* place for groovers of arglists   */
    for (i=1;i<len;i++) {          /* assume compiler can opt this     */
      setStk(1-i,(p2=argn(i,p1))); /* copy heads of lists to be mapped */
      sargn(parm,i-1,((opcd == s_genc) || (opcd == s_ngenc)) ? p2.rg->ar : p2);
    }
    iexp = arg1(p1);
    save(iexp);
    hold(parm);  /* hold on to parm block - don't return in unbind */
    save(parm);
    goreto(s_sys_apply, (opcd - s_gen) + s_gen1);
    break;

  case s_gen1:
  case s_genc1:
  case s_ngen1:
  case s_ngenc1:
    parm = getTos();
    len  = nargs(parm);
    if ((opcd == s_gen1) || (opcd == s_genc1)) {
      if (isNil(getRes())) goto genout;
    } else { 
      if (!isNil(getRes())) goto genout;
    }
    CKPARM;
    for (i=0;i<len;i++) {            /* For all groovers        */
      p2 = getStk(-i-2).rg->dr;      /*   move groover along    */
      if (isNil(p2)) goto genout;    /*   res is what it is     */
      if (!isCons(p2)) map_error(inv_funarg,opcd);
      setStk(-i-2,p2);               /*    save moved groover    */
      sargn(parm,i,((opcd==s_genc1) || (opcd==s_ngenc1)) ? p2.rg->ar : p2);
    }
    iexp = getStk(-1);
    goreto(s_sys_apply, opcd);  /* come back to where you came from */
    break;
  genout:
    release(parm);             /* release hold on parm   */
    popStk(len+2);             /* parm, iexp  & groovers */
    goto alret; 
    
  case s_map:
  case s_mapc:
    sendRes(pnil);
    if ((len = nargs(parm)) < 2) map_error(inv_numarg,opcd);
    p1   = parm;
    parm = allocEnv(len-1); 
    for (i=1;i<len;i++) {          /* assume compiler can opt this     */
      p2 = argn(i,p1);
      if (isNil(p2)) goto alret;
      if (!isCons(p2)) map_error(inv_funarg,opcd);
      setStk(len-i,p2); /* copy heads of lists to be mapped */
      sargn(parm,i-1,(opcd == s_mapc) ? p2.rg->ar : p2);
    }
    pushStk(len-1);
    iexp = arg1(p1);
    save(iexp);
    hold(parm);  /* don't return in unbind */
    save(parm);
    goreto(s_sys_apply, (opcd == s_mapc) ? s_mapc1 : s_map1);
  break;

  case s_map1:
  case s_mapc1:
    parm = getTos();
    len  = nargs(parm);
    CKPARM;
    for (i=0;i<len;i++) { 
      p2 = getStk(-i-2).rg->dr;    /* get and move groover along      */
      if (isNil(p2)) {             /* this groover has hit the end    */
	release(parm); 
	popStk(len+2);             /* drop parm, iexp and groovers    */
	sendRes(pnil);             /* map[c] returns nil              */
	goto alret;                /* and ciao for now                */ 
      }
      if (!isCons(p2)) map_error(inv_funarg,opcd);
      setStk(-i-2,p2);
      sargn(parm,i,(opcd == s_mapc1) ? p2.rg->ar : p2);
    }
    iexp = getStk(-1);
    goreto(s_sys_apply, opcd);
    break;
    
  case s_mapn:
  case s_mapcan:
    if ((len=nargs(parm)) < 2) map_error(inv_numarg,opcd);
    sendRes(pnil);
    p1   = parm;                   /* p1 standing in for old parm      */   
    parm = allocEnv(len-1);        /* One arg for each argument list   */
    for (i=1;i<len;i++) {          /* assume compiler can opt this     */
      p2 = argn(i,p1);
      if (isNil(p2)) goto alret;
      if (!isCons(p2)) map_error(inv_funarg,opcd);
      setStk(len-i,p2);            /* copy heads of lists to be mapped */
      sargn(parm,i-1,(opcd == s_mapcan) ? p2.rg->ar : p2);
    }
    pushStk(len-1);                /* place for groovers of arglists   */
    iexp = arg1(p1);
    save(iexp);                    /* save iexp to apply on top for now */
    hold(parm);                    /* don't return in unbind for re-use */
    save(parm);
    goreto(s_sys_apply, (opcd == s_mapcan) ? s_mapcan1 : s_mapn1);
    break;

  case s_mapn1:
  case s_mapcan1:
    parm = getTos();
    len  = nargs(parm);
    for (i=0;i<len;i++) {       /* For each groover                */
      p2 = getStk(-i-2).rg->dr; /*   move groover along            */
      if (isNil(p2)) {          /*   this groover's hit the end so */
	release(parm);          /*     release hold on parm        */
	popStk(len+2);          /*     pop parm, iexp and groovers */
	goto alret;             /*     and it's all over baby blue */
      }                         
      if (!isCons(p2)) map_error(inv_funarg,opcd);
      setStk(-i-2,p2);
    }
    CKPARM;
    for (i=0;i<len;i++) {   
      p2 = getStk(-i-2);
      sargn(parm,i,(opcd == s_mapcan1) ? p2.rg->ar : p2);
    }
    p3 = getRes();
    if (isNil(p3)) { /* Do not have a non nil result yet */
      iexp = getStk(-1);
      goreto(s_sys_apply, opcd);
    } else {            /* got first result onto which we will nconc the rest */
      parm = restore(); /* pop off parm to stick the other guys underneath    */
      iexp = getTos();  /* -3 iexp                                            */
      save(p3);         /* -2 bury the head of result list (final result)     */
      save(p3);         /* -1 this will be the current result slot            */
      save(parm);       /*  0 save what you will need to use again            */
      goreto(s_sys_apply, (opcd == s_mapcan1) ? s_mapcan2 : s_mapn2); 
    }
    break;

  case s_mapn2:
  case s_mapcan2:
    p2   = getStk(-1);              /* current result */
    p3   = getRes();
    if (!isNil(p3))  nconc2(p2,p3); /* tack res on previously currrent result*/
    else p3 = p2;                   /* else no change, we will save p3 as cr */
    parm = getTos();
    len  = nargs(parm);
    CKPARM;
    for (i=0;i<len;i++) {   
      p2 = getStk(-i-4).rg->dr; /* get groover from stack and move along */
      if (isNil(p2)) {          /* this groover has hit the end of list  */
	sendRes(getStk(-2));    /* send our buried final result          */
	release(parm);          /* release hold on parm                  */
	popStk(len+4);          /* parm, cr, fr, iexp & groovers         */
	goto alret;             /* and we are out of here                */
      }
      if (!isCons(p2)) map_error(inv_funarg,opcd);
      setStk(-i-4,p2);          /* update groover on stack               */
      sargn(parm,i,(opcd == s_mapcan2) ? p2.rg->ar : p2); /* update slot */
    }
    setStk(-1,p3);              /* udpate current result                 */
    iexp = getStk(-3);          /* iexp just under final result          */
    goreto(s_sys_apply,opcd);   /* and just do it again please baby!     */
    break;

  case s_mapcar:
  case s_maplist:
    sendRes(pnil);
    if ((len=nargs(parm)) < 2) map_error(inv_funarg,opcd);
    temi = fs_size;       /*  max possible list len */
    for (i=1;i<len;i++) { /* first arg is function  */
      p1 = argn(i,parm);
      if (isNil(p1)) goto alret;
      j = listLen(p1);        /* check for valid lists is done here    */
      if (j < temi) temi = j; /* find shortest list argument           */
    }
    p2 = getfs(temi);              /* create result list               */
    save(p2);                      /* save result                      */
    saveDest();                    /* bury until we are done           */
    dest = allocDest(0);           /* for argp->FS                     */
    setDest(p2);                   /* argp -> newly copied list        */
    p1 = parm;                     /* will be using old parm as p1     */   
    parm = allocEnv(len-1);        /* One arg for each list            */
    pushStk(len-1);                /* place for groovers of arglists   */
    for (i=1;i<len;i++) {          /* assume compiler can opt this     */
      setStk(1-i,(p2=argn(i,p1))); /* copy heads of lists to be mapped */
      ASSERT(isCons(p2));
      sargn(parm,i-1,(opcd == s_mapcar) ? p2.rg->ar : p2);
    }
    iexp = arg1(p1);
    save(iexp);
    hold(parm);                     /* in case we can re-use him       */
    save(parm);
    goreto(s_sys_apply, (opcd == s_mapcar) ? s_mapcar1 : s_maplist1);
    break;

  case s_mapcar1:
  case s_maplist1:
    parm = getTos();
    len  = nargs(parm);
    if (isNil(fstep_dest())) { /* move dest to next of res list */
      zap_dest();
      release(parm);
      popStk(len+2);       /* pop off parm,iexp & groovers */
      restoreDest();       /* restore res                  */
      sendRes(restore());  /* and send result              */
      goto alret;
    }
    CKPARM;
    for (i=0,j=-2;i<len;i++,j--) {
      p2 = getStk(j).rg->dr; /* move groovers along */
      setStk(j,p2);
#if (0)
      if (isNil(p2)) error(inv_funarg,"unexpected end of list");      
      if (!isCons(p2)) error(inv_funarg,"list structure got mangled");
#endif
      if (isNil(p2))
	alpsOuts(alpserr," arggg\n");
      sargn(parm,i,(opcd == s_mapcar1) ? p2.rg->ar : p2);
    }
    iexp = getStk(-1);
    goreto(s_sys_apply, opcd);
    break;
 
  case s_len:    sendRes(mkNum(listLen(arg1(parm))));                goto srend;
  case s_assoc:  sendRes(uassoc(arg1(parm), arg2(parm)));            goto srend;
  case s_last:   sendRes(last(nargs(parm), parm));                   goto srend;
  case s_revl:   sendRes(revl(nargs(parm), parm));                   goto srend;
  case s_rotl:   sendRes(rotl(nargs(parm), parm));                   goto srend;
  case s_pos:    sendRes(position(arg1(parm), arg2(parm)));          goto srend;
  case s_subst:  sendRes(subst(arg1(parm), arg2(parm), arg3(parm))); goto srend;
  case s_num:    sendRes(lnum(arg1(parm)));                          goto srend;
  case s_chr:    sendRes(lchr(arg1(parm)));                          goto srend;
  case s_faddr:  sendRes(mkNum(arg1(parm).wh));                      goto srend;
  case s_mint:   sendRes(mint(arg1(parm), arg2(parm)));              goto srend;
  case s_mkprc:  sendRes(mkprc(parm));                               goto srend;
  case s_rdda:   sendRes((pointer)getWhole(arg1(parm)));             goto srend;
  case s_peep:   sendRes(dopeep(parm));                              goto srend;
  case s_mget:   sendRes(mget(parm));                                goto srend;
  case s_mput:   sendRes(mput(parm));                                goto srend;
  case s_sysp:   sendRes(mkNum(arg1(parm).at->sysp.wh));             goto srend;
  case s_arg:    sendRes(arg1(parm));                                goto srend;
  case s_incr:   sendRes(incr(car(iexp)));                           goto alret;
  case s_decr:   sendRes(decr(car(iexp)));                           goto alret;

  case s_sort:   
    if ((nargs(parm) != 2) || (!isList(arg2(parm))))
	error(inv_funarg,"(sort <fun> <list>)");
    p1 = arg2(parm);   /* list to be sorted                 */
    if (listLen(p1) <= 1) { sendRes(p1); goto srend; }
    save(iexp);        /* for debug of srend                */
    save(p1);          /* protect the whole unsorted list   */
    save(pnil);        /* tells merge it is done            */
    save(p1);          /* tape read head of state machine   */
    iexp = arg1(parm); /* function to apply for comparisons */
    reserve(parm);     /* going to be giving it to apply    */
    /* drop through */
  case s_rsrt:
    p1 = restore();     /* tape read head */
    if (isNil(p1))      {  opcd = s_merge; goto alcnt; }
    if (isNil(cdr(p1))) { 
      save(cons(p1.rg->ar, pnil)); opcd = s_merge; goto alcnt;
    }
    sarg1(parm,car(p1));
    sarg2(parm,cadr(p1)); 
    save(p1);
    save(iexp);
    save(parm);
    goreto(s_sys_apply, s_rsrt1);
    break;

  case s_rsrt1:
    parm = restore();
    iexp = restore();
    p1   = restore();
    save(!(isNil(getRes())) ? list(car(p1), cadr(p1)) : list(cadr(p1),car(p1)));
    save(cddr(p1)); /* work on rrest of list */
    opcd = s_rsrt;
    break;

  case s_merge:
    p2 = restore();
    p1 = restore();
    if (isNil(p1)) { 
      sendRes(p2); 
      restore();   /* unprotect unsorted list */
      iexp = restore();
      unReserve(parm);
      goto srend;
    }
    save(p1);
    save(p2);
    sarg1(parm,car(p1));
    sarg2(parm,car(p2)); 
    save(iexp);
    save(parm);
    goreto(s_sys_apply, s_merge1);
    break;

  case s_merge1: 
    parm = restore();
    iexp = restore();
    p2   = restore();
    p1   = restore();
    if (!isNil(getRes())) {
      save(p1); // result list starts here
      save(p1); // continues here
      save(cdr(p1));
      save(p2);
    } else {
      save(p2);
      save(p2);
      save(p1);
      save(cdr(p2));
    }
    /* drop through */

  case s_merge2:
    p2 = restore();
    p1 = restore();
    if (isNil(p1)) {// tack on p2 and goto merge
      p3 = restore(); // current pos
      p3.rg->dr = p2;
      opcd = s_merge;
      goto alcnt;
    }
    if (isNil(p2)) {// tack on p1 and goto merge
      p3 = restore(); // current pos
      p3.rg->dr = p1;
      opcd = s_merge;
      goto alcnt;
    }
    save(p1);
    save(p2);
    sarg1(parm,car(p1));
    sarg2(parm,car(p2)); 
    save(iexp);
    save(parm);
    goreto(s_sys_apply, s_merge3);
    break;

  case s_merge3:
    parm = restore();
    iexp = restore();
    p2   = restore();
    p1   = restore();
    if (!isNil(getRes())) {
      p3 = restore(); // current pos
      p3.rg->dr = p1;
      save(p1);       // new pos
      save(cdr(p1));
      save(p2);
    } else {
      p3 = restore(); // current pos
      p3.rg->dr = p2;
      save(p2);      // new pos
      save(p1);
      save(cdr(p2));
    }
    opcd = s_merge2;
    break;

  case s_implod:
    len = nargs(parm);
    p1  = argn(len-1,parm);
    if (!isList(p1)) error(inv_funarg,"implod needs list");
    if (isNil(p1))
      if ((len > 2) && !isNil(arg2(parm)))  sendRes(r.s.noref); /* null ref vector */
      else sendRes(r.s.nonum); /* null vector */
    else sendRes(implode(p1,               // list
			 len>1 ? arg1(parm) : pnil, // axis
			 isList(car(p1))  ? ref : vrtype((car(p1))), 
			 len==3 ? arg2(parm) : pnil));
    goto srend;

  case s_explod:  sendRes(explode(nargs(parm),parm));            goto srend;
  case s_sqrt:    sendRes(alps_sqrt(arg1(parm)));                goto srend;
  case s_modexp:  sendRes(alps_modExp(nargs(parm),parm));        goto srend;
  case s_int:     sendRes(alpsInt(arg1(parm)));                  goto srend;
  case s_frc:     sendRes(alpsFrc(arg1(parm)));                  goto srend;
  case s_isint:   sendRes(alpsIsInt(arg1(parm)));                goto srend;
 /* the list hackers aka the boys with the pearly white teeth */
  case s_delete:  sendRes(delete_from_list(arg1(parm), arg2(parm))); goto srend;
  case s_rplaca:  sendRes(setCar(arg1(parm),arg2(parm)));            goto srend;
  case s_rplacd:  sendRes(setCdr(arg1(parm),arg2(parm)));            goto srend;
  case s_nconc:   sendRes(nconc(parm));                              goto srend;
  case s_nrev:    sendRes(nrevList(arg1(parm)));                     goto srend;
    /* A. P. Hellish lads */ 
  case s_dist:
  case s_plus:
  case s_minus:
  case s_times:
  case s_divid:
  case s_ceil:
  case s_floor:
  case s_resid:
  case s_power:
  case s_log:
  case s_trig:
  case s_binom:
  case s_andd:
  case s_xor:
  case s_vor:
  case s_nandd:
  case s_nord:
  case s_great:
  case s_eqapl:
  case s_greatq:
  case s_lessn:
  case s_lessq:
  case s_neapl:  sendRes(execute(opcd,nargs(parm),parm));  goto srend;
  case s_roll:   sendRes(roll        (nargs(parm),parm));  goto srend;
  case s_nott:   sendRes(alps_not    (nargs(parm),parm));  goto srend;
  case s_shape:  sendRes(shape       (nargs(parm),parm));  goto srend;
  case s_ravel:  sendRes(ravel       (nargs(parm),parm));  goto srend;
  case s_cat:    sendRes(catenate    (nargs(parm),parm));  goto srend;
  case s_gup:
  case s_gdn:    sendRes(grade(opcd,  nargs(parm),parm));  goto srend;
  case s_iota:   sendRes(iota        (nargs(parm),parm));  goto srend;
  case s_rev:    sendRes(reverse     (nargs(parm),parm));  goto srend;
  case s_transp: sendRes(transpose   (nargs(parm),parm));  goto srend;
  case s_minv:   sendRes(minv        (nargs(parm),parm));  goto srend;
  case s_komp:   sendRes(kompress    (nargs(parm),parm));  goto srend;
  case s_expnd:  sendRes(expand      (nargs(parm),parm));  goto srend;
  case s_take:   sendRes(take        (nargs(parm),parm));  goto srend;
  case s_drop:   sendRes(drop        (nargs(parm),parm));  goto srend;
  case s_rot:    sendRes(rotate(nargs(parm),parm,0));      goto srend;
  case s_shft:   sendRes(rotate(nargs(parm),parm,1));      goto srend;
  case s_decod:  sendRes(decod       (nargs(parm),parm));  goto srend;
  case s_encod:  sendRes(encod       (nargs(parm),parm));  goto srend;
  case s_elmnt:  sendRes(element     (nargs(parm),parm));  goto srend;
  case s_index:  sendRes(alpsIndex   (nargs(parm),parm));  goto srend;
  case s_reduc:
  case s_scan:   sendRes(redscan(opcd,nargs(parm),parm));  goto srend;
  case s_inrp:   sendRes(innerp      (nargs(parm),parm));  goto srend;
  case s_outp:   sendRes(outerp      (nargs(parm),parm));  goto srend;
  case s_rank:   sendRes(mkNum((number)dims(arg1(parm)))); goto srend;    
  case s_tally:  sendRes(tally(arg1(parm)));               goto srend;    
  case s_search: sendRes(search      (nargs(parm),parm));  goto srend;
  case s_four:
  case s_ifour: 
#if (COMPLEX)
    sendRes(fourier(opcd,nargs(parm),parm));  goto srend;
#else
    error(not_imp,"no complex support");
#endif
  case s_iset:   sendRes(iset        (nargs(parm),parm));  goto srend;
  case s_aref:   sendRes(aref        (nargs(parm),parm));  goto srend;
  case s_aset:   sendRes(aset        (nargs(parm),parm));  goto srend;

    /* Operating system interface primitives */
  case s_sys: // Execute system command
    if (daemonic) error(not_imp,"sys socket");
    i = format(nargs(parm), parm, tline);
    if (i < max_line_len) tline[i] = '\0'; /* FIXME */
    ntemp = getNumVal(a.v.hwd); /* Remember watchdog */
    setNumval(a.v.hwd,0);
    if (system((char *)tline) == 0) sendRes(ptru);   /*a la unix*/
    else                    sendRes(pnil);
    setNumval(a.v.hwd,ntemp);
    goto srend;

  case s_dl: // directory list
    if (nargs(parm) == 0) p1 = chartab[point];
    else if (nargs(parm) != 1) error(inv_funarg,"dl");
    else p1 = arg1(parm);
    sendRes(dodir(p1));
    goto srend;

  case s_getenv: // get environment variable
    { char *mEnvv;
      if (nargs(parm) != 1) error(inv_numarg,NULL);
      if ((!isaChr(p1=arg1(parm))) || (p1.vr->varels >= max_line_len))
	error(inv_funarg,NULL);
      mkString(p1,tline,"Env var name");
      mEnvv = getenv((char *)tline);
      sendRes((mEnvv == NULL) ? pnil :  buildChr(mEnvv,strlen(mEnvv)));
    }
    goto srend;
    
  case s_setenv: // set environment variable value
    { int retval;
      if (nargs(parm) != 2) error(inv_numarg,NULL);
      if ((!isaChr(p1=arg1(parm))) || (nels(p1) >= max_line_len) ||
	  (!isaChr(p2=arg2(parm))) || (nels(p2) >= max_line_len))
	error(inv_funarg,NULL);
      mkString(p1,tline,"Env var name");
      mkString(p2,tline1,"Env var value");
      retval = setenv((char *)tline,(char *)tline1,true);
      sendRes((retval) ? pnil : ptru);
    }
    goto srend;
    
  case s_wd: // working directory
    if (!getcwd((char *)tline,max_line_len)) error(ios_err,strerror(errno));
    sendRes(buildChr((char *)tline,strlen((char *)tline)));
    if (nargs(parm) == 0) ; // structured programming oblige
    else if (nargs(parm) == 1) {
      if ((isaChr(p1=arg1(parm))) && (p1.vr->varels < max_line_len))  {
	mkGString(p1,tline);
	if (chdir((char *)tline)) error(bad_file,"chdir");
      } else error(inv_funarg,"chdir");
    } else error(inv_numarg,NULL);
    goto srend;

  case s_pu: // purge file
    if (nargs(parm) != 1) error(inv_numarg,NULL);
    if ((!isaChr(p1=arg1(parm))) || (p1.vr->varels >= max_line_len))
      error(inv_funarg,NULL);
    mkGString(p1,tline);
    if (unlink((char *)tline)) error(bad_file,"unlink");
    sendRes(p1);
    goto srend;

  case s_rn: // rename file
    if (nargs(parm) != 2) error(inv_numarg,"(rn <oldname> <newname>)");
    if ((!isaChr(p1=arg1(parm))) || (p1.vr->varels >= max_line_len) ||
	(!isaChr(p2=arg2(parm))) || (p2.vr->varels >= max_line_len))
      error(inv_funarg,NULL);
    {
      struct stat from,to;
      char *fromfile = (char *)tline;
      char *tofile   = (char *)tline1;
      mkGString(p1,(uchar *)fromfile);
      mkGString(p2,(uchar *)tofile);
      sendRes(p2);
      if (!lstat(tofile,&to))    error(inv_funarg,"<newname> exists");
      if (lstat(fromfile,&from)) error(inv_funarg,"<oldname> not found");
      /* if same inode on same device do nothing */
      if ((from.st_dev == to.st_dev) && (from.st_ino == to.st_ino)) goto srend;
      if (link(fromfile,tofile)) error(bad_file,"link");
      if (unlink (fromfile) && errno != ENOENT) {
	  unlink (tofile);
	  error(bad_file,"can't unlink <oldname>");
      }
    }
    goto srend;
  
  case s_st: // file status
    if (nargs(parm) != 1) error(inv_funarg,"st");
    sendRes(dofstat(arg1(parm)));
    goto srend;

  case s_fmt:
    len = format(nargs(parm), parm, tline);
    sendRes(mkChr(tline,len));
    goto srend;

  case s_intern:
    p1 = arg1(parm);
    if (!isaChr(p1)) error(inv_funarg,NULL);
    if (isSym(p1)) sendRes(p1);
    sendRes(makeAtom(p1.vr->varadd.sp,p1.vr->varels, s_var));
    goto srend;

  case s_setqq: /* fsubr */
    p1 = car(iexp);
    p2 = cadr(iexp);
    sendRes(p2);
    ck_ass(p1);
    setSym(p1,p2);
    goto alret;

  case s_spec:
  case s_setq: /* fsubr */
    if (isNil(iexp)) { sendRes(pnil); goto alret; }
    ck_ass(car(iexp));
    save(iexp);
    if (isNil(cdr(iexp))) sendRes(pnil);
    else { // checking for isConst here is slower since most cases are not
      iexp = cadr(iexp);
      goreto(eval, s_setqq0);
      goto alcnt;
    }
    /* drop through */

  case s_setqq0: /* aux */
    iexp = restore();
    p1 = car(iexp);
    ASSERT(isSym(p1));
    setSym(p1,getRes());
    if (!isNil(cdr(iexp)) && !isNil(cddr(iexp))) {
      iexp = cddr(iexp);
      opcd = s_setq;
      goto alcnt;
    }
    goto alret;

  case s_set: /* subr */
    p1 = arg1(parm);
    ck_ass(p1);
    setSym(p1,arg2(parm));
    sendRes(arg2(parm));
    goto srend;

    /* lisp function definition */
  case s_defun: /* fsubr */
    checkDef(iexp,"(de Name (Arg*) Body)");
    setSym(car(iexp),cons(s[s_lambda],cdr(iexp)));
    sendRes(car(iexp));
    goto alret;

  case s_defexpr: /* fsubr */
    checkDef(iexp,"(df Name (Arg*) Body)");
    setSym(car(iexp),cons(s[s_fexpr],cdr(iexp)));
    sendRes(car(iexp));
    goto alret;

  case s_defmacro: /* fsubr */
    checkDef(iexp,"(dm Name (Arg*) Body)");
    setSym(car(iexp),cons(s[s_macro],cdr(iexp)));
    sendRes(car(iexp));
    goto alret;

  case s_defclos: /* fsubr */
    p1 = car(iexp); 
    if (!isList(p1)) error(inv_funarg,"(dc (Name Args+) Body)");
    p2 = car(p1);
    checkFunName(p2);
    p3 = cdr(p1); /* argument list */
    checkSymList(p3);
    save(p4 = list(s[s_lambda],p3)); /* protect */
    p4.rg->dr.rg->dr = cdr(iexp);
    setSym(p2,list3(s[s_fnarg],p4,envp));/* make & send funarg triple*/
    pop1();
    reserveChain(envp);  /* batten the hatches below */
    sendRes(p2);
    goto alret;

  case s_defconst: /* fsubr */
    if (!isCons(iexp) || !isSym(p1=car(iexp)) || (!isVar(p1) && !isCon(p1)))
      error(inv_funarg,"expected (defconst symbol value)");
    save(car(iexp));
    if (!isConst(iexp = cadr(iexp))) {
	goreto(eval, s_defcn0);
	goto alcnt;
    }
    sendRes(iexp);
    /* drop thru */
    
  case s_defcn0:
    p1 = restore();
    ASSERT(isSym(p1));
    setGSym(p1,getRes());
    p1.at->spl = s_con;
    sendRes(p1);
    goto alret;
    
  case s_var:
    error(sys_cor,"how did we get here");
    break;

    /************* user interface to interpreter *************/
  case s_apply: /* subr */
    if ((nargs(parm) < 2) || !isList(argn(nargs(parm)-1,parm)))
      error(inv_funarg,"apply wants: function arg .. list");
    iexp = arg1(parm); /* function to apply */
    
  case s_apply1:
    if (isSym(iexp)) {
      switch (evtab[iexp.at->spl].lispEval) {
      case subr:  /* put all args into an env block */
	parm = mkApplyEnvParm(parm);
	opcd = s_sys_apply;
	goto alcnt;
      case fsubr:  /* put all args into a list */
	opcd = iexp.at->spl; /* execute directly */
	iexp = mkApplyListParm(parm);
#if (CKARGS)
	if (!chkList(iexp)) error(inv_funarg,"bad list");
#endif	
	goto alcnt;
      case evar:
	iexp = lookup(iexp);  /* look up value of variable symbol */
	opcd = s_apply1;
	goto alcnt;
      case expr:
      case fexpr:
      case econ: 
	error(inv_fun,(char *)mkString(iexp,eline,evtab[opcd].lispName));
      default:	 error(pgmr_inc,evtab[iexp.at->spl].lispName);
      }
    }
    if (isConst(iexp)) error(inv_fun,"Attempt to apply constant as function");
    ASSERT(isCons(iexp));
    p1 = car(iexp);
    if (isSym(p1)) {
      switch (evtab[p1.at->spl].lispEval) {
      case expr:   /* s_cont & s_lambda and s_labelonly */
	parm = mkApplyEnvParm(parm);
	opcd = s_sys_apply;
	goto alcnt;
      case fexpr:  /* s_fexpr, s_macro and s_fnarg (TODO: test fnarg) */
	parm = mkApplyListParm(parm);
	save(parm);
	stkret(s_fxend);
	if (p1.at->spl == s_macro) stkret(s_ev_res); /* must eval res */
	opcd = s_sys_apply;
	goto alcnt;
      case econ:  error(inv_fun,"((Constant..)) application"); 
      default:  stkret(s_apply1);  goreto(eval, s_ret_iexp); goto alcnt;
      }
    } else { /* car iexp not a symbol */
      if (isConst(car(iexp))) error(inv_fun,"constant");
      stkret(s_apply1);
      goreto(eval, s_ret_iexp); 
      goto alcnt;
    }
    break;

  case s_eval:  /* subr */
    iexp = arg1(parm);
    if (nargs(parm) == 1) { opcd = eval; goto alcnt; } /*  simple case       */
    p1 = arg2(parm);
    if (isNil(p1) || (isEnv(p1))) { /* do the env yo-yo thing                */
      p2 = find_active_env(p1); /* find intersection with active chain       */
      swap_down(envp,p2);       /* unbind act chain to point of intersection */
      swap_up(p2,p1);           /* restore bindings for eval                 */
      save(iexp);               /* compatibility with s_fnarg                */
      save2(&envp,p1);          /* save envptr old envptr; new is p1         */
      save(p2);                 /* save point of intersection                */
      save(p1);                 /* save new env for debugr                   */
      goreto(eval, s_eval1e);   /* send old cerberus off on a hunt           */
    } else { /* assume we have assoc list in env */
      for (p2=p1;!isAtom(p2);p2=cdr(p2)) {
	if ((!isCons(car(p2))) || (!isSym(caar(p2))))
	   error(inv_funarg,"Arg 2 expected a-list");
      }
      if (!isNil(p2)) error(inv_funarg,"Arg 2 bad a-list termination");
      abind(p1);   /* its the a-list */
      save(p1);
      goreto(eval, s_eval1a);
    }
    break;

  case s_eval1a:   p1 = restore(); /* alist */   unbinda(p1);   goto alret;

  case s_eval1e:            /* Demeter rejoices; s_fnarg comes here!*/
    p1 = restore();         /* funarg / eval binding env            */
    ASSERT(isEqq(p1,envp)); /* stacked copy for debugr              */
    p2 = restore();         /* restore POI                          */
    swap_down(envp,p2);     /* undo provided eval bindings          */
    setEnv(restore());      /* restore old environment pointer      */
    swap_up(p2,envp);       /* restore bindings                     */
    iexp = restore();       /* saved in s_eval and s_fnarg          */
    goto alret;

  case s_fnarg:  /* entered from s_sys_apply for expr, from eval0 for fexpr */
    if (listLen(iexp) != 3) error(inv_funarg,"invalid closure");
    p1 = caddr(iexp);         /* this is the wrapped env   */
    if (!isEnv(p1)) error(inv_funarg,"invalid env in closure");
    save2(&iexp,cadr(iexp));   /* closed function */
    if (isSym(iexp)) {
      if (!isEqq(iexp,s[s_eval]) && !isEqq(iexp,s[s_apply]))
	error(inv_funarg,"bad func in closure");
    } else if (!isList(iexp)  || isNil(iexp) || !isSym(car(iexp)))
	error(inv_funarg,"bad func in closure");
    if (isActive(p1))  { /* funarg still bound. go below decks */
      swap_down(envp, p1);
      save2(&envp,p1);         /* need to swap back to here */
      stkret(s_fnarg1);
      goto fnargout;
    } else { /* funarg unbound:  Demeter grieves for Persephone */
      p2 = find_active_env(p1); /* find intersection with active chain */
      swap_down(envp,p2); /* unbind active chain to point of intersection */
      swap_up(p2,p1);     /* restore ancient binding */
      save2(&envp,p1);    /* save old env            */
      save(p2);           /* save POI                */
      save(p1);           /* save new env for debugr */
      stkret(s_eval1e);
      fnargout:
      if (isCons(iexp)) {
	p1 = mcar(iexp);
	ASSERT((evtab[(integer)p1.at->spl].lispEval == expr) ||
	       (evtab[(integer)p1.at->spl].lispEval == fexpr));
      } else {
	ASSERT(isEqq(iexp,s[s_eval]) || isEqq(iexp,s[s_apply]));
      }
      opcd = s_sys_apply;
    }
    break;

  case s_fnarg1:
    p1   = envp;
    setEnv(restore());
    swap_up(p1, envp);
    iexp = restore();
    goto alret;

  case s_let: /* fsubr */
    if (isNil(iexp)) { sendRes(pnil); goto alret; }
    if (!isList(mcar(iexp))) error(inv_funarg,"Expected variable-list");
    stkret(s_unbind);
  in_let:
    len  = listLen(mcar(iexp));
    saveDest();
    dest = allocDest(len);
    initDest(dest);
    save(iexp);
    iexp = mcar(iexp); /* binding candidates */
    if (isNil(iexp)) { alps_link(dest); opcd = s_let_end;  goto alcnt; }
    opcd = s_let0;
    /* dropping thru */

  case s_let0:
    p1 = car(iexp);   /* p1 -> head let binding */
    if (isAtom(p1)) {    /* no val, bind to nil    */
      if (!isSym(p1)) error(con_ass,NULL);
      sendToSymSlot(p1);
      sendRes(isQuadvar(p1) ? lookup(p1) : pnil);
      iexp = cdr(iexp);
      if (isNil(iexp)) { swap(dest); alps_link(dest); opcd = s_let_end; }
      else               next_dest();
      goto alcnt;
    }
    p2 = car(p1);
    if (!isSym(p2)) error(con_ass,NULL);
    sendToSymSlot(p2);
    if (isNil(cdr(p1))) { /* nil initialiser */
      sendRes(isQuadvar(p2) ? lookup(p2) : pnil);
      iexp = cdr(iexp);            /* next binding */
      if (isNil(iexp)) { 
	swap(dest); alps_link(dest); opcd = s_let_end;
      }  else next_dest();
      goto alcnt;
    }
    save(iexp);
    iexp = mcar(mcdr(p1));
    if (isConst(iexp))    sendRes(iexp);  /* drop through */
    else if (isSym(iexp)) sendRes(lookup(iexp)); /*drop through */
    else  { goreto(eval,s_let1); goto alcnt; }

  case s_let1:
    iexp = cdr(restore());   /* saved in let0 */
    if (!isNil(iexp)) { next_dest(); opcd = s_let0; goto alcnt;}
    swap(dest);
    alps_link(dest);
    /* drop through */

  case s_let_end:
    iexp = cdr(restore()); /* saved in s_let[*] now pointing to body */
    zap_dest();
    restoreDest();
    sendRes(pnil);       /* in case of empty body */
    if (s_prog_end == (opcd = retstk())) { /* see goreto(evlis,pc) below */
      popStk(3);        /* hurl:  iexp, envp and s_nop                   */     
      stkret(s_unbind); /* when we come here dest has been linked        */
      save(envp);       /* it is now in envstack via link(). need for go */
      save(iexp);       /* headless prog body                            */
    }
    goreto(s_evlis,opcd); /* evlis body, return to caller stacked opcode */
    break;

  case s_letstar:
    if (isNil(iexp)) { sendRes(pnil); goto alret; }
    if (!isList(mcar(iexp))) error(inv_funarg,"Expected variable-list");
    stkret(s_unbind);
  in_letstar:
    len = listLen(mcar(iexp));
    saveDest();
    dest = allocDest(len);
#if (BINDING==DEEP || BINDING==DEEPC)
    // Prevent lookup from finding bindings in progress in letstar
    // Not needed for SHALLOW since lookup does not look at this envblock
    for (i=0;i<len;i++) dest.ev->args[i].ar = pnil; 
#endif
    initDest(dest);
    alps_link(dest);  /* oooooh??? aaaahhhh... aaargghh!!! */
    save(iexp);
    iexp = mcar(iexp);
    if (isNil(iexp)) { opcd = s_let_end; goto alcnt; }
    opcd = s_letstar0; /* drop through */

  case s_letstar0:
    p1 = car(iexp);   /* p1 -> head let binding */
    if (isAtom(p1)) {
      if (!isSym(p1)) error(con_ass,NULL);
      sendRes(isQuadvar(p1) ? lookup(p1) : pnil);
      sendToSymSlot(p1); // sent to symslot after lookup for deep binder
      cacheSymVal(p1,getRes());   // dest env is bound DEEP
      swap_dest();
      iexp = cdr(iexp);
      if (isNil(iexp)) { opcd = s_let_end; goto alcnt; }
      next_dest();
      goto alcnt;
    }
    p2 = car(p1);
    if (!isSym(p2)) error(con_ass,NULL);
    if (isNil(cdr(p1))) {   /* iexp in list no val */
      sendRes(isQuadvar(p2) ? lookup(p2) : pnil);
      sendToSymSlot(p2); // sent to symslot after lookup for deep binder
      cacheSymVal(p2,getRes());
      swap_dest();
      iexp = iexp.rg->dr;            /* next binding */
      if (isNil(iexp)) opcd = s_let_end;
      else             next_dest();
      goto alcnt;
    }
    save(iexp);
    iexp = mcar(mcdr(p1));
    if (isConst(iexp))  sendRes(iexp);  /* drop through */
    else if (isSym(iexp)) sendRes(lookup(iexp)); /* drop through */
    else { goreto(eval,s_letstar1);  goto alcnt; }

  case s_letstar1:
    iexp = restore();
    p1   = caar(iexp);
    sendToSymSlot(p1); // send to symslot after eval for deep binder
    cacheSymVal(p1,getRes());
    swap_dest();
    iexp = cdr(iexp);
    if (isNil(iexp)) opcd = s_let_end;
    else            { opcd = s_letstar0; next_dest(); }
    break;

  case s_prog:
    if (isNil(iexp)) { sendRes(pnil); goto alret; }
    if (!isList(mcar(iexp))) error(inv_funarg,"Expected variable-list");
    stkret(s_nop);
    save(envp);
    save(iexp);
    stkret(s_prog_end); /* see let_end and yes, its very cheap and ugly */
    goto in_let;

  case s_progstar:
    if (isNil(iexp)) { sendRes(pnil); goto alret; }
    if (!isList(mcar(iexp))) error(inv_funarg,"Expected variable-list");
    stkret(s_unbind);
    save(envp);
    save(iexp);
    stkret(s_prog_end); /* see let_end and yes... */
    goto in_letstar; 

  case s_prog_end:
    popStk(1); /* hurl iexp body (used by goto) */
    if (!isEnv(restore())) error(sys_cor,"stack out of sync");
    goto alret; /* hit s_unbind in normal case */

  case s_return:
    if (nargs(parm) > 1) error(inv_numarg,"(return [<exp>])");
    p1 = findCtl(t,0,s_prog_end);
    p2 = (nargs(parm)) ? arg1(parm) : pnil; /* return nil by default */
    if (isNil(p1)) error(inv_fun,"return outside of prog");
    t->recstkp = p1;
    save(p2);                   /* current result */
    goreto(s_ratchetd,s_ret1);  /* climb down the stack cleaning up as we go */
    break;

  case s_ret1:
    p2 = restore();       /* current res                               */
    p1 = restore();       /* drop past prog_end marker                 */
    ASSERT(p1.wh == s_prog_end);
    popStk(1);           /* pop the body needed for goto               */
    p1   = restore();    /* return the env you have stolen from me     */
    swap_down(envp,p1);
    setEnv(p1);
    sendRes(p2);
    goto alret; /* nop or unbind */

  case s_go:
    if (nargs(parm) != 1) error(inv_numarg,"(go <exp>)");
    p1 = arg1(parm);
    if (isRef(p1)) {
      if (nels(p1)==0) goto alret; /* don't go anywhere i.e. next expression */
      p1 = p1.vr->varadd.pt[0];
    }
    if (!isSym(p1)) error(inv_funarg,"go wants symbol");
  goto_again:      /* ASSERT p1 not changed */
    p2 = findCtl(t,0,s_prog_end);
    if (isNil(p2)) error(inv_funarg,"no target found for go");
    t->recstkp = p2;     /* This is the marker for the descender to get to */
    save(p1);            /* Save Label, restored in s_go1                   */
    goreto(s_ratchetd,s_go1);
    break;

  case s_go1:
    p1 = restore();                      /* Label */
    p2 = getTos(); 
    ASSERT(p2.wh == s_prog_end);
    p3   = getStk(-1);                   /* body parts */
    while (!isNil(p3) && !isEqual(p1,car(p3))) p3 = cdr(p3); /* find label */
    if (isNil(p3)) { popStk(3); goto goto_again; }      /* not found again */
    p4   = getStk(-2);    /* the env     */
    ASSERT(isEnv(p4));
    swap_down(envp,p4); /* restore env */
    setEnv(p4);
    iexp = cdr(p3); /* body spot to continue from */
    opcd = s_evlis;
    break;

  case s_catch: /* fsubr */
    if (listLen(iexp) < 2) error(inv_numarg,"(catch <tag> <exp>+)");
    save(iexp);
    iexp = car(iexp);
    goreto(eval,s_catch1); /* tag exp */
    break;

  case s_catch1:
    p1 = getRes();
    if (!isSym(p1)) error(inv_funarg,"catch tag expected symbol");
    iexp = cdr(restore());
    saveDest();
    save(envp);
    save(getRes()); /* save the tag result */
    goreto(s_evlis,s_catch2);
    break;

  case s_catch2:
    (void)restore(); /* throw away tag */
    if (envp.pt != restore().pt) error(sys_cor,"catch2 bad stack");
    restoreDest();
    goto alret;

  case s_throw: /* subr */
    if (nargs(parm) != 2) error(inv_numarg,"(throw <tag> <exp>)");
  throw_some_more:
    p1 = findCtl(t,0,s_catch2);
    if (isNil(p1)) error(inv_funarg,"throw missed catch");
    t->recstkp = p1;    /* This is the marker to ratchet down to */
    save(parm);
    goreto(s_ratchetd,s_throw1);
    break;

  case s_throw1:
    parm = restore();
    p1 = restore();  /* seen this catch2 */
    ASSERT(p1.wh == s_catch2);
    p2 = restore(); /* the tag of the current catch */
    if (isEq(p2,arg1(parm))) {
      p1   = restore(); /* saved envptr */
      restoreDest();
      swap_down(envp,p1);
      setEnv(p1);
      sendRes(arg2(parm)); /* result of throw */
      goto alret;
    }
    goto throw_some_more;
    
  case s_bail:
    p1 = findCtl(t,0,s_condcase1);
    if (isNil(p1)) {            
      showError(t);                  /* no error handlers to call so shout */
      t->recstkp = t->base;          /* and rappel down to the very bottom */
      save(pnil);                    /* save default result                */
      goreto(s_ratchetd,s_bailout);  /* and come back for final swap_down  */
    } else {
      t->recstkp = p1;               /* Err handler to ratchet down to     */
      save(pnil);
      goreto(s_ratchetd,s_condcase2);/* and come back for handler handling */
    }
    break;
    
  case s_bailout:
    swap_down(envp,t->envbase);
    setEnv(t->envbase);
    opcd = s_done;
    break;

  case s_condcase: /* fsubr */
    if (listLen(iexp) < 3) 
      error(inv_numarg,"(condition-case <var> <protected-form> <handlers>+)");
    if (!isSym(car(iexp))) error(inv_funarg,"<var> not symbol");
    p1 = cddr(iexp);
    while (!isNil(p1)) {
      if (!isList(car(p1))) error(inv_funarg,"<handler> not list");
      p1 = cdr(p1);
    }
    save(car(iexp));                  /* save var                          */
    save(cddr(iexp));                 /* save handlers                     */
    save(envp);                       /* save environment for handlers     */
    iexp = cadr(iexp);                /* evaluate the body                 */
    goreto(eval,s_condcase1);
    break;

  case s_condcase1:                   /* get here only with error-free body*/
    popStk(3);                        /* pop saved envp,handlers and var   */
    goto alret;                       /* Res is that of bod                */

  case s_condcase2:                   /* here on error in condcase body    */
    popStk(1);                        /* evaluate condcase handlers        */
    p1 = restore();                   /* Came here from s_ratchetd looking */
    ASSERT(p1.wh == s_condcase1);     /* for s_condcase1 in t->recstkp     */
    p1 = restore();                   /* restore envp end environment      */
    swap_down(envp,p1);
    setEnv(p1);
    p2 = restore();                   /* restore handlers                   */
    p3 = restore();                   /* the dreaded var                    */
    while (!isNil(p2)) {              /* look for matching handler          */
      if (isEqual(caar(p2),errSym[t->errflg])) break;
      if (isList(caar(p2)) && !isNil(memq(errSym[t->errflg],caar(p2)))) break;
      p2 = cdr(p2);
    }
    if (isNil(p2)) {
      opcd = s_bail;                  /* handler not found, bail some more  */
      goto alcnt;
    } 
    if (!isNil(p3)) {                 /* If we have a var bind error info   */
      p1 = cons(errSym[t->errflg], mkChr((uchar *)t->errinf,strlen(t->errinf)));
      save(p1);
      alps_bind1(p3,p1);              /* binding with argp==0               */
      pop1();
    } else {
      p4 = allocEnv(0);               /* null envblock to keep things simple*/
      alps_link(p4);                  /* link to active environment         */
    }
    t->errflg = no_err;               /* error has been "handled"           */
    iexp = cdar(p2);                  /* body of handler                    */
    goreto(s_evlis,s_unbind);
    checkStkExt(t->cntptr.ct);        /* Disable stack extension if not used*/
    setTcbStkLim(t);
    break;
        
  case s_uwdprot: /* fsubr */
    save(mcdr(iexp));                 /* start of the cleanup forms        */
    save(envp);                       /* env to restore for cleanup        */
    iexp = mcar(iexp);                /* body becomes expression           */
    goreto(eval,s_uwdp1);             /* eval exp and come back to cleanup */
    break;

  case s_uwdp1:
    p1   = restore();                 /* saved env                         */
    iexp = restore();                 /* cleanup forms                     */   
    save_res();                       /* remember result of body           */
    goreto(s_evlis,s_uwdp2);          /* preform cleanup                   */
    break;

  case s_uwdp2:
    restore_res();                    /* re-instate result from body       */
    goto alret;

  case s_ratchetd: /* Ratchet down stack for unwind-protect & condition-case */
    p1   = t->recstkp;               /* target position in the stack         */
    opcd = retstk();                 /* operation to return to               */
    p2   = restore();                /* current result or parm               */
    while (!isEqq(stkp,p1)) {        /* target position handled by return op */
      p3 = restore();                /* pop the stack and take a look        */
      if (p3.wh == s_uwdp1) {        /* Is it an unwind-protect frame ?      */
	p4   = restore();            /* Re-instate bindings to point where   */ 
	swap_down(envp,p4);          /* unwind-protect form was evaluated    */
	setEnv(p4);                  /* Environment is here now              */
	p4   = iexp;                 /* Some iexp swizzeling going on        */
	iexp = restore();            /* Unwind-protect clean-up forms        */
	save(p2);                    /* current res or parm                  */
	stkret(opcd);                /* Op to return to                      */
	stkret(s_ratchetd);          /* Come back while target not found     */
	save(p4);                    /* Old iexp                             */
	goreto(s_evlis,s_rs_iexp);   /* Restore iexp after clean up forms    */
	goto alcnt;
      }
      if (p3.wh == s_trailer) { /* restore dest */
	dest = restore();
	p3   = restore();  /* dest.ev->argp */
	ASSERT(p3.pt == dest.ev->argp.pt); // just checking
      } else if (p3.wh == s_eval1e) { /* must be compatible with s_eval1e!  */
	popStk(1) ;             /* skip  saved new env                      */
	p4 = restore();         /* point of intersection with active chain  */
	swap_down(envp,p4);     /* swap down to it                          */
	setEnv( getTos());      /* old env going into the eval              */
	swap_up(p4,envp);       /* restore to state when eval/funarg invoked*/
      } else if (p3.wh == s_brkout) {
	alpsSetPrompt(restore());    /* restore user prompt                */
      } else if (p3.wh == s_dbgout) {
	if (isNil(restore())) clrDbg();
      } else if (p3.wh == s_sys_eval1 || p3.wh == s_teval1) evald--;
    }
    save(p2);
    break;

  case s_cwcc: /* subr */
    reserveChain(envp);               /* protect against parm block replay    */
    reserveChain(dest);               /* dest possibly active in evargs       */
    saveCtx();             /* push iexp parm envp and dest onto current stack */
    iexp = arg1(parm);                /* iexp to pass continuation as arg to  */
    parm = allocEnv(1);               /* simulate evalargs                    */
    sarg1(parm,pnil);                 /* avoid weirdo if boogey entered       */
    save(p1 = saveStack());           /* save whole control stack             */
    sarg1(parm,cons(s[s_cont],p1));   /* build new continuation in arg1       */
    pop1();                           /* release protect on saved stack       */
    goreto(s_sys_apply,s_cwcc1);      /* goto sys_apply and come back to cwcc1*/
    break;                      

  case s_cwcc1:    restoreCtx();    goto alret;
    
  case s_cont:
    p1 = cdr(iexp);              /* control stack pointer              */
    if (!isCnt(p1)) error(inv_fun,"invalid continuation");
    if (nargs(parm) > 1)
      error(inv_funarg,"continuation takes at most 1 parm");
    restoreStack(p1);            /* switch to continuation stack       */
    p1 = envp;                   /* current env for restoreEnv         */
    p2 = (nargs(parm)) ? arg1(parm) : pnil; /* result of cont          */
    restoreCtx();                /* iexp,parm,envptr & dest            */
    restoreEnv(p1,envp);         /* restore bindings                   */
    sendRes(p2);                 /* send result of cont                */
    goto alret;                  /* return to what tos says            */

  case s_peval:	/* subr */
    if (nargs(parm) < 1 || nargs(parm) > 2)
      error(inv_numarg,"(peval <exp> [env])");
    if (nargs(parm) == 2)  {/* got given an env */
      p2 = arg2(parm);
      if (!isNil(p2) && !isEnv(p2))  error(inv_funarg,"(peval <exp> [env])");
    } else p2 = envp; /* By default the child inherits its progenitor's env */
    p1             = allocTcb(ctl_depth);
    p1.tb->iexp    = arg1(parm);
    p1.tb->envp    = p2;
    p1.tb->envbase = p2;
    reserveChain(p2); /* don't release this stuff until no longer reffed */
    scheduleTcb(p1,eval);
    sendRes(p1);
    goto srend;

  case s_getpri:
    sendRes(mkNum(arg1(parm).tb->priority));
    goto srend;

  case s_setpri:
    p1   = arg2(parm);
    temi = getInt(arg1(parm));
    if (temi < TCB_MAX_PRI || temi > TCB_MIN_PRI)
      error(inv_funarg," priory out of range");
    p1.tb->priority = temi;
    if (p1.tb->state == runbl) {
      p2 = remTcb(runbl, p1);
      ASSERT(isEqq(p1,p2));
      enqTcb(runbl,p1);
      /* pre-empt if p1 now has higher priority than current tcb */
      if (temi < t->priority) fSet(pmtFlag);
    }
    sendRes(p1);
    goto srend;
    
  case s_send: /* send expression to tcb for evaluation */
    p1 = arg1(parm); /* expression  */
    p2 = arg2(parm); /* target task */
    p3 = (nargs(parm) == 3) ? arg3(parm) : envp; /* optional env */
    sendRes(p1);
    reserveChain(p3); /* don't let it get dropped on the way out */
    sendTcb(p2,p1,p3);
    goto srend;

  case s_args: /* fsubr */
     p1 = findCtl(t,0,s_fxend);
     if (isNil(p1)) error(inv_funarg,"No fexpr context");
     p1.pt--;
     sendRes(*p1.pt);
     goto alret;
       
     
  case s_fun: /* fsubr - make a closure around the functional argument */
    if (listLen(iexp) != 1) error(inv_funarg,"function takes one argument");
    iexp = car(iexp); /* iexp is the functional argument */
    opcd = s_fun1;
    goto fun1;

  case s_fun1:  /* Evaluate the argument until we find an apply-able function */
    iexp = getRes();
  fun1:
    if (isConst(iexp)) error(inv_funarg,"closure of constant attempted"); 
    if (isSym(iexp)) {
      topcd = iexp.at->spl;
      switch (evtab[topcd].lispEval) {
      case subr:
      case fsubr: 
	if (topcd == s_eval || topcd == s_apply) {
	  /* these girls call lookup - so wrap them up */
	  sendRes(list3(s[s_fnarg],iexp, envp));
	  reserveChain(envp);  /* avoid greedily returning these guys */
	} else { /* other built-ins just get quoted */
	  sendRes(list(s[s_quote], iexp));	
	}
	goto alret;
      case evar:  sendRes(lookup(iexp)); goto alcnt; // TODO: loop protection
      default:	  error(inv_funarg,"closure constant symbol"); 
      }
    } else { /* we have a cons <=> not a const and not a sym */
      p1 = car(iexp); /* check out the function */
      if (isSym(p1)) {
	if (isQuote(p1)) { sendRes(cadr(iexp)); goto alcnt; }
	switch (evtab[p1.at->spl].lispEval) {
	case expr:
	case fexpr: /* function is one of lambda and the boys */
	  if (p1.at->spl == s_fnarg) {
	    /* already closed - do nothing */
	  } else {
	    sendRes(list3(s[s_fnarg],iexp, envp));/* make & send funarg triple*/
	    reserveChain(envp);  /* avoid greedily returning these guys */
	  }
	  goto alret;
	default: /* go figure */
	  goreto(eval, s_fun1);
	  break;
	} 
      } else { /* car of arg not a symbol so hit it with eval */
	goreto(eval, s_fun1);
      }
    }
    break;

    /********* Optimised classic LISP control **********/
  case s_cond:
    if (!isList(iexp)) error(inv_funarg,"list expected");
    sendRes(pnil);                            /* what was the question again? */
    opcd = s_cond_cont;                       /* for looping in on cond_cont  */
    goto cond_head;

  case s_cond_cont:
  cond_cont:
    iexp = restore(); 
    if (!isNil(getRes())) {                  /* Got true cond clause          */
      iexp = cdar(iexp);                     /* body of clause                */
      if (isNil(iexp)) goto alret;           /* no body to eval, leave        */
      else { opcd = s_evlis;  goto evlis1; } /* eval body                     */
    } else  {
      iexp = cdr(iexp);                      /* next clause                   */
  cond_head:	
      if (isNil(iexp)) goto alret;           /* NO TRUE COND CLAUSE           */
      if (!isList(mcar(iexp))) error(inv_funarg,"list expected");
      save(iexp);
      if (isNil(mcar(iexp))) goto alcnt;     /* loop on cond_cont             */
      iexp = caar(iexp);                     /* cond exp of clause            */
      if (isConst(iexp)) { sendRes(iexp);         goto cond_cont; }        
      if (isSym(iexp))   { sendRes(lookup(iexp)); goto cond_cont; }
      goreto(eval, s_cond_cont);             /* eval cond exp                 */
    }
    goto alcnt;

  case s_evlis:  
    if (isNil(iexp)) {
      sendRes(pnil);
      goto alret;
    }
    goto evlis1;

  case s_evlis1:
    iexp = cdr(restore());  /* next expression          */
  evlis1:
    if (isNil(cdr(iexp))) { 
      iexp = car(iexp);     /* eval last exp regardless */
      opcd = eval;          /* and don't come back      */
    } else {
      p1 = car(iexp);
      if (isAtom(p1)) {  
	iexp = cdr(iexp);  /* don't evaluate possibly unbound goto labels */
	goto evlis1;
      } else {
	save(iexp);
	iexp = car(iexp);         /* expression  */
	goreto(eval, s_evlis1);   /* eval exp    */
      }
    }
    goto alcnt;
  
    /********** LISP Interpreter **********/
 case s_sys_apply: /* This guy has become too big */
   if (isSym(iexp)) {
      switch (evtab[iexp.at->spl].lispEval) {
      case subr:
	INCCT(appct,0);
	opcd = iexp.at->spl; 
#if (CKARGS)	
	checkArgs(opcd,parm);
#endif
	break; 
      case fsubr:
	INCCT(appct,1);
	opcd = iexp.at->spl;
#if (CKARGS)	
	 if (!chkList(parm)) error(inv_funarg,"fsubr parms not a list");
#endif
	break;
      case evar: 
	INCCT(appct,2);
	iexp = lookup(iexp); // should not occur ?
	break;
      case expr:
      case fexpr: 
      case econ: 
	error(inv_fun,(char *)mkString(iexp,eline,evtab[opcd].lispName));
      default: error(pgmr_inc,evtab[iexp.at->spl].lispName);
      }
      goto alcnt;
    }
    if (isConst(iexp)) error(inv_fun,"constant");
    /* iexp not atomic */
    p1 = car(iexp);
    if (isSym(p1)) {
      switch (evtab[p1.at->spl].lispEval) {
      case expr:
	INCCT(appct,3);
	if (p1.at->spl==s_lambda) {
	  INCCT(appct,4);
	  //	  tcbFlush(t);
	  //	  showCtl(alpsout,t->cntptr);
#if (TAIL)
  /* stkp->eva_end:dest:dest->argp:trailer:[subr | iexp]:app_ret1:iexp */
  /*             0   -1         -2      -3            -4       -5   -6 */
	  p2 = getStk(-4);                 /* function pending application  */
	  if ((getTos().wh==s_eva_end) &&  /* did we come from eva_last     */
	      isEqq(iexp,getStk(-6))   &&  /* iexp called self or subr */
	      (isEqq(iexp,p2) || isTrrSubr(p2))) { /* args for iexp or subr */
	    INCCT(trrct,0);
	    gypo_bind(parm,envp);
	    save(iexp);
	    iexp = cddr(iexp);
	    goreto(s_evlis,s_app_ret0);    /* 0 unbinds when evlis is done */
	    goto alcnt;
	  } else if ((getTos().wh==s_app_ret1) && 
		     isEqq(iexp,getStk(-1)))      {// app_ret1:iexp
	    INCCT(trrct,1);
	    gypo_bind(parm,envp);
	    iexp = cddr(iexp);
	    opcd = s_evlis;                  // re-use stacked app_ret later;
	    goto alcnt;
	  } else if (getTos().wh == s_app_ret1) { 
	    // looking for [:s_app_ret:iexp?]*:s_app_ret:iexp
	    p3.wp = stkp.wp; /* find previous application of iexp */
	    p4    = envp;
	    while (*p3.wp == s_app_ret1     && 
		   !isEqq(iexp, *(p3.pt-1)) &&
		   (p3.wp > t->cntptr.ct->stack.wp + 3)) {
	      p3.wp -= 2;
	      p4 = nenv(p4);
	    }
	    if (*p3.wp == s_app_ret1 && isEqq(iexp, *(p3.pt-1))) {
	      INCCT(trrct,2);
#if (BINDING==DEEP || BINDING==DEEPC)
	      save(envp);
	      stkret(s_rs_env);
	      setEnv(p4);
	      gypo_bind(parm,envp);
#else 
	      gypo_bind(parm,p4);
#endif
	      iexp = cddr(iexp);
	      opcd = s_evlis;
	      goto alcnt;
	    }
	  }
#endif
	  INCCT(trrct,3);
	  alps_bind(cadr(iexp),parm);
	  save(iexp);                      /* for tail recursion detector */
	  iexp = cddr(iexp);
	  goreto(s_evlis,s_app_ret1);
	} else if (p1.at->spl == s_label) {
	  INCCT(appct,5);
	  p2 = cadr(iexp);               /* label tag                    */
	  iexp = cddr(iexp);             /* body                         */
	  alps_bind1(p2,iexp);           /* bind label tag to body       */
	  ASSERT(car(iexp).at->spl==s_lambda);
	  alps_bind(cadr(iexp),parm);    /* bind parms to formal args    */
	  iexp = cddr(iexp);             /* lambda body                  */	
	  save(iexp);                    /* for tail recursion detector  */
	  goreto(s_evlis,s_app_ret2);    /* 2 unbinds when evlis is done */
	} else {
	  INCCT(appct,6);
	  ASSERT(p1.at->spl == s_cont);
	  opcd = p1.at->spl;
	}
	goto alcnt;
      case fexpr:
	p1 = car(iexp);
	if (p1.at->spl == s_fnarg) {  /* funarg is a special case */
	  INCCT(appct,7);
	  opcd = s_fnarg;
	} else 	if (p1.at->spl == s_fexpr || p1.at->spl == s_macro) {
	  INCCT(appct,8);
	  len = listLen(mcar(mcdr(iexp)));   /* number of formal args  */
	  if ((len-1) > (listLen(parm))) { 
	    error(inv_numarg,
		  mkEmes(4,evtab[p1.at->spl].lispName,
			 " needs at least ",itos(len-1)," arguments"));
	  }
	  p1   = allocEnv(len);       /* crack open a fresh env block       */
	  for (i = 0; i < len-1; i++) {
	    sargn(p1,i,car(parm));    /* put args from list into value slot */
	    parm = mcdr(parm); 
	  }
	  sargn(p1,len-1,parm);       /* rest from list into last slot      */
	  alps_bind(cadr(iexp), p1);/* bind parm to formal args           */
	  iexp = cddr(iexp);        /* body                               */
	  goreto(s_evlis, s_unbind);
	} else error(pgmr_inc,"sys_apply: fexpr exception");
	goto alcnt;
      case econ:  error(inv_fun,"pid?");         break;
      default:    
	INCCT(appct,9);
	save(parm);  
	goreto(eval, s_app_loop); 
      }
    } else {
      if (isConst(p1)) error(inv_fun,"Attempted application of a constant");
      INCCT(appct,10);
      save(parm);
      stkret(s_app_loop);
      goreto(eval, s_ret_iexp);
    }
    break;
    /*sys_apply*/

  case s_app_loop:
    parm = restore();
    opcd = s_sys_apply;
    goto alcnt;

  case s_evargs:
    if (isNil(iexp)) {
      INCCT(argct,0);
      parm = allocEnv(0); 
      iexp = restore();      /* saved in eval0 */
      opcd = s_sys_apply;
      goto alcnt;
    }
    if (!isCons(iexp)) error(inv_funarg,"list expected");
    saveDest();
    dest = allocDest(i = listLen(iexp));
    INCCT(argct,i);
    initDest(dest);   /* first slot in dest is initial target for sendRes() */
    goto eva_go;
  
  case s_eva_do:
    iexp = cdr(restore()); /* saved in s_evargs_do, next arg  */
    next_dest();           /* target next parm slot in dest   */ 
  eva_go:
    if (isNil(cdr(iexp))) goto eva_last; /* last arg to evaluate, TAIL op */
    /*    sendToSymSlot(iexp); could be useful for debug    */
    p1 = car(iexp);
    if (isConst(p1))           { sendRes(p1);         INCCT(evact,0); }
    else if (isSym(p1))        { sendRes(lookup(p1)); INCCT(evact,1); }
    else if (isQuote(car(p1))) { sendRes(cadr(p1));   INCCT(evact,2); }
    else { 
      save(iexp); 
      iexp = car(iexp); 
      goreto(eval,s_eva_do); 
      INCCT(evact,3);
      goto alcnt; 
    }
    iexp = cdr(iexp);      /* next argument                   */
    next_dest();           /* next sendRes() destination slot */  
    INCCT(evact,4);
    goto eva_go;           /* go around again                 */

  eva_last:
    iexp = car(iexp);           /* iexp contains final arg to be evaluated */
    if (isConst(iexp))           { sendRes(iexp);         INCCT(evact,5); }
    else if (isSym(iexp))        { sendRes(lookup(iexp)); INCCT(evact,6); } 
    else if (isQuote(car(iexp))) { sendRes(cadr(iexp));   INCCT(evact,7); } 
    else { 
      goreto(eval,s_eva_end);   /* s_eva_end is TAIL mark */
      INCCT(evact,8);
      goto alcnt;   
    } 

 /* Came from eval or dropped thru from eva_last on Const Symbol and Quote */
  case s_eva_end:
    parm = dest;         /* food for apply                        */
    zap_dest();          /* crash if someone sends anything here  */
    restoreDest();       /* previous dest now receives sendRes()  */
    iexp = restore();    /* saved in eval0 for use in s_sys_apply */
    opcd = s_sys_apply;
    INCCT(evact,9);
    goto alcnt;

    /* here are the expensive but pretty girls */
    
  case s_sys_eval: /* the soul of the machine */
    if (isConst(iexp))      { sendRes(iexp);         INCCT(evct,0); goto alret;}
    if (isSym(iexp))        { sendRes(lookup(iexp)); INCCT(evct,1); goto alret;}
    if (isQuote(car(iexp))) { sendRes(cadr(iexp));   INCCT(evct,2); goto alret;}
#if (TAIL!=true)
    evald++;
    save(iexp);
    stkret(s_sys_eval1);
#endif
#if (RANGE)
    if (!chkList(iexp)) error(inv_funarg,"invalid list");
#endif
    parm = cdr(iexp);
    iexp = car(iexp); 

    opcd = s_eval0;    
    INCCT(evct,3);
    /* Dropping through */

  case s_eval0:
    /* handles function discovery and  dispatch for subr,fsubr,expr & fexpr */
    fClr(wdgFlag);   /* progress is coming past here */
    if (isSym(iexp)) { 
      switch (evtab[iexp.at->spl].lispEval) {
      case subr:  
	INCCT(evct,4);   /* eval subr */
	goto ev0out;
      case fsubr: 
	opcd = iexp.at->spl; /* execute directly and hope it's well behaved! */
	iexp = parm;         /* fsubrs must dig through iexp themselves      */
	INCCT(evct,5);       /* eval fsubr                                   */
	goto alcnt;
      case evar:
	iexp = lookup(iexp);  /* look up value of variable symbol in iexp */
	INCCT(evct,6);        /* eval evar                                */
	goto alcnt;
      case expr:
      case fexpr:
      case econ: 
	error(inv_fun,(char *)mkString(iexp,eline,evtab[opcd].lispName));
      default:	 error(pgmr_inc,evtab[iexp.at->spl].lispName);
      }
    }
    if (isConst(iexp)) error(inv_fun,"Attempt to apply constant as function");
    /* iexp is not atomic */
    p1 = car(iexp);
    if (isSym(p1)) {
      switch (evtab[p1.at->spl].lispEval) {
      case expr:  /* s_lambda s_label and s_cont only */
	p2 = mcdr(iexp);     /* label tag */
#if (RANGE)
	if (p1.at->spl == s_label) {
	  if (!isSym(mcar(p2))) error(inv_funarg,"label tag must be symbol");
	  p2 = mcdr(p2); /* function */
	  if (mcar(p2).at->spl != s_lambda)  
	    error(inv_funarg,"label function must be lambda form");
	  p2 = mcdr(p2); /* args body */ 
	}
	if (isNil(p2) || !isList(mcar(p2)))
	  error(inv_funarg,"formal arguments must be a list");
#endif
	INCCT(evct,7);         /* eval expr */
	goto ev0out;
	break; 
      case fexpr:  /* s_fexpr, s_macro  and s_fnarg */
	if (p1.at->spl == s_fexpr || p1.at->spl == s_macro) {
	  opcd = s_sys_apply;
	  save(parm);
	  stkret(s_fxend);
	  if (p1.at->spl == s_macro) {
	    stkret(s_ev_res);    /* macro must eval res after unbind */
	    INCCT(evct,9);       /* eval macro */
	  } else INCCT(evct,8);  /* eval fexpr */
	} else if (p1.at->spl == s_fnarg) {  /* funarg is a special case */
	  p1 = cadr(iexp); /* check to see if args need to be evalled */
	  if (isSym(p1)) {
	    if (!isEqq(p1,s[s_eval]) && !isEqq(p1,s[s_apply])) 
	      error(inv_funarg,"Unexpected primitive in closure");
	    goto ev0out;
	  } else if (!isCons(p1) || !isSym(car(p1))) /* FIXME: Dodgy */
	    error(inv_fun,"bad function in closure");
	  p1 = p1.rg->ar; /* p1 head of body */
	  if (evtab[p1.at->spl].lispEval == expr) {
	    INCCT(evct,10);    /* fnarg expr */
	    goto ev0out;
	  } else {/* FIXME: handle closed label form! */
	    opcd = s_fnarg; 
	    if (p1.at->spl == s_fexpr) {
	      save(parm);
	      stkret(s_fxend);
	      INCCT(evct,11); /* fnarg fexpr */
	    } else if (p1.at->spl == s_macro) {
	      save(parm);
	      stkret(s_fxend);
	      stkret(s_ev_res); 
	      INCCT(evct,12); /* fnarg macro */
	    } else error(not_imp,mkEmes(2,"fnarg case",
					evtab[p1.at->spl].lispName));
	  }
	} else error(pgmr_inc,"eval0: fexpr exception");
	goto alcnt;
      case econ:  error(inv_fun,"((Constant..)) application");      break;
      default: 
	goto ev0loop;
      }
    } else { /* car iexp not a symbol */
      if (isConst(car(iexp))) error(inv_fun,"constant");
    ev0loop:
      stkret(s_eval0);  
      save(parm); 
      goreto(eval, s_rp_giexp);
      INCCT(evct,13);  /* eval recursion */
    }
    goto alcnt;
  ev0out: /* this path leads to evargs and then sys_apply */
    save(iexp);  /* for s_evargs */
    iexp = parm;
    opcd = s_evargs;  
    goto alcnt;
    /*eval0*/
    break;

  srend:  /* subr end, only if parm is still intact from s_sys_apply */
#if (DEBUG)
    ASSERT(isSym(iexp));
    ASSERT(evtab[iexp.at->spl].lispEval==subr);
    ASSERT(!isNil(parm));
    ASSERT(isEnv(parm));
#endif
    if (!isBusy(parm)) {
      INCCT(envct,5);  /* eval subr parm recovery */
      retEnv(parm);
      parm = pnil;
    }
    goto alret;

    /* auxiliary housekeeping entry points */
  case s_rs_iexp:   iexp = restore();                    goto alret;
  case s_ret_iexp:  iexp = getRes();                     goto alret;
  case s_rp_giexp:  parm = restore();  iexp = getRes();  goto alret;
  case s_rs_env:    setEnv(restore());                   goto alret;
  case s_unbind:    unbind();                            goto alret;
  case s_app_ret0:  pop1();                              goto alret;
  case s_app_ret1:  pop1(); unbind();                    goto alret;
  case s_app_ret2:  pop1(); unbind();  unbind();         goto alret;
  case s_ev_res:    iexp = getRes();   opcd = eval;      goto alcnt;
  case s_fxend:     restore(); /* parm */                goto alret;
    
  case s_bootstrap:
    p1 = getRes();
    actv.v.hdx = p1;
    setVal(a.v.hdx,p1); /* set the default expression #DX         */
    opcd = eval;        /* and evaluate whatever is in iexp (#LX) */
    break;

  case s_evaldx:
    iexp = lookup(a.v.hdx);
    opcd = eval;
    shutting_down = true;
    break;
    
  case s_sys_eval1:
    iexp = restore();
    evald--;
    goto alret;

  case s_teval: /* implicitly dgbFlag is set */
    if (isfSet(ctlFlag)) {fClr(ctlFlag); goreto(s_break, s_tevalx); goto alcnt;}
    if (isfSet(stpFlag)) {fClr(stpFlag); goreto(s_break, s_tevalx); goto alcnt;}
    /* return or fall through to tevalx */

  case s_tevalx: /* repeat of sys_eval + envptr save for break */
    if (isConst(iexp))   { sendRes(iexp);               goto alret; }
    if (isSym(iexp))     { sendRes(lookup(iexp));       goto alret; }
    evald++;
    save(envp);   /* extra stuff for */
    save(dest);   /* debug support */
    save(iexp);
    parm = cdr(iexp);
    iexp = car(iexp);
    goreto(s_eval0,s_teval1);
    break;

  case s_teval1:
    iexp = restore();
    p1   = restore(); /* dest */
    ASSERT(isEqq(dest,p1));
    p1   = restore(); /* env */
    ASSERT(isEqq(envp,p1));
    evald--;
    if (isfSet(resFlag)) {
      fClr(resFlag);
      alpsOuts(alpsout,"=>");
      dprtln(alpsout,getRes(),80);
    }
    goto alret;  
    /*s_teval1*/

  case s_done:
    disposition = done;
    goto alout;

  case s_nop: goto alret;

  default: error(sys_cor,"Operation dispatch failure in bigEval"); break;
  }/*opcd case*/

  /* default instruction exit is continue */
  goto alcnt;  /*  a while {} would be too far away from here */

 alout: /* This is the only way out except longjmp from error() */

#if ((SYSTRACE & TASKONLY) && DEBUG)
  alpsOutsn(alpserr,4,"D",itos(runtcb.tb->tid),":",
	      (char *)dispstr[disposition],"\n");
#endif

  return disposition;
}  /*bigEval*/

/* *************** SIGNAL & ERROR HANDLERS ************ */

#if (STATS)
static struct itimerspec profts;/* profiling time spec  */
static timer_t proftimer;       /* profiling timer      */

static void stats_start() {
  profts.it_interval.tv_sec  = 0;
  profts.it_interval.tv_nsec = PROF*1000;
  profts.it_value.tv_sec     = 0;
  profts.it_value.tv_nsec    = PROF*1000;
  timer_settime(proftimer,0,&profts,NULL);
}

static void stats_stop() {
  profts.it_interval.tv_sec  = 0;
  profts.it_interval.tv_nsec = 0;
  profts.it_value.tv_sec     = 0;
  profts.it_value.tv_nsec    = 0;
  timer_settime(proftimer,0,&profts,NULL);
}

void stats_handler(int sig) { 
  if (in_boogey) sttms[s_gc]++;
  else  if (!isNil(runtcb)) {
    ASSERT((opcd >= s_nop) && (opcd < s_trailer));
    sttms[opcd]++;
  }
}
#endif

static whole busyTickCount = 0; // moved to int due to Arm linux FPU bug
static whole idleTickCount = 0;
static whole watchDogCount = 0; // Number of ms without progress

void timer_handler(int sig) {
  number barkInterval;
  if (isNil(runtcb)) idleTickCount++;
  else {
    fSet(pmtFlag);         /* request pre-emption */
    busyTickCount++;
    if (isfClr(wdgFlag)) { // Big eval had progress
      watchDogCount = 0;
      fSet(wdgFlag);
    } else { // wdgFlag still set => no progress so far
      barkInterval = getNumVal(a.v.hwd);
      if (barkInterval <=0) return; /* watchdog has been muzzled */
      if (((number)watchDogCount*TICKS)/1000.0 > barkInterval) {
	watchDogCount = 0;
	error(sys_int, "watchdog timer expired");
      } else watchDogCount++;
    }
  }
}

void int_handler(int sig) {
  //  int wlen;
  //  wlen = write(1,"\nSIGINT\n",8);
  if (isfSet(ctlFlag)) error(sys_int, errTab[sys_int].errmes);
  else {
    fSet(ctlFlag);
    if (isNil(runtcb)) mreptcb.tb->tflags |= ctlFlag;
    else               runtcb.tb->tflags  |= ctlFlag;
  }
}

void term_handler(int sig) {
  int wlen;
  if (end_of_program) alpsExit(1);
  end_of_program = true;
  savewspc       = (isNil(getVal(a.v.hsw))) ? false : true;
  wlen = write(1,"SIGTERM\n",8);
}

static void segv_handler(int sig, siginfo_t *si, void *unused) {
  int range;
  whole lim;
#if (DEBUG)  
  alpsMsg(5,"Fault address: ",itox((whole)si->si_addr),
	  "\nStack address: ",itox(stkp.wh),"\n");
#endif
  if (!isNil(runtcb)) {
    lim = runtcb.tb->cntptr.ct->stklim.wh;
    range = (whole)si->si_addr - lim;
    if (range < 256) { /* stack overflow */
#if (DEBUG)
       alpsMsg(7,"Stack limit:   ",itox(lim),"\nStack overflow at depth ",
	       itos((stkp.wh-runtcb.tb->base.wh)/indlen),
	       " in ",evtab[opcd].lispName,"\n");
#endif
      stkp.wp--;
      error(stk_ovfl," recovering...");
    }
    tcbFlush(runtcb.tb);
    printTcb(alpserr,runtcb);
    showCtl(alpserr,runtcb.tb->cntptr);
  }
  alpsMsg(1,"\nSIGSEGV...\n");
  alpsExit(1);
}

#if (DEBUG)
static sigset_t pause_ss;
void alpsPause() {
  int sig;
  alpsMsg(5,"alpsPause: pause sig ",itos(SIGCONT)," pid ",itos(getpid()),"\n");
  sigwait(&pause_ss, &sig);
  alpsMsg(1,"alpsPause: resume\n");
}
#endif

static sigset_t alpsSigMask;
static int alpsSigBlock = 0;
void alpsBlockSignals() {
  sigset_t blockMe;
  int retval;
  sigemptyset(&blockMe);
  sigaddset(&blockMe,SIGALRM);
  sigaddset(&blockMe,SIGINT);
#if (STATS)
  sigaddset(&blockMe,SIGRTMIN);
#endif
  if (alpsSigBlock) error(pgmr_inc,"sig block");
  retval = sigprocmask(SIG_BLOCK,&blockMe,&alpsSigMask);
  if (retval)
    error(ios_err,mkEmes(3,"Can't block signals: ",strerror(errno),"\n"));
  alpsSigBlock++;
}

void alpsUnblockSignals() {
  int retval;
  if (!alpsSigBlock) error(pgmr_inc,"sig unblock");
  retval = sigprocmask(SIG_SETMASK,&alpsSigMask,NULL);
  if (retval)
    error(ios_err,mkEmes(3,"Can't unblock signals: ",strerror(errno),"\n"));
  alpsSigBlock--;
}

void alpsAbend(const char *mess) {
  alpsMsg(2,mess,"\nalps: Abend\n");
  alpsExit(1);
}

/* ***************** TOP LEVEL ROUTINES ********** */

static const char *USAGE =
  "Usage:\n alps [-d] [-n] [-D[T]] [-f <init-file>] [-s] [-t <tty>] [-w <dir>] " 
  "[-S <scale>] [-h <hist-file>] [-e <s-expression>]\n";

static void alpsPrintBanner() {
  alpsOutsn(cout(),9,myname," ",hosts[HOST],itosn(PROC,3),
	    "bit Interpreter V",rtos(ALPS_VERSION,4,2),
	    //" cpt ",itos(alpsCPT),
	    " ", itos((512 + wspBsiz)/1048576),"MB\n");
}
#define xstr(s) str(s)
#define str(s) #s
static void alpsPrintVersion() {
  alpsOutsn(cout(),13,
	      "Sourcefile ",__FILE__," ",REV,
	    "\ngccVersion ", __VERSION__,
	    "\ngccFlags   ",xstr(GCCFLAGS),
	    "\ngccHost    ",xstr(GCCHOST),
	    "\ngccProc    ",xstr(GCCPROC),
	    "\nExecutable ");
  alpsPrintBanner();
}

static void *alpsMapMem(int scale) {
  void *maddr;
  ASSERT((wsp_size & (wsp_block - 1)) == 0);
  /* Calculate sizes for the various regions and the corresponding total */
  fs_size     = scale*wsp_block*2048;    /* no. of list nodes (rg_blks)  */
  ctl_depth   = scale*wsp_block*32;      /* control stack depth (ints)   */
  envsize     = ctl_depth*maxstacks;     /* same as tot stacks (ints)    */
/* total(ints = (ATM,Cnt,Tcb,Fob,anon,overhead) + FS       +  Env+Stk    */
  wsp_size    = scale*wsp_block*wsp_bsize   + fs_size*2 + envsize*2;
  wspBsiz     = wsp_size*indlen;         /* total size in bytes          */
  
  //if (!(r.s.wspc.ip = malloc(wspBsiz))) {
#if (PROC==32)
  maddr = mmap((void *)0xf3500000,wspBsiz,
#elif (PROC==64)
  maddr = mmap((void *)0x00007f0000000000L,wspBsiz,
#else
		     Invalid PROC )		     
#endif
		     PROT_READ|PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS,-1,0);
  if (maddr == (void *)-1) {
    alpsAbend(mkEmes(6,myname," unable to mmap Workspace size ",
		     itox(wspBsiz)," ",strerror(errno),"\n"));
  }
  mapaddr.vd = maddr;
  return maddr;
}

static void alpsUnmapMem() {
  int res;
#if (PROC==32)
  res = munmap((void *)0xf3500000,wspBsiz);
#elif (PROC==64)
  res = munmap((void *)0x00007f0000000000L,wspBsiz);
#else
  Invalid PROC;
#endif
  if (res) {
    alpsAbend(mkEmes(6,myname," unable to munmap Workspace size ",
		     itox(wspBsiz)," ",strerror(errno),"\n"));
  }
  mapaddr.vd = NULL;
}

static void alpsInit(int argc, char *argv[]) {
  /* our friends the law and the king (long ago: left & right expression) */
  pointer lex,rex;
  bool    ws_load_ok,showVers = false;
  char   *errs,buf[256],*version;
  int     c,i,len,sign,bpnt,wlen;
  whole   scale;

  initCharSets();  /* isDigit needed for stoi, isSpec  for initWorkspace */

  while ((c = getopt (argc, argv, "DS:TVde:f:h:ndirst:vw:")) != -1)
    switch (c)  {
    case 'D': initFlags |= dbgFlag;                break;
    case 'T': initFlags |= trcFlag;                break;
    case 'V': showVers   = true;                   break;
    case 'd': daemonic   = true;                   break;
    case 'e': def_expr   = optarg;
      one_shot = true; /* one shot mreptcb */
      break;
    case 'f': mLoadF     = optarg;                 break;
    case 'h': mHist      = optarg;                 break;
    case 'n': no_notice  = true;                   break;
    case 'r': rawTerm    = true;                   break; /* no alpsTermReader*/
    case 's': ssid       = true;                   break; /* session leader */
    case 't': mTty       = optarg;                 break;
    case 'v': /* got to do it manually since APL variables are not initted */
      len = rascal(ALPS_VERSION,10,4,2,&sign,(uchar *)buf,&bpnt,0);
      buf[len++] = '0'; buf[len] = '0';
      version = mkEmes(4,subs(buf,bpnt),"-",subs(&buf[bpnt],2),"\n");
      wlen = write(1,version,strlen(version));
      exit(0);
      break; /* NOT REACHED */
    case 'w':
      if (chdir(optarg)) 
	alpsAbend(mkEmes(3,optarg,": Bad dir\n",strerror(errno)));
      break;
    case 'S':
      if (!stoi(optarg, &scale) || scale < 1)
	alpsAbend(mkEmes(3,optarg,": Invalid scale factor\n"));
      wsp_scale = scale;
      break;
    default:
      alpsAbend(USAGE);
    }

  myname = argv[0];

  if (!mTty)   mTty   = in_file;
  if (!mHist)  mHist  = hist_file;
  if (!mLoadF) mLoadF = load_file;
  if (ssid)    setsid();

  alpsCPT  = sysconf(_SC_CLK_TCK);
  pagesize = getpagesize();

  if ((ws_load_ok = load_wspc((uchar *)save_file)) != true) {
    r.s.wspc.vd = alpsMapMem(wsp_scale);
#if (DEBUG)
  memset(r.s.wspc.ip,0,wspBsiz);
#endif
    initWorkspace();
    initEnv();
    makeCounters();  /* only when STATS==true */
    initCnt();	     /* part of root vars */ 
    initTcbTab();
  }
  initFiles(ws_load_ok);
  ASSERT(isSym(a.v.hcp));
  ASSERT(no_root_vars==NUM_ROOT_VARS);
  alpsFlush(alpserr);
  initVars(argc, argv);
  /* propagate initial runtime flags to mreptcb   */
  mreptcb.tb->tflags = initFlags; /* only trcFlag */
  if (showVers) {
    alpsPrintVersion();
    alpsExit(0);
  }
 

  alpsReadHistory();
  alpsin->hp = lookup(a.v.hhs); /* history pointer   */
  /* initialise tasks */
  if (!ws_load_ok) { /* built default #LX and read history */
    if (!access(mLoadF,R_OK)) {
      lex  = buildChr(mLoadF,strlen(mLoadF));
      protect(lex);
      rex = list(s[s_load],lex); /* #LX <= (load mLoadF) */
      unprotect();
    } else rex = pnil; 
    setVal(a.v.hlx,rex); /* set the latent expression #LX   */
  } 
#if (GRAF)
  alpsInitGraphics(argc,argv);
#endif
#if (INSTCON)
  alpsInitInst();
#endif
#if  (DEBUG)
  /* Sigset for alpsPause */
  sigemptyset(&pause_ss);
  sigaddset(&pause_ss, SIGCONT);
  sigprocmask(SIG_BLOCK, &pause_ss, NULL);
#endif
#if (STATS)
  /* Stats timer */
  stats_sigev.sigev_notify    = SIGEV_SIGNAL;
  stats_sigev.sigev_signo     = SIGRTMIN;
  stats_sigev.sigev_value.sival_ptr = &proftimer;
  sigemptyset(&stats_sa.sa_mask);
  stats_sa.sa_handler  = stats_handler;
  stats_sa.sa_flags    = SA_RESTART;
  sigaction(SIGRTMIN, &stats_sa, NULL); 
  timer_create(CLOCK_REALTIME,&stats_sigev,&proftimer);
#endif
#if (TIMER)
  /* System timer for preempt, watchdog and task time accounting */
  timer_action.sa_handler   = timer_handler;
  sigemptyset(&timer_action.sa_mask);
  timer_action.sa_flags     = SA_RESTART;
  sigaction(SIGALRM, &timer_action, NULL);
  alpsTick.it_interval.tv_sec  = 0;
  alpsTick.it_interval.tv_usec = TICKS * 1000;
  alpsTick.it_value.tv_sec     = 0;
  alpsTick.it_value.tv_usec    = TICKS * 1000;
  setitimer(ITIMER_REAL,&alpsTick,0);
#endif
  /* terminate nicely if possible */
  term_action.sa_handler    = term_handler;
  sigemptyset(&term_action.sa_mask);
  term_action.sa_flags      = SA_RESTART;
  sigaction(SIGTERM, &term_action, NULL);  
  /* handle terminal resizing */
  winch_action.sa_handler   = alpsWinchHandler;
  sigemptyset(&winch_action.sa_mask);
  winch_action.sa_flags     = SA_RESTART; 
  sigaction(SIGWINCH, &winch_action,NULL); 
  /* user interrupt handler  */
  int_action.sa_handler      = int_handler;      
  sigemptyset(&int_action.sa_mask);
  int_action.sa_flags        = SA_RESTART;
  if (sigaction(SIGINT, &int_action, NULL))        /* user interrupt */
    alpsError("sigaction sigint");
  segv_action.sa_flags = SA_SIGINFO;
  sigemptyset(&segv_action.sa_mask);
#if (FAST)
  /* Hardware stack overflow detection */
  segv_action.sa_sigaction = segv_handler;
  if (sigaction(SIGSEGV, &segv_action, NULL) == -1)
    alpsError("sigaction sigsegv");
#endif
  /*  signal(SIGFPE,SIG_IGN); TODO: error handling for FPE */
  if (!no_notice) alpsPrintBanner();
  alpsOutln(cout());
  if (ws_load_ok) {
    alpsOuts(cout(),helloMess);
    alpsFlush(cout());
    if (!isNil(lookup(a.v.hlx))) { /* Run any latent expression in #LX  */
      mreptcb.tb->iexp = lookup(a.v.hlx);
      // ASSERT(mreptcb.tb->opcd==s_nop); /* set to nop in s_exit and s_bye */
      mreptcb.tb->opcd = eval;
    }
    return;
  } 
  r.s.sysprmpt = alps_make_prompt("alps: ");
  r.s.brkprmpt = alps_make_prompt("break: ");
  r.s.bprompt  = r.s.brkprmpt;
  alpsSetPrompt(r.s.sysprmpt);
  actv.v.hpr   = getVal(a.v.hpr);
  /*Bootstrap:  build and schedule read eval print loop default expression   */
  lex = buildChr(def_expr,strlen(def_expr));
  rex = allocBuf(lex); /*build file buffer with our repl string              */
  rex.fb->fobpvt.fi->bufstat = ldvs_ok;      /* got data                     */ 
  rex.fb->fobpvt.fi->ecci    = nels(lex);    /* number of chars in def_expr  */
  stkPush(mreptcb.tb->cntptr,s_done);        /* bottom of stack              */
  if (one_shot) {
    stkPush(mreptcb.tb->cntptr,s_evaldx);    /* evaluate #DX once and die    */
  }
  stkPush(mreptcb.tb->cntptr,s_bootstrap);   /* da magic switch   #DX        */
  stkPushp(mreptcb.tb->cntptr,rex);   /* sys_read will find buf for #DX here */
  mreptcb.tb->opcd = s_sys_read;             /* just go and read #DX buffer  */
  mreptcb.tb->iexp = lookup(a.v.hlx);        /* initial (latent) expression  */
  enqTcb(runbl,mreptcb);                     /* read-eval-print task run q   */
  setVal(a.v.hlx,pnil);                      /* no default #LX is saved wspc */
  enqTcb(done,iotcb);                        /* put io task on the done q    */
}

static char **dummy_arg;
static bool alpsRestore(const uchar *name) {
  bool ws_load_ok;
  ws_load_ok = load_wspc(name);
  if (ws_load_ok) {
    initFiles(ws_load_ok);
    initVars(0, dummy_arg);
    alpsReadHistory();
    alpsin->hp = lookup(a.v.hhs); /* history pointer   */
    alpsOuts(cout(),helloMess);
    alpsFlush(cout());
    if (!isNil(lookup(a.v.hlx))) { /* Run any latent expression in #LX  */
      mreptcb.tb->iexp = lookup(a.v.hlx);
      mreptcb.tb->opcd = eval;
    }
  }
  return ws_load_ok;
}

static void alpsShutDown(void) {
  char *epitaph;
  int  wlen;

  swap_down(envp,pnil);
  if (!no_notice) {
    alpsOutsn(alpsout,3,"\n/alps: Session ",savewspc?"suspending":"ended",
	      " per user request.\n");
    alpsFlush(alpsout);
  }

#if (WSPSAV)
  if (savewspc) {
    closeFiles();
    if (save_wspc((uchar *)save_file)) {
      epitaph = mkEmes(7,"Workspace saved in ", save_file," from ",
		       itox(r.a[0].wh)," size = ",
		       itos(wspBsiz/0x100000),"MB\n");
      wlen = write(1,epitaph,strlen(epitaph));
    } else {
      alpsAbend(mkEmes(5,"Unable to save workspace in", save_file,
			" : ",strerror(errno),"\n"));
    }
  }
#endif
  alpsExit(alpsExitCode);
}

static void scheduleTcb(pointer lex, lispTypes opcode) {
  *++(lex.tb->cntptr.ct->ctlsp).wp = s_done;
  lex.tb->opcd = opcode;
  enqTcb(runbl,lex);
}

static void alpsScheduler() { 
  /* schedule tcbs on completed tcb done and io/timer events */
  fd_set fdsel[3];
  int i,j,lfd,nfds,nfound;
  struct timeval timeout;
  whole leastimer;
  pointer lex,task,ot,tt;
  number  now;
  
  /* check for those who need to be selected */
  nfds = 0;
  if (!isNil(disptab[winp]) || !isNil(disptab[swait])) { // cond for select
    leastimer = TICKS*1000; /* milliseconds to microseconds */
    bzero(fdsel,sizeof(fdsel));
    for (task=disptab[winp];!isNil(task);task=task.tb->next) {// scan wimpers
      lfd = task.tb->inpfd;
      if (readcb[lfd]) FD_SET(lfd,&fdsel[0]);
      if (exceptcb[lfd]) FD_SET(lfd,&fdsel[2]);
      if ((readcb[lfd] || exceptcb[lfd]) && (lfd > nfds)) nfds = lfd;
    }
    for (task=disptab[swait];!isNil(task);task=task.tb->next) {// scan waiters
      for (lex=task.tb->filwq;!isNil(lex);lex=cdr(lex)) { // file wait queue
	lfd = car(lex).fb->fobpvt.fi->fd;
	FD_SET(lfd,&fdsel[0]);
	if (exceptcb[lfd]) FD_SET(lfd,&fdsel[2]);
	if (lfd > nfds) nfds = lfd;
      }
    }
#if (GRAF)
    if (ez_fd >= 0) { FD_SET(ez_fd,&fdsel[0]); if (ez_fd > nfds) nfds = ez_fd; }
#endif
    nfds = nfds + 1;
    /* gflag is set in plot when there is something to draw */
    if (!isNil(disptab[runbl]) || !isNil(disptab[tyld]) || gflag) {
      /* have work to do so poll */
      timeout.tv_usec = 0;
      timeout.tv_sec  = 0;
    } else { /* nobody is running so we can wait a bit */
      timeout.tv_usec = leastimer % 1000000;
      timeout.tv_sec  = leastimer / 1000000;
    }
    nfound = select(nfds,
		    (fd_set *)(&fdsel[0]),
		    (fd_set *)(&fdsel[1]),
		    (fd_set *)(&fdsel[2]),
		    &timeout);
    if (nfound < 0) {
      if (errno != EINTR) alpsMsg(3,"Select err ",strerror(errno),"\n");
      goto  sched_timer_waiters;
    }
#if (GRAF)
    if (gflag) { alpsServiceWidgets(); gflag = 0; } /* plot needs servicing */
#endif
    if (nfound == 0) goto sched_timer_waiters;
    /* nfound > 0 */
    j=0;
#if (GRAF)
    if (ez_fd >=0 && FD_ISSET(ez_fd,&fdsel[0])) {
      alpsServiceWidgets();
      if (nfound == ++j)  goto sched_timer_waiters;
    }
#endif
      /* scan wimpers again for ready files */
    for (ot=task=disptab[winp];!isNil(task);) {
      lfd = task.tb->inpfd;
      if (FD_ISSET(lfd,&fdsel[0]) || FD_ISSET(lfd,&fdsel[2])) {
	/* 4now invoke callback  (no lisp code) only at a time would occur
	   since wimp waits on one fd only                                */
	if (FD_ISSET(lfd,&fdsel[0])) { j++; (*readcb[lfd])(lfd); }
	if (FD_ISSET(lfd,&fdsel[2])) { j++; (*exceptcb[lfd])(lfd); }
	/*  dequeue from wimpers */
	if (isEqq(ot,task)) ot=disptab[winp]=task.tb->next;
	else ot.tb->next = task.tb->next;
	tt   = task; 
	task = task.tb->next;
	enqTcb(runbl,tt); /* wake up the sleeper */
	if (nfound == j) goto sched_timer_waiters;
      } else {ot = task; task = task.tb->next; }
    }
    /* scan waiters again for ready files */
    for (ot=task=disptab[swait];!isNil(task);) {
      for (lex=task.tb->filwq;!isNil(lex);lex=cdr(lex)) { /* scan fils */
	lfd = car(lex).fb->fobpvt.fi->fd;
	if (FD_ISSET(lfd,&fdsel[0]) || FD_ISSET(lfd,&fdsel[2])) {
	  if (FD_ISSET(lfd,&fdsel[0])) j++;
	  if (FD_ISSET(lfd,&fdsel[2])) j++; 
	  /* dequeue task */
	  if (isEqq(ot,task)) ot=disptab[swait]=task.tb->next;
	  else ot.tb->next = task.tb->next;
	  tt   = task; 
	  task = task.tb->next;
	  setTcbRes(tt,cons(car(lex),pnil));
	  enqTcb(runbl,tt); /* wake up the sleeper */	
	  if (nfound == j) goto sched_timer_waiters;
	  break; /* done with this task */
	}
      }
      if (isNil(lex)) { // nobody dequeued - so move on
	ot = task;
	task = task.tb->next;
      }
    }
  }
  
 sched_timer_waiters:  /* shedule timer waiters */
  now = mkrtime();
  for (ot=task=disptab[swait];!isNil(task);) {
    if (((task.tb->stime > 0.0) && (task.tb->stime <= now)) || isfSet(ctlFlag)) {
      if (isEqq(ot,task)) ot=disptab[swait]=task.tb->next;
      else ot.tb->next = task.tb->next;
      tt=task; task=task.tb->next;
      setTcbRes(tt,mkNum(now-tt.tb->stime)); /* delta time to wait */
      tt.tb->stime = 0.0;  /* reset time just for the form */
      tt.tb->tcbwq = pnil; /* drop tcb wait list */
      tt.tb->filwq = pnil; /* drop fil wait list */
      enqTcb(runbl,tt);
    } else {ot = task; task = task.tb->next; }
  }
  if (isfSet(ctlFlag)) { // wake up all the wimpers
    for (task=disptab[winp];!isNil(task);) {
      tt            = task; 
      task          = task.tb->next;
      disptab[winp] = tt.tb->next;
      enqTcb(runbl,tt); 
    }
  }
  /* Lastly move the guys on the yielded queue to back onto runbl so that */
  /* they end up behind the newly scheduled tasks of the same priority    */
  while (!isNil(task = deqTcb(tyld))) enqTcb(runbl,task);
}    /* End scheduler */

static dispstate runTcb(pointer task) {
  pointer eframe,p1,p2,lex;
  dispstate dispCode;
  
  if (sigsetjmp(env_main,true)) { /* save signal mask */
    ASSERT(isEqq(runtcb,task));
    lex    = runtcb.tb->cntptr;
    lastcb = runtcb;  
    runtcb = pnil;
    if (alpsSigBlock) alpsUnblockSignals();
    alpsFlush(alpsout); 
    if (task.tb->errflg==stk_ovfl) {
      if (lex.ct->extflg) { // Already running on stack extension
	alpsOuts(alpserr,"Overflow in stack extension \n");
	task.tb->errinf = " recovery not possible";
	showError(task.tb);
	crank_down(lex.ct->ctlsp,lex.ct->stack);
	swap_down(envp,task.tb->envbase);
	task.tb->dest = dest;
	task.tb->envp = task.tb->envbase;
	return done;
      } else { /* recovery - use stack extension */
	protStack(lex.ct,true); /* Enable extension */
	setTcbStkLim(task.tb);
	/* reset again in resetTcb() at the end of recovery */
      }
    }
    if (isfSet(dbgFlag)) {
      setEnv(task.tb->envp);
      p1 = findCtl(task.tb,0,s_brkc); /* are we in a break ? */
      if (!isNil(p1)) {  /* yes we are */
	showError(task.tb);
	alpsOuts(alpserr,"Error in break\n");
	p2 = p1.pt[-3]; /* current env in break frame */
	crank_down(lex.ct->ctlsp,p1);
	swap_down(envp,p2);
	//	task.tb->dest = dest;
	task.tb->envp = p2;
	lex.ct->ctlsp.pt = --p1.pt;
	task.tb->opcd = s_brkc;
	return runbl;
      }
      eframe = findCtl(task.tb,0,s_teval1);
      if (!isNil(eframe)) {
	showError(task.tb);
	crank_down(lex.ct->ctlsp,eframe);
	swap_down(envp,eframe.pt[-3]);
	restore_last_eval_frame(task.tb,eframe); // envp restored here
	stkPush(task.tb->cntptr,s_teval);
	task.tb->opcd = s_break;
	freeze(task.tb);  /* freeze everybody else */
	return tyld;
      }
    } 
    task.tb->opcd = s_bail;      /* bailing out */
    return tyld;
  }
  if (!isEqq(lastcb,task)) { /* Context switch: restore new guy's env */
    restoreEnv(isNil(lastcb) ? pnil : lastcb.tb->envp,task.tb->envp);
  }
  ASSERT(isNil(runtcb));
  runtcb   = task;            /* pmtFlag only settable while runtcb != pnil */
  tcbCache(task.tb);          /* set global runtime from tcb                */
  /* Set main eval entry and exit points depending on dbgFlag for this tcb  */
  if (isfSet(dbgFlag)) setDbg();
  else                 clrDbg();
#if ((SYSTRACE & TASKONLY) && DEBUG) 
  alpsOutsn(alpserr,5,"R",itos(runtcb.tb->tid),":",
	    evtab[runtcb.tb->opcd].lispName,"\n");
#endif
  dispCode = bigEval(runtcb.tb);   // this is the only call to bigEval
  tcbFlush(task.tb);               // save global runtime in tcb
  runtcb   = pnil;                 // close the dpre_empt window
  lastcb   = task;                 // remember the one that went before
#if (DEBUG)
  tcbring[tcbrind++] = task.tb->tid; // remember who has gone before
  if (tcbrind==BB_TCBDEPTH) tcbrind = 0;
  task.tb->iexctr  = iexctr;
#endif
  return dispCode;  // Tell the dispatcher what to do  with him
} /* runTcb */

static void alpsDispatcher() {
  pointer   current,task,ot,tt,wkr,wsName;
  dispstate dstate;
  fibrec   *f;
  bool      res;

  uidle        += idleTickCount;
  idleTickCount = 0; // TODO block signal
    
  if (!isNil(disptab[runbl])) {  /* we have a candidate */
    current = deqTcb(runbl);
#if 0
    alpsOutsn(alpserr,9,"Dispatching ",itox(current.wh),
	      " nenv "  , itox(current.tb->envp.wh),
	      " cenv "  , itox(envp.wh),
	      " rtflgs ", itox(current.tb->tflags),"\n");
    alpsFlush(alpserr);
#endif
    /* If dispatcher is locked only dispatch the holder of the lock */
    if (!isNil(displock) && (!isEqq(current,displock))) dstate = bench;
    else { 
      dstate = runTcb(current); /* The only place where we run the bastard */
      /* Perform time accounting. FIXME: should defer signal here */
      current.tb->utime += busyTickCount;
      ubusy             += busyTickCount;
      busyTickCount      = 0;
      if (current.tb->tflags & ctlFlag) {
	current.tb->tflags &= ~ctlFlag;
	fClr(ctlFlag);
	/* Handle condition-case for user_int when TDEBUG is false */
	current.tb->errinf = "";
	current.tb->errexp = pnil;
	current.tb->eopcd  = s_nop;
	current.tb->opcd   = s_bail;
	current.tb->errflg = user_int;
	dstate = tyld; 
      }
    }
    switch (dstate) { /* Now how did he finish ? */
    case done:
      /* Ensure that the binding environment is clean */
      ASSERT(current.tb->envbase.pt == current.tb->envp.pt);
      // in case he is at death's door remove his legacy
      if (!isNil(current.tb->envp)) {
	restoreEnv(current.tb->envp,pnil);
      }
      if (current.tb->errflg) {
	if (isEqq(current,mreptcb)) {
	  alpsOutsln(alpserr,"Trapped out back to main");
	} else {
	  alpsOutsn(alpserr,3,"Task ",itos(current.tb->tid)," aborted.\n");
	}
	alpsFlush(alpserr);
      }
      if (isEqq(displock, current)) thaw(); /* release dispatcher lock if held*/
      lastcb = pnil;                        /* no optnty 4 context switch opt */
      if (current.tb == mreptcb.tb) {       /* this chap's work is never done */
	resetTcb(current.tb,true,pnil);     /* no res for mreptcb either      */
	if (isNil(getVal(a.v.hdb))) current.tb->tflags &= ~dbgFlag; /*clear or*/
	else  current.tb->tflags |= dbgFlag;/* set debug flag in tcb          */
	clrRawMode(fparr[mreptcb.tb->inpfd]); /* make sure line reader is actv*/
	mreptcb.tb->iexp = lookup(a.v.hdx); /* default exp is now your mission*/
	scheduleTcb(mreptcb,eval);          /* back to work you lazy bugger!  */
	end_of_program = shutting_down;     /* maybe after a sleep            */
      } else {
	resetTcb(current.tb,true,getTcbRes(current));/*set res for  waiters */
	/* Scan waiter queue for folks waiting for current to be done */
	/* Could be more efficient... */
	for (ot=task=disptab[swait];!isNil(task);) {
	  /* scan waiter's wait list to see if current task is among them */
	  for (wkr = task.tb->tcbwq; !isNil(wkr); wkr = cdr(wkr)) { 
	    if (isEqq(car(wkr),current)) break;
	  }
	  if (!isNil(wkr)) { /* Remove the chap from swait q */
	    if (isEqq(ot,task)) ot=disptab[swait]=task.tb->next;
	    else ot.tb->next = task.tb->next;
	    tt            = task;
	    task          = task.tb->next;
	    tt.tb->tcbwq  = pnil;  /* drop waiter's wait list            */
	    tt.tb->stime  = 0.0;   /* no timer wait any more in any case */
	    /* tell tt that current is done giving its final result      */
	    setTcbRes(tt,cons(current,current.tb->fres));
	    enqTcb(runbl,tt);      /* now tt shall not wait any longer   */ 
	  } else {ot = task; task = task.tb->next; }
	}
	if (isNil(current.tb->iexpq)) {
	  enqTcb(done, current);    /* you now are done */
	} else { /* but you still have work to do */
	  current.tb->iexp    =  caar(current.tb->iexpq);
	  current.tb->envp    = cadar(current.tb->iexpq);
	  current.tb->envbase = cadar(current.tb->iexpq);
	  //  alpsOuts(alpsout,"Scheduling with ");
	  //  dprtln(alpsout, current.tb->iexp, 30);
	  ASSERT(isEnv(current.tb->envp) || isNil(current.tb->envp));
	  current.tb->iexpq    =   cdr(current.tb->iexpq);
	  scheduleTcb(current,eval);
	}
      }
      break;

    case runbl:  enqTcb(runbl, current); break;

    case swait:
      enqTcb(swait, current); /* put on timer / tcb wait q */
      break;
      
    case winp:
      /* save fdset so we can schedule him when select mask occurs */
      /* By convention we pop the fob that was put on tos for us   */
      f = getFil(*current.tb->cntptr.ct->ctlsp.pt--); /* !! inline restore */
      current.tb->inpfd = f->fd;
      readcb[f->fd]     = f->readcb;    /* read callback for fibrec        */
      exceptcb[f->fd]   = f->exceptcb;  /* except (oob) cb                 */
      enqTcb(winp, current);
      break;
      
    case tyld:    
      /* put him on the yield queue */
      enqTcb(tyld, current);
#if (WSPSAV)
      if (current.tb->opcd == s_saveaux) { 
      /* must save workspace outside bigEval for saver to appear in disptab */
	wsName = lookup(a.v.hws); /* before we drop down env */
	swap_down(envp,current.tb->envbase);
	lastcb = pnil; /* need context switch going back */
	current.tb->opcd = s_nop; /* nothing was going on, really! */
	res = save_wspc(mkString(wsName,tline,"file name"));
	/* place result of save on stack for saveaux */
	stkPushp(current.tb->cntptr,(res) ? ptru : pnil);
	current.tb->opcd = s_saveaux; 
        break;
      }
      if (current.tb->opcd == s_rstraux) {
	wsName = lookup(a.v.hws); /* before we drop down env */
	swap_down(envp,current.tb->envbase);
	closeFiles();
	res = alpsRestore(mkString(wsName,tline,"file name"));
	if (!res) {
	  alpsAbend("Failed to restore workspace - unable to recover\n");
	}
	break;
      }
#endif
      break;

    case bench:   enqTcb(bench, current);      break;

    default: error(pgmr_inc,"main dispatcher");
    }
  } 
}

/* ************** MAIN PROGRAMME ****************** */

int main(int argc, char *argv[]) {

  alpsInit(argc,argv);

  while (!end_of_program) {
    alpsScheduler();  /* schedule work */
    alpsDispatcher(); /* do some work  */
  }

  alpsShutDown();
}  /* main */
