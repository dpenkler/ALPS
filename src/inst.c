/************** Instrument IO ******************/
#include "alps.h"

#if (INSTCON==true)
#include <gpib/ib.h>

#define INST int

void alpsInitInst() {
}

inline static INST getInst(pointer lex) {return(lex.fb->data1.it);}
inline static void setInst(pointer lex, INST id) {lex.fb->data1.it = id;}
static void ckInstErr(int err) {
  if (err & 0x8000) {
    alpsOutsn(alpserr,5,"iberr:",itox(iberr)," ibsta:",itox(err),"\n");
    error(ios_err,NULL);
  }
}

void instClean(pointer lex) {
  INST id;
  char iname[max_line_len];
  ASSERT(isInst(lex));
  mkString(lex.fb->name, (uchar *)iname,"Instrument name");
  alpsOuts(alpserr,mkEmes(3,"instClean for ",iname, "\n"));
  id = lex.fb->data1.it;
  if (id >= 0) ibonl(id,0);
}

pointer alpsOpenInst(pointer parm) {
  INST id;
  int len;
  pointer rex,tex,dev,name;
  int addr,minor, pad;
  unsigned char iname[max_line_len];
  unsigned char *devname;
  if ((nargs(parm) != 1) ||( (!isaChr(arg1(parm))) && !isNum(arg1(parm))))
    error(inv_funarg,"(oio <chr> | num)");
  dev = arg1(parm);
  if (isaChr(dev)) {
    mkString(dev, (uchar *)iname,"Instrument name");
    id = ibfind((char *)iname);
    name = dev;
    devname = iname;
    alpsOutsn(alpserr,3,"oio opening ",devname,"\n");
  } else if (isNum(dev)) {
    addr  = getWhole(dev);
    minor = (addr / 100) - 7;
    pad   =  addr % 100;
    if ((pad < 0) || (pad > 30))
      error(inv_funarg,"invalid pad (0 <= pad <= 30)");
    if (minor < 0)
      error(inv_funarg,"invalid select code (7 <= sc");
    id = ibdev(minor,pad,0,14,1,0);
    devname = itos(addr);
    name = mkChr(devname,strlen((char *)devname));
    alpsOutsn(alpserr,5,"oio opening minor ",itos(minor),"  Pad ",itos(pad),"\n");
  }

  if (id >= 0) { /* ibfind/ibdev will shout but we check anyway */
    ibtmo(id,14);
    tex.it = id; /* if id < 0 => inst fob is closed */
    rex = allocFob(inst,name,tex,pnil,0);
    ibconfig(id, IbcUnAddr, 1); // set unaddressing mode
    return(rex);
  }
  return(pnil);
}

static const integer max_rio_len=4096;

pointer alpsReadInst(pointer parm) {
  pointer inst,rex;
  integer len;
  int err;
  char ibuf[max_rio_len];
  INST id;
  if ((nargs(parm)!=1) || (!isInst(arg1(parm)))) 
    error(inv_funarg,"(rio <inst>)");
  inst = arg1(parm);
  id   = getInst(inst);
  if (id < 0) error(ios_err,"Instrument offline");
  alpsBlockSignals();
  err  = ibrd(id,ibuf,max_rio_len);
  alpsUnblockSignals();
  ckInstErr(err);
  len  = ibcntl;
  return((len) ? mkChr((uchar *)ibuf,len) : pnil);
}

pointer alpsWriteInst(pointer parm) {
  pointer inst,rex;
  integer len;
  int err;
  INST id;
  if ((nargs(parm) != 2) || (!isInst(arg1(parm))) || (!isaChr(arg2(parm))))
    error(inv_funarg,"(wio <inst> <str>)");
  inst = arg1(parm);
  id   = getInst(inst);  
  if (id < 0) error(ios_err,"Instrument offline");
  rex  = arg2(parm); 
  alpsBlockSignals();
  err  = ibwrt(id,vadd(rex).sp,nels(rex));
  alpsUnblockSignals();
  ckInstErr(err);
  len  = ibcntl;
  if (len != nels(rex)) alpsOuts(alpserr,"wio short write\n");
  return(rex);
}

pointer alpsCloseInst(pointer parm) {
  INST id;
  pointer inst;
  if ((nargs(parm) != 1) || (!isInst(arg1(parm))))
    error(inv_funarg,"(cio <inst>)");
  inst = arg1(parm);
  id   = getInst(inst);
  if (id >= 0) {
    ibonl(id,0);
    setInst(inst,-1);
    return(ptru);
  }
  return(pnil);
}
#endif /* INSTCON */
