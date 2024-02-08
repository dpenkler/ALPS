/*  ************************* SOUND SECTION *********************  */
#include "alps.h"
#if (SOUND==true)

#include <sys/soundcard.h>
#define MAX_SND_BUF_LEN  48000*10
#define NUM_SND_CHANS    2
#define AMPLITUDE        32767
static int dspfd=-1;              /* SOUND file descriptor              */
static int dsplen=2048;           /* buffer size in samples             */
static int dspfs=0;               /* current sampling frequency         */
static int dspchn=0;              /* current number of channels         */
static char *snd_file="/dev/dsp";

static void sound_setfs() {
  int tmp,fs;
  fs = (int)getNumVal(a.v.hfs);
  tmp = fs;
  if (ioctl(dspfd, SNDCTL_DSP_SPEED, &tmp)) {
   alpsError("ioctl(dspfd,SNDCTL_DSP_SPEED,#FS)");
   exit(1);
  }
  if (fs != tmp) {
    alpsOutsn(alpserr,5,
	      "SampFreq ",itos(fs)," unsupported, gives ",itos(tmp),"\n");
    exit(1);
  }
  dspfs = fs;
  //  alpsOutsn(alpserr,3, "SampFreq ",itos(fs),"Hz\n");
}

static int sound_set_chan(int n) {
  int tmp = n;
  if (ioctl(dspfd, SNDCTL_DSP_CHANNELS, &tmp)) {
    alpsError("ioctl(dspfd,SNDCTL_DSP_CHANNELS)");
    return 0;
  }
  if (tmp != n) {
    alpsOuts(alpserr,"Couldn't set number of channels on /dev/dsp\n");
    dspchn = 0;
    return 0;
  }
  dspchn = n;
  return 1;
}

pointer sound_sync() {
  if (ioctl(dspfd, SNDCTL_DSP_SYNC, NULL) < 0) {
    alpsError("ioctl dsp post failed\n");
    return pnil;
  } else {
    return ptru;
  }
}

int sound_get_len() {
  int tmp;
  ioctl (dspfd, SNDCTL_DSP_GETBLKSIZE, &tmp);
  if (tmp == -1) {
    alpsError ("ioctl(dspfd, SNDCTL_DSP_GETBLKSIZE, &dspbuflen)");
    return -1;
  }
  
  tmp /= 2; /* we work in short samples */
  //  alpsOutsn(alpserr,3," Using dsp buf len ",itos(tmp)," samples\n");

  return tmp;
}

pointer sound_init() {
  /* return working sample buffer len */
  int tmp;
  static int samplesize = 16;

  if (dspfd >= 0) close(dspfd);
  
  if ((dspfd = open (snd_file, O_RDWR, 0)) == -1) {
    alpsError("Cannot open snd_file\n");
    return pnil;
  }

  ioctl(dspfd, SNDCTL_DSP_RESET, 0);

  sound_setfs(); /* set sample freq */

   
  tmp = samplesize;
  if (ioctl(dspfd, SNDCTL_DSP_SAMPLESIZE, &samplesize) == -1) {
    alpsError (" unable to set sample size on snd_file\n");
    exit(1);
  }
  if (tmp != samplesize) {
    alpsOutsn(alpserr,3,"Sample size ",itos(tmp)," not supported!\n");
    return pnil;
  }

#if 1
  tmp = 0x80008;
  if (ioctl(dspfd, SNDCTL_DSP_SETFRAGMENT, &tmp)) {
    alpsError (" unable to set fragment size on snd_file\n");
    exit(1);
  }
#endif
 
  if (!sound_set_chan(NUM_SND_CHANS)) return pnil;

  dsplen = sound_get_len();
  //  alpsOutsn(alpserr,5,"frag size ",itos(dsplen)," fs ", itos(dspfs), " \n");
  
  return ptru; //sound_sync();
}

static short dspin[2*MAX_SND_BUF_LEN];
static short dspout[2*MAX_SND_BUF_LEN];
static int sound_read(int slen) { /* num samples , 2 chan ,* sizof(short) */
  int n,blen;

  blen = slen*2*sizeof(short);
  ASSERT(blen<=sizeof(dspin));
  n = read(dspfd,dspin, blen); /* always seem to get full samples */
  if (n != blen) {
    if (n < 0) alpsError("read(dspfd...)");
    else alpsOutsn(alpserr,5,"got ",itos(n)," bytes, wanted ",itos(blen),"\n");
  }
  return (n/2);
}

static void sound_write(int slen) {
  int len,blen;

  blen = slen*sizeof(short);
  len = write(dspfd,dspout,blen);
  if (len < 0) {
    alpsError("write(dspfd...)");
      return;
  }
  if (len != blen)
    alpsOutsn(alpserr,5,"Short write to dsp wanted ",itos(blen)," did ",
	      itos(len),"\n");
}

void sound_close() {
  if (ioctl(dspfd, SNDCTL_DSP_RESET, 0)) 
    alpsError("Couldn't reset DSP on close\n");
  close(dspfd);
  dspfd = -1;
}

pointer alps_rec(int n, pointer p) {
  int len,ret,lnd;
  int i,j,k;
  pointer lex,res;
  res.it = 0;
  if (dspfd < 0) error(ios_err,"Sound not initialised");
  if ((n != 1) || !isNum(lex=arg1(p)) ||
      (0>=(len=(vadd(lex).np[0]*getNumVal(a.v.hfs)+iround))) ||
      (len > MAX_SND_BUF_LEN)) error(inv_funarg,"rec too many samples");
  sound_setfs();
  ret = sound_read(len);
  lnd = ret/len;
  ASSERT(lnd*len==ret);
  res = alloc_sb(len,lnd);
  lex = vadd(res);
  for (k=i=0;i<len;i++)
    for (j=0;j<lnd;j++)
      lex.np[i+(j*len)] = dspin[k++];
  ASSERT(k==ret);
  return(res);  /* protected by sendres (eq (p res) [num_chans len]) => t */
}

pointer alps_play(int n, pointer p) {
  int len,mlen,ret,lnd;
 int i,j,k,nsamps,tsamp;
  pointer lex,rex,res;
  if (dspfd < 0) error(ios_err,"Sound not initialised");
  if ((n < 1) || (n > 2) ||
      (!isNum(lex=arg1(p)) || ((n==2) && (!isNum(rex=arg2(p))))))
      error(inv_funarg,"play buf [duration]");
  lnd = dims(lex);
  if ((lnd < 1) || (lnd > 2)) error(dom_range,"play (p buf)={1,2}");
  if (lnd == 2) { lnd = getDim(lex,0); len = getDim(lex,1); }
  else          { lnd = 1;             len = nels(lex);     }
  if (n==2) nsamps = dspfs*getNum(rex); /* fs * duration  */
  else nsamps = len;
  // alpsOutsn(alpserr,3,"play: ",itos(nsamps*lnd)," total samples\n");
  if (nsamps <= 0) return(ptru);
  rex = vadd(lex);
  tsamp = 0;
  alpsBlockSignals();
  while (tsamp < nsamps) {
    mlen = nsamps - tsamp;
    if (mlen > dsplen) mlen = dsplen;
    for (i=k=0;i<mlen;i++) {
      dspout[k++] = AMPLITUDE * rex.np[(tsamp + i) % len];
      dspout[k++] = AMPLITUDE * rex.np[(tsamp + i) % len + (lnd > 1)*len];
    }
    //alpsOutsn(alpserr,3,"play: ",itos(k)," samples\n");
    sound_write(k);
    tsamp += mlen;
  }
  sound_sync();
  alpsUnblockSignals();
  return(ptru);
}

void alps_beep(int f, int d) { // frequency duration
  int k,nsamps,blen,len,init_flag=1;
  double freq,rps;

  if (dspfd < 0) {
    sound_init();
    init_flag=0;
  }
  if (!f) return; // * zero freq => no sound
  nsamps = d*dspfs/100;
  freq = f*81.38; // Pascal 3.2 Procedure Library 14-9
  rps = (freq*2*pi)/dspfs;
  for (k=0; k<nsamps; k++) dspout[k] = 32767*sin(rps*k);
  blen = nsamps*sizeof(short);
  alpsBlockSignals();
  len = write(dspfd,dspout,blen);
  if (len < 0) {
    error(ios_err,"write to dsp failed\n");
    return;
  }
  if (len != blen)
    alpsOutsn(alpserr,5,"Short write to dsp wanted ",itos(blen),
	      " got ",itos(len),"\n");
  sound_sync();
  alpsUnblockSignals();
  if (!init_flag) sound_close();
}



#endif /* SOUND */
