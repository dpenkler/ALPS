/*  ************************* SOUND SECTION *********************  */
#include "alps.h"
#if (SOUND!=false)

#include <linux/soundcard.h>
#include <stdint.h>
#define MAX_SND_BUF_LEN  48000*10
#define NUM_SND_CHANS    2
#if (SOUND==1) /* 16 bit samples */
#define SAMP_SIZE        16
#define AMPLITUDE        ((2<<(SAMP_SIZE - 2)) - 1)
#define SAMP_TYPE        short
#define SAMP_FORMAT      AFMT_S16_NE
#endif
#if (SOUND==2) /* 32 bit samples */
#define SAMP_SIZE        32
#define AFMT_S32_LE      0x00001000  /* missing from soundcard.h */
#define AMPLITUDE        ((uint32_t)(2<<(SAMP_SIZE - 2)) - 1)
#define SAMP_TYPE        int
#define SAMP_FORMAT      AFMT_S32_LE
#endif
static int dspfd=-1;              /* SOUND file descriptor              */
static int dsplen=2048;           /* buffer size in samples             */
static int dspfs=0;               /* current sampling frequency         */
static int dspchn=0;              /* current number of channels         */

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

static pointer sound_sync() {
  if (ioctl(dspfd, SNDCTL_DSP_SYNC, NULL) < 0) {
    alpsError("ioctl dsp post failed\n");
    return pnil;
  } else {
    return ptru;
  }
}

static int sound_get_len() {  /* returns number of samples */
  int tmp;
  ioctl (dspfd, SNDCTL_DSP_GETBLKSIZE, &tmp);
  if (tmp == -1) {
    alpsError ("ioctl(dspfd, SNDCTL_DSP_GETBLKSIZE, &dspbuflen)");
    return -1;
  }
  
  tmp /= (SAMP_SIZE / 8); /* convert number of octets to  samples */
  //  alpsOutsn(alpserr,3," Using dsp buf len ",itos(tmp)," samples\n");

  return tmp;
}

pointer sound_init() {
  /* return working sample buffer len */
  int tmp;
  char snd_file[32];
  static int sample_format = SAMP_FORMAT;

  if (dspfd >= 0) close(dspfd);

  strcpy(snd_file, "/dev/dsp");
  if ((tmp = (int)getNumVal(a.v.hsc)))
    strcat(snd_file, (char *)itos(tmp));
    
  if ((dspfd = open (snd_file, O_RDWR, 0)) == -1) {
    alpsError(mkEmes(3, "Cannot open ", snd_file, "\n"));
    return pnil;
  }

  ioctl(dspfd, SNDCTL_DSP_RESET, 0);

  sound_setfs(); /* set sample freq */

   
  tmp = sample_format;
  if (ioctl(dspfd, SNDCTL_DSP_SETFMT, &sample_format) == -1) {
    alpsError (" unable to set sample size on snd_file\n");
    exit(1);
  }
  if (tmp != sample_format) {
    alpsOutsn(alpserr,3,"Sample format ",itos(tmp)," not supported!\n");
    return pnil;
  }

#if 0
  tmp = 0x80008;
  if (ioctl(dspfd, SNDCTL_DSP_SETFRAGMENT, &tmp)) {
    alpsError (" unable to set fragment size on snd_file\n");
    exit(1);
  }
#endif
 
  if (!sound_set_chan(NUM_SND_CHANS)) return pnil;

  dsplen = sound_get_len();
  //  alpsOutsn(alpserr,5,"frag size ",itos(dsplen)," fs ", itos(dspfs), " \n");
  alpsOutsn(alpsout,7,"Sound initialised on ", snd_file, ".  Amplitude: ", itos(AMPLITUDE), " DSPLEN ", itos(dsplen), "\n");
  
  return ptru; //sound_sync();
}

static SAMP_TYPE dspin[2*MAX_SND_BUF_LEN];
static SAMP_TYPE dspout[2*MAX_SND_BUF_LEN];
static int sound_read(int slen) { /* num samples */
  int n,blen;

  blen = slen*NUM_SND_CHANS*sizeof(SAMP_TYPE);
  ASSERT(blen<=sizeof(dspin));
  n = read(dspfd,dspin, blen); /* always seem to get full samples */
  if (n != blen) {
    if (n < 0) alpsError("read(dspfd...)");
    else alpsOutsn(alpserr,5,"got ",itos(n)," bytes, wanted ",itos(blen),"\n");
  }
  return (n/sizeof(SAMP_TYPE));
}

static void sound_write(int slen) {
  int len,blen;

  blen = slen*sizeof(SAMP_TYPE);
  len = write(dspfd,dspout,blen);
  if (len < 0) {
    alpsError("write(dspfd...)");
      return;
  }
  if (len != blen)
    alpsOutsn(alpserr,5,"Short write to dsp wanted ",itos(blen)," did ",
	      itos(len),"\n");
}

pointer sound_close() {
  if (dspfd < 1) return pnil;
  if (ioctl(dspfd, SNDCTL_DSP_RESET, 0)) 
    alpsError("Couldn't reset DSP on close\n");
  close(dspfd);
  dspfd = -1;
  return ptru;
}

pointer alps_rec(int n, pointer p) { /* number of seconds */
  int len,ret,lnd;
  int i,j,k;
  pointer lex,res;
  res.it = 0;
  if (dspfd < 0) error(ios_err,"Sound not initialised");
  if ((n != 1) || !isNum(lex=arg1(p)) || nels(lex) != 1)
    error(inv_funarg,"(rec <seconds>: numeric singleton)");
  len=(vadd(lex).np[0]*getNumVal(a.v.hfs)+iround);
  if (len <= 0) error(inv_funarg,"(rec <duration>) duration not positive");
  if (len > MAX_SND_BUF_LEN) error(inv_funarg,"rec too many samples");
  sound_setfs();
  alpsBlockSignals();
  ret = sound_read(len);
  alpsUnblockSignals();
  lnd = ret/len;
  ASSERT(lnd*len==ret);
  res = alloc_sb(len,lnd);
  lex = vadd(res);
  for (k=i=0;i<len;i++)
    for (j=0;j<lnd;j++)
      lex.np[i+(j*len)] = (number)dspin[k++] / AMPLITUDE;
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
  if (d > 100) d = 100; /* longest beep is a nerve-racking 1 second */
  nsamps = d*dspfs/100;
  freq = f*81.38; // Pascal 3.2 Procedure Library 14-9
  rps = (freq*2*pi)/dspfs;
  for (k=0; k<nsamps; k++) dspout[k] = dspout[k+nsamps] = AMPLITUDE*sin(rps*k);
  blen = NUM_SND_CHANS*nsamps*sizeof(SAMP_TYPE);
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
