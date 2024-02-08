/*** sound.h include file ***/
extern pointer sound_init();
extern pointer alps_rec(int n, pointer p);
extern pointer alps_play(int n, pointer p);
extern void    alps_beep(int f, int d);
extern void    sound_close();
