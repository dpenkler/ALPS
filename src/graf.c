
/*  ************************* GRAPHICS SECTION *********************  */
#include "alps.h"

#if (GRAF==true)

#include <EZ.h>

static number p1,p2,p3; //FIXME

static const float colarr[16][3] = {
  {  0,  0,  0},
  {1.0,1.0,1.0},
  {1.0,  0,  0},
  {1.0,0.5,  0},
  {1.0,1.0,  0},
  {0.5,1.0,  0},
  {  0,1.0,  0},
  {  0,1.0,0.5},
  {  0,1.0,1.0},
  {  0,0.5,1.0},
  {  0,  0,1.0},
  {0.5,  0,1.0},
  {1.0,  0,1.0},
  {1.0,0.5,1.0},
  {1.0,0.5,0.5},
  {0.5,0.5,0.5}};

/* graphics globals */
int ez_fd;                        /* Graphics fd used for select loop   */
int gflag=0;                      /* graphics service request flag      */
static int ez_dw,ez_dh;           /* width and height of display        */

void alpsInitGraphics(int argc, char *argv[]) {
  if (0 <= EZ_Initialize(argc,argv,1)) {
    ez_fd = ConnectionNumber(EZ_GetDisplay());
    gflag = 0;
    EZ_GetDisplaySize(&ez_dw, &ez_dh);
    setSym(a.v.hgw,mkNum(ez_dw)); /* export graphics screen size system vars */
    setSym(a.v.hgh,mkNum(ez_dh));
    p1=p2=p3=0.0; //FIXME
  } else {
    ez_fd = -1;
  }
}

void alpsDestroyWidget(pointer lex) {
  EZ_Widget *w = lex.fb->data2.vd;
  if (w != NULL) {
    EZ_DestroyWidget(w); /* The pointer will be zapped by EZ destroy callback */
    // alpsOutsn(alpserr,3,"widSweeper destroyed ",itox(lex.wh),"\n");
  }
}

void alpsServiceWidgets() { // need this to avoid include EZ.h in alps.c
  EZ_ServiceEvents();
}

void alpsEZDestroyCB(EZ_Widget *widget, void *pdata) {
  pointer lex;
  if (widget != NULL) {
    lex.vd = pdata;
    ASSERT(isWidget(lex));
    lex.fb->data2.pt = NULL;
    alpsOutsn(alpserr,3,"destroyCB zapping widget for ",itox(lex.wh),"\n");
  }
}

static pointer allocWidget(pointer parent, EZ_Widget *w) {
  pointer lex,rex;  /* Law and King comme d'hab */
  rex.vd = w;
  lex    = allocFob(wdgt,pnil,parent,rex,0);
  EZ_AddWidgetCallBack(w,EZ_DESTROY_CALLBACK,alpsEZDestroyCB,lex.vd,0);
  EZ_SetWidgetPtrData(w,lex.vd);
  return(lex);
}

static EZ_Widget *getWidget(pointer lex) {
  EZ_Widget *ret = NULL;
  if (isWidget(lex)) ret = lex.fb->data2.vd;
  else ASSERT(isNil(lex));
  return(ret);
}

/* Must set and display canvas before calling **/
pointer glinit() {
  if (ez_fd < 0) return(pnil);
  number *cc = getNumVec(a.v.hcc);    /* Clear Colour Vector                  */
  EZ_SetBackBuffer(EZ_PIXMAP);        /* set back buffer for X11 prims        */
  EZ_DrawBuffer(EZ_BACK);             /* Allocate back buffer in case         */
  EZ_DrawBuffer(isNil(lookup(a.v.hgb)) ? EZ_FRONT : EZ_BACK); /* set for real */
  EZ_RGBMode();                       /* we are using RGB mode                */
  EZ_FrontFace(EZ_CCW);               /* front face is counter clockwise      */
  //  EZ_LogicOp(EZ_SET);
  EZ_ClearColorf(cc[0],cc[1],cc[2],cc[3]);
  return(ptru);
}

static bool alps_light = false;

pointer gclear() {
  EZ_Clear(EZ_COLOR_BUFFER_BIT | EZ_DEPTH_BUFFER_BIT);
  return(ptru);
}


pointer genable(int n, pointer p) {
  int i,mode;
  pointer lex;
  if (ez_fd < 0) return(pnil);
  for (i=0;i<n;i++) {
    mode = getInt(lex = argn(i,p));
    EZ_Enable(mode);
    if (mode == EZ_LIGHTING) alps_light = true;
  }
  return(ptru);
}

pointer gdisable(int n, pointer p) {
  int i,mode;
  pointer lex;
  if (ez_fd < 0) return(pnil);
  for (i=0;i<n;i++) {
    mode = getInt(lex = argn(i,p));
    EZ_Disable(mode);
    if (mode == EZ_LIGHTING) alps_light = false;
  }
  return(lex);
}

pointer gmaterial(int n, pointer p) {
  int i,face,propName;
  float propVal[4];
  pointer lex;
  if (ez_fd < 0) return(pnil);
  face     = getInt(arg1(p));
  propName = getInt(arg2(p));
  lex      = arg3(p);
  for (i=0;i<min(nels(lex),4);i++) propVal[i] = vadd(lex).np[i];
  EZ_Materialfv(face,propName,propVal);
  return(arg3(p));
}

pointer glight(int n, pointer p) {
  int i,lightNum,propName;
  float propVal[4];
  pointer lex;
  if (ez_fd < 0) return(pnil);
  lightNum = getInt(arg1(p));
  propName = getInt(arg2(p));
  lex      = arg3(p);
  for (i=0;i<min(nels(lex),4);i++) propVal[i] = vadd(lex).np[i];
  EZ_Lightfv(lightNum,propName,propVal);
  return(arg3(p));
}

pointer glightmod(int n, pointer p) {
  int i,propName;
  float propVal[4];
  pointer lex;
  if (ez_fd < 0) return(pnil);
  propName = getInt(arg1(p));
  lex      = arg2(p);
  for (i=0;i<min(nels(lex),4);i++) propVal[i] = vadd(lex).np[i];
  EZ_LightModelfv(propName,propVal);
  return(lex);
}

pointer gshademod(int n, pointer p) {
  int shadeModel;
  pointer lex;
  if (ez_fd < 0) return(pnil);
  lex = arg1(p);
  shadeModel = getInt(lex);
  EZ_ShadeModel(shadeModel);
  return(lex);
}

static int alps_proj = 0;
pointer proj(int p) { /* if(p==false) then ortho else perspective proj */
  number  *wn,*nf,*op,*of,*vp;
  if (ez_fd < 0) return(pnil);
  wn = getNumVec(a.v.hwn); /* xy window */
  nf = getNumVec(a.v.hnf); /* Near and Far planes along Z axis */
  op = getNumVec(a.v.hop); /* Observer Position */
  of = getNumVec(a.v.hof); /* Observer Focus    */
  vp = getNumVec(a.v.hvp); /* Viewport */
  /* the line through OP and OF is the Z axis */
  if (p) {
    EZ_MatrixMode(EZ_PROJECTION);
    EZ_LoadIdentity();
    EZ_Frustum(wn[0], wn[1], wn[2], wn[3], nf[0], nf[1]);
    EZ_MatrixMode(EZ_MODELVIEW);
    EZ_LoadIdentity();
    EZ_LookAt(op[0], op[1], op[2], of[0], of[1], of[2], 0.0, 1.0, 0.0); /* y is up */
  } else {
    EZ_MatrixMode(EZ_MODELVIEW);
    EZ_LoadIdentity();
    EZ_Ortho(wn[0], wn[1], wn[2], wn[3], nf[0], nf[1]);
  }
  EZ_ViewPort(vp[0], vp[1], vp[2], vp[3]);

  alps_proj = true;
  return(p?ptru:pnil);
}

#define alpn(a,b,c) EZ_Normal3f(a,b,c)
#define alpv(a,b,c) EZ_Vertex3f(a,b,c)

static inline void normalise(number V[3]) { /* warning in situ replacement! */
  number R = V[0]*V[0] + V[1]*V[1] + V[2]*V[2];
  R = sqrt(R);
  if (R < 1e-30) return;
  V[0] /= R;
  V[1] /= R;
  V[2] /= R;
  return;
}

#define xprod(V,U,Z) \
 normalise(V); \
 normalise(U); \
 Z[0] = U[1]*V[2] - U[2]*V[1]; \
 Z[1] = U[2]*V[0] - U[0]*V[2]; \
 Z[2] = U[0]*V[1] - U[1]*V[0]
/* points clockwise */
static inline void cnorm(number *mesh, int rep, int p1,int p2, int p3, int p4) 
{
  number R,U[3],V[3],Z[3];
  int rep2=rep+rep;
  int flag =0;
  U[0] = mesh[p2]        - mesh[p1];
  U[1] = mesh[p2 + rep]  - mesh[p1 + rep];
  U[2] = mesh[p2 + rep2] - mesh[p1 + rep2];
  R = sqrt(U[0]*U[0] + U[1]*U[1] + U[2]*U[2]);
  if (R == 0.0) {
    U[0] = mesh[p3]        - mesh[p1];
    U[1] = mesh[p3 + rep]  - mesh[p1 + rep];
    U[2] = mesh[p3 + rep2] - mesh[p1 + rep2];
    R = sqrt(U[0]*U[0] + U[1]*U[1] + U[2]*U[2]);
    //alpsOutsn(alpserr,4,itos(p1)," p2-p1 zero ",rtos(R,10,3),"\n");
    flag = 1;
    if (R == 0.0) return;
  }
  U[0] /= R;  U[1] /= R;  U[2] /= R; /* normalise */
  V[0] = mesh[p4]        - mesh[p1];
  V[1] = mesh[p4 + rep]  - mesh[p1 + rep];
  V[2] = mesh[p4 + rep2] - mesh[p1 + rep2];
  R = sqrt(V[0]*V[0] + V[1]*V[1] + V[2]*V[2]);
  if (R == 0.0) {
    if (flag) return; /* normal will be zero */
    V[0] = mesh[p3]        - mesh[p1];
    V[1] = mesh[p3 + rep]  - mesh[p1 + rep];
    V[2] = mesh[p3 + rep2] - mesh[p1 + rep2];
    R = sqrt(V[0]*V[0] + V[1]*V[1] + V[2]*V[2]);
    //alpsOutsn(alpserr,2,itos(p1)," p4-p1 zero\n");
    if (R == 0) return;
  }
  V[0] /= R;  V[1] /= R;  V[2] /= R;
  Z[0] = U[2]*V[1] - U[1]*V[2];
  Z[1] = U[0]*V[2] - U[2]*V[0];
  Z[2] = U[1]*V[0] - U[0]*V[1];
  normalise(Z);
  alpn(Z[0],Z[1],Z[2]);
}

static void resetFont() {
  alpsOutsln(alpserr,"Warning: invalid font; #FN reset to large vector");
  setSym(a.v.hfn,mkNum(EZ_LARGE_VECTOR_FONT));
  EZ_SetVectorFont(EZ_LARGE_VECTOR_FONT);
}

pointer plot(integer narg, pointer p) {
  
  pointer lex,plst,res,rex;
  integer i,j,n,dimd,rep,rep2,pen,mode,pmode,mmode,style,width,font_set=0;
  uint tmp;
  number tem,xlo,xhi,*win;
  if (ez_fd < 0) return(pnil);
  win   = getNumVec(a.v.hwn);
  xlo   = win[0];
  xhi   = win[1];
  pmode = getNumVal(a.v.hpm);
  pen   = getNumVal(a.v.hpn);
  mode  = getNumVal(a.v.hdm);
  tmp   = getNumVal(a.v.hls);

  style = tmp & 3;
  if (style > 2) {
    style=0;
    alpsOutsln(alpserr,"Warning: invalid line style in #LS reset to 0");
    tmp &= 0xffc;
    setSym(a.v.hls,mkNum(tmp));
  }
  width = tmp >> 2;
  if (!alps_proj) proj(false); /* If not commited it is Orthonormal */
  /* #GB if using back buffer must call gupdate explicitly */
  EZ_DrawBuffer(isNil(lookup(a.v.hgb))? EZ_FRONT : EZ_BACK);
  EZ_SetLineStyle(style,width);
  if (narg > 2) error(inv_numarg,"(graf [mode] points]");
  //alpsOutsn(alpserr,3,"Mode is ",itos(mode),"\n");
  EZ_PolygonMode(EZ_FRONT_AND_BACK,pmode);
  if (narg == 2) {
    res = plst = arg2(p);
    mmode = getInt(arg1(p));
    if (mmode < 0 || mmode > 2)
      error(inv_funarg,"Move mode can only be 0,1 or 2");
  } else {
    mmode = 0;
    res = plst = arg1(p);
  }
  if (!isList(plst)) plst = cons(plst,pnil);
  protect(plst); /* does not hurt to protect arg */
  for (;!isNil(plst);plst=cdr(plst)) { /* object in list */
    lex = car(plst);
    if (isNum(lex)) {
      dimd = dims(lex);
      if ((dimd > 3) || (dimd < 1)) error(dom_range,"rank={1,2,3}");
      rep = 1.0;
      if (dimd > 1) for (i=1;i<dimd;i++) rep *= getDim(lex,i);
      else rep = getDim(lex,0);
      rep2 = rep+rep;
      rex = vadd(lex);
      if (rep == 1) {
	EZ_Begin(mode);
	EZ_Color3fv((float *)colarr[pen]);
	  if (mmode == 1) { // absolute from last point to this
	  if (dimd == 2)
	  switch (getDim(lex,0)) {
	  case 1:  EZ_Vertex2f(xlo,rex.np[0]);                      break;
	  case 2:
	    EZ_Vertex2f(p1,p2);
	    EZ_Vertex2f(p1=rex.np[0],p2=rex.np[rep]);
	    break;
	  case 3:
	  case 4:
	    EZ_Vertex3f(p1,p2,p3);
	    EZ_Vertex3f(p1=rex.np[0],p2=rex.np[rep],p3=rex.np[rep2]);
	    break;
	  default: error(dom_range,"ords in {1,2,3,4}");
	  } else EZ_Vertex2f(xlo,rex.np[0]);
	} else 	if (mmode == 2) { // relative from last point to this
	  if (dimd == 2)
	  switch (getDim(lex,0)) {
	  case 1:  EZ_Vertex2f(xlo,rex.np[0]);                      break;
	  case 2:
	    EZ_Vertex2f(p1,p2);
	    EZ_Vertex2f(p1+=rex.np[0],p2+=rex.np[rep]);
	    break;
	  case 3:
	  case 4:
	    EZ_Vertex3f(p1,p2,p3);
	    EZ_Vertex3f(p1+=rex.np[0],p2+=rex.np[rep],p3+=rex.np[rep2]);
	    break;
	  default: error(dom_range,"ords in {1,2,3,4}");
	  } else EZ_Vertex2f(xlo,rex.np[0]);
	} else { // mmode 0 
	  if (dimd == 2)
	  switch (getDim(lex,0)) {
	  case 1:  EZ_CMove2f(xlo,rex.np[0]);                       break;
	  case 2:  EZ_CMove2f(p1 = rex.np[0], p2 =rex.np[rep]);     break;
	  case 3:
	  case 4:  EZ_CMove3f(p1=rex.np[0],p2=rex.np[rep],p3=rex.np[rep2]);
	    break;
	  default: error(dom_range,"ords in {1,2,3,4}");
	  } else EZ_CMove2f(xlo,rex.np[0]);
	}
	EZ_End();
      } else {
	tem = (xhi-xlo)/(double)(rep-1);
	if (mode==EZ_TMESH || mode==EZ_QMESH) {
	  if (dimd != 3) error(dom_range,"mesh modes need rank 3 arg");
	  int uind,vind;
	  int ncols = getDim(lex,2);
	  int nrows = getDim(lex,1);
	  number *mesh = vadd(lex).np;
	  ASSERT(rep <= 1048576); /* EZ_GL.h MAX_NUM_VTS */
	  EZ_Begin(mode);
	  EZ_Color3fv((float *)colarr[pen]);
	  EZ_SetTQMeshSize(nrows,ncols);
	  if (alps_light && (getDim(lex,0) == 3 || getDim(lex,0) == 4)) {
	    for (i=0;i<nrows-1;i++) {
	      for (j=0;j<ncols-1;j++) {
		uind = i*ncols + j;
		vind = (i+1)*ncols + j;
		cnorm(mesh,rep,uind,uind+1,vind+1,vind);
		alpv(mesh[uind],mesh[uind + rep],mesh[uind + rep2]);
	      }
	      uind = i*ncols + j; /* last column */
	      vind = (i+1)*ncols + j;
	      cnorm(mesh,rep,uind-1,uind,vind,vind-1);
	      alpv(mesh[uind],mesh[uind + rep],mesh[uind + rep2]);
	    }
	    for (j=0;j<ncols-1;j++) { /* last row */
	      uind = (nrows - 1)*ncols + j;
	      vind = (nrows - 2)*ncols + j;  /* No wrap */
	      cnorm(mesh,rep,vind,vind+1,uind+1,uind);
	      alpv(mesh[uind],mesh[uind + rep],mesh[uind + rep2]);
	    }
	    uind = nrows*ncols - 1; /* very last element */
	    vind = (nrows - 1)*ncols  - 1;
	    cnorm(mesh,rep,uind-1,vind-1,vind,uind);
	    alpv(mesh[uind],mesh[uind + rep],mesh[uind + rep2]);
	    EZ_End();
	    continue; /* for objects in list */
	  }
	}
        if (mode==EZ_QUAD_STRIP) {
	  if (dimd != 3) error(dom_range,"QUAD_STRIP mode needs rank 3 arg");
	  int uind,vind;
	  int ncols = getDim(lex,2);
	  int nrows = getDim(lex,1);
	  number *mesh = vadd(lex).np;
	  if (alps_light && (getDim(lex,0) == 3 || getDim(lex,0) == 4)) {
	    for (i=0;i<nrows-1;i++) {
	      uind = i*ncols;
	      vind = (i+1)*ncols;
	      EZ_Begin(mode);
	      EZ_Color3fv((float *)colarr[pen]);
	      cnorm(mesh,rep,uind,uind+1,vind,vind+1);
	      alpv(mesh[uind],mesh[uind + rep],mesh[uind + rep2]);
	      alpv(mesh[vind],mesh[vind + rep],mesh[vind + rep2]);
	      for (j=1;j<ncols;j++) {
		uind++;
		vind++;
		alpn(0.,0.,1.);
		cnorm(mesh,rep,uind-1,uind,vind,vind-1);
		alpv(mesh[uind],mesh[uind + rep],mesh[uind + rep2]);
		alpv(mesh[vind],mesh[vind + rep],mesh[vind + rep2]);
	      }
	      EZ_End();
	    }
	    continue; /* for objects in list */
	  }
	}
	if (dimd > 1) {
	  switch (getDim(lex,0)) {
	  case 1:
	    EZ_Begin(mode);
	    EZ_Color3fv((float *)colarr[pen]);
	    for (i=0;i<rep;i++)
	      EZ_Vertex2f(xlo+(float)i*tem,rex.np[i]);
	    EZ_End();
	    break;
	  case 2:
	    EZ_Begin(mode);
	    EZ_Color3fv((float *)colarr[pen]);
	    for (i=0;i<rep;i++) {
	      EZ_Vertex2f(rex.np[i],rex.np[rep+i]);
	    }
	    EZ_End();
	    break;
	  case 3:
	  case 4: /* ord 4 ignored (only used for transforms) */
	    { /* for rank 3 each line is plotted separately */
	      int npoly = (dimd==3) ? getDim(lex,2) : 1;
	      int npnts = getDim(lex,1);
	      int mrep  = npnts * npoly;
	      int mrep2 = mrep + mrep;
	      int ind;
		for (i=0;i<npoly;i++) {
		  EZ_Begin(mode);
		  EZ_Color3fv((float *)colarr[pen]);
		  for (j=0;j<npnts;j++) {
		    ind = i + j*npoly;
		    EZ_Vertex3f(rex.np[ind],rex.np[mrep+ind],rex.np[mrep2+ind]);
		  }
		  EZ_End();
		}
	    }
	    break;
	  default: error(dom_range,"ords in {1,2,3,[4]}");
	  }
	} else { /* dimd == 1 */
	  EZ_Begin(mode);
	  EZ_Color3fv((float *)colarr[pen]);
	  for (i=0;i<rep;i++) EZ_Vertex2f(xlo+(float)i*tem,rex.np[i]);
	  EZ_End();
	}
      }
    } else if (isaChr(lex)) {
      int font_type,font_num;
      idstr font_name;
      pointer font = lookup(a.v.hfn);
      if (!font_set) { /* need to set the font */
	/* TODO: cache current font */
	if (isNum(font)) {
	  font_type = EZ_VECTOR_FONT;
	  font_num  = getNum(font);
	  if (font_num != EZ_SMALL_VECTOR_FONT &&
	      font_num != EZ_LARGE_VECTOR_FONT) {
	    resetFont();
	  } else {
	    EZ_SetVectorFont(font_num);
	  }
	} else if (isChr(font)) {
	  font_type = EZ_BITMAP_FONT;
	  mkString(font,font_name,"font name");
	  EZ_SetBitmapFont((char *)font_name);
	} else {
	  resetFont();
	}
	font_set = 1;
      }
      n = nels(lex);
      char TmpBuf[n+1];
      float crot = getNumVal(a.v.hcr); 
      memcpy(TmpBuf,vadd(lex).sp,n);
      TmpBuf[n] = 0;
      EZ_DrawString(font_type,TmpBuf); // Currently no way to rotate in EZWGL
    } else  error(inv_funarg,"plot [num | chr]");
  } /* for objects in list */
  unprotn(1); /* plst in case we had to cons */
  gflag = true;
  return(res);
}

/* widget fidget */

/* alps EZ Callback handler */
static void alpsEZCB(EZ_Widget *widget, void *pdata) {
  pointer lex,rex,cbexp;
  if (ez_fd < 0) return;
  rex.vd = EZ_GetWidgetPtrData(widget);
  lex.vd = pdata; /* FIXME this better be protected elsewhere */
  scheduleIOCB(lex, rex, 1);
}

/* alps EZ Event handler*/
void alpsEZEV(EZ_Widget *widget, void *pdata,
		      int eventType, XEvent *xevent) {
  pointer lex,rex,cbexp,p,p1,p2,p3,tex;
  float   fx,fy;
  int     h,w;
  rex.vd = EZ_GetWidgetPtrData(widget);
  lex.vd = pdata; /* FIXME this better be protected elsewhere */
  p1     = mkNum(eventType);
  protect(p1);
  p2 = pnil;
  p3 = pnil;
  switch (eventType) {
  case EZ_KEY_PRESS:       p2 = mkNum(EZ_PressedKey);  break;
  case EZ_BUTTON1_PRESS:
  case EZ_BUTTON2_PRESS:
  case EZ_BUTTON3_PRESS:
  case EZ_BUTTON1_RELEASE:
  case EZ_BUTTON2_RELEASE:
  case EZ_BUTTON3_RELEASE:
  case EZ_POINTER_MOTION:
    EZ_GetWidgetDimension(widget, &w, &h);
    EZ_Screen2World(EZ_PointerCoordinates[0],h-EZ_PointerCoordinates[1],
		    &fx,&fy);
    protect(p2 = mkNum(fx));
    protect(p3 = mkNum(fy));
    break;
  default:
    break;
  }
  protect(tex = getfs(4)); /* build list of values for single arg */
  p = tex;
  p.rg->ar = rex;    /* car widget              */
  p = p.rg->dr;
  p.rg->ar = p1;     /* cadr event              */
  p = p.rg->dr;
  p.rg->ar = p2;     /* caddr Key or pointer 1  */
  p = p.rg->dr;
  p.rg->ar = p3;     /* cadddr pointer 2        */
  unprotall();
  scheduleIOCB(lex, tex, 0);
}

void alpsCfgWidget(EZ_Widget *w, pointer conf) {
  pointer lex,ptr,ent,args;
  float frv1,frv2;
  int wtype,resource,rv,rv1,rv2,rv3,rv4;
#define MAX_STR_RSRC_LEN 255
  char cr[MAX_STR_RSRC_LEN+1];
  if (ez_fd < 0) return;
  while (!isNil(conf)) {
    ent = car(conf);
    if (!isList(ent) || (listLen(ent) < 2))
      error(inv_funarg,"mkwdgt needs list of <res val..>");
    ptr = car(ent);
    args = cdr(ent);
    if (!isNum(ptr)) error(inv_funarg,"mkwdgt: resource must be numeric");
    resource = getInt(ptr);
    switch (resource) {
    case EZ_CALLBACK:
      lex = car(args); /* no closure here so user can choose */
      EZ_ConfigureWidget(w,EZ_CALLBACK,alpsEZCB,lex.vd,0);
      break;
   case  EZ_EVENT_HANDLER:
      lex = car(args); /* no closure here so user can choose */
      EZ_ConfigureWidget(w,EZ_EVENT_HANDLER,alpsEZEV,lex.vd,0);
      break;
      
      /* single string arg */
      /*      case EZ_DND_BUBBLE_STRING: */
    case EZ_SELECTION_BACKGROUND:
    case EZ_SELECTION_FOREGROUND:
    case EZ_INDICATOR_COLOR:
    case EZ_FOREGROUND:
    case EZ_TEXT_BACKGROUND:
    case EZ_BACKGROUND:
    case EZ_HIGHLIGHT_BG:
    case EZ_HIGHLIGHT_FG:
    case EZ_BITMAP_FILE:
    case EZ_IMAGE_FILE:
    case EZ_FONT_NAME:
    case EZ_BUTTON_SHORTCUT:
      /*      case EZ_BUBBLE_STRING: */
    case EZ_LABEL_STRING:
    case EZ_GLOB_PATTERN:
    case EZ_ENTRY_STRING:
    case EZ_NAME:
    case EZ_CLASS:
    case EZ_LCD_BACKGROUND:
    case EZ_LCD_FOREGROUND:
    case EZ_LED_BACKGROUND:
    case EZ_SMETER_BACKGROUND:
    case EZ_SMETER_FOREGROUND:
    case EZ_POPUP_NAME:
    case EZ_WM_WINDOW_NAME:
    case EZ_WM_ICON_NAME:
    case EZ_WM_ICON_XPM_FILE:
    case EZ_BG_IMAGE_FILE:
    case EZ_BG_IMAGE_FILE_B:
    case EZ_TERM_CURSOR_COLOR:
    case EZ_DIAL_COLOR:
    case EZ_CURSOR_NAME:
    case EZ_DND_DRAG_CURSOR_NAME:
    case EZ_SHAPE_FILE:
    case EZ_RESOURCE_STRING:
      ptr = car(args);
      if (!isaChr(ptr)) error(inv_funarg,"mkwdgt: resource needs string");
      if (nels(ptr) > MAX_STR_RSRC_LEN)
	error (inv_funarg,"mkwdgt: string resource too long");
      memcpy(cr,vadd(ptr).sp,nels(ptr));
      cr[nels(ptr)] = 0; /* null termination of string */
      EZ_ConfigureWidget(w,resource,cr,0);
      break;
      /* single int */
    case EZ_X:
    case EZ_Y:
    case EZ_SHAPED_WINDOW:
    case EZ_WIDTH:
    case EZ_WIDTH_HINT:
    case EZ_HEIGHT:
    case EZ_HEIGHT_HINT:
    case EZ_INDICATOR_TYPE:
    case EZ_EMBEDER:
    case EZ_BORDER_WIDTH:
    case EZ_BORDER_TYPE:
    case EZ_PADX:
    case EZ_PADY:
    case EZ_IPADX:
    case EZ_IPADY:
    case EZ_STACKING:
    case EZ_SIDE:
    case EZ_LABEL_POSITION:
    case EZ_JUSTIFICATION:
    case EZ_FILL_MODE:
    case EZ_TEXT_SPACING:
    case EZ_FONT_ID:
    case EZ_SLIDER_DISPLAY_VALUE:
    case EZ_SLIDER_LENGTH:
    case EZ_SLIDER_WIDTH:
    case EZ_SLIDER_BORDER_WIDTH:
    case EZ_TEXT_LINE_LENGTH:
    case EZ_INDICATOR_SIZE_ADJUST:
    case EZ_MOVABLE:
    case EZ_RESERVE_MENU_BUTTON:
    case EZ_BACKING_STORE:
    case EZ_EXPAND:
    case EZ_TRANSIENT:
    case EZ_SCROLLBAR_WIDTH:
    case EZ_SCROLLBAR_BORDER_WIDTH:
    case EZ_PROPAGATE:
    case EZ_ATTACH_INT_DATA:
    case EZ_UNDERLINE:
    case EZ_CHECK_BUTTON_ON_VALUE:
    case EZ_CHECK_BUTTON_OFF_VALUE:
    case EZ_CHECK_BUTTON_ON_OFF:
    case EZ_RADIO_BUTTON_GROUP:
    case EZ_RADIO_BUTTON_VALUE:
    case EZ_OPTIONAL_HSCROLLBAR:
    case EZ_OPTIONAL_VSCROLLBAR:
    case EZ_FANCY_LIST_BOX_TYPE:
    case EZ_FANCY_LIST_BOX_COLUMNS:
    case EZ_TEXT_WIDGET_EDITABLE:
    case EZ_RETURN_VALUE:
    case EZ_OPTIONAL_ENTRY_EDITABLE:
    case EZ_MENU_TEAR_OFF:
    case EZ_OPTIONAL_ENTRY_REMEMBER_HISTORY:
    case EZ_PADB:
    case EZ_LCD_NDIGITS:
    case EZ_LCD_FONT_SIZE:
    case EZ_LED_WIDTH:
    case EZ_LED_HEIGHT:
    case EZ_WMHINTS:
    case EZ_GEOMETRY_MANAGER:
    case EZ_LABELED_ICON:
    case EZ_HSCROLL_INCREMENT:
    case EZ_VSCROLL_INCREMENT:
    case EZ_SMETER_STOPED:
    case EZ_SMETER_SHOW_VALUE:
    case EZ_SMETER_ORIENTATION:
    case EZ_FORGET_X:
    case EZ_FORGET_Y:
    case EZ_FORGET_W:
    case EZ_FORGET_H:
    case EZ_FORGET_POSITION:
    case EZ_FORGET_SIZE:
    case EZ_FORGET_GEOMETRY:
    case EZ_WM_INITIAL_STATE:
    case EZ_MARK_CLEAN:
    case EZ_HIGHLIGHT_MODE:
    case EZ_RUBBER_BAND:
    case EZ_DRAG_HANDLE:
    case EZ_DOCKABLE:
    case EZ_SLIDER_STYLE:
    case EZ_ARROW_TYPE:
    case EZ_OPTIONAL_ENTRY_STYLE:
    case EZ_WM_IGNORE:
    case EZ_TERM_SAVE_LINES:
    case EZ_TERM_REVERSE_VIDEO:
    case EZ_RULER_DISPLAY_VALUE:
    case EZ_RULER_TICK_UNIT:
    case EZ_RULER_TICK_SCALE:
    case EZ_RULER_TICK_OFFSET:
    case EZ_RULER_TICK_LENGTH:
      /*      case EZ_RULER_ORIENTATION: */
    case EZ_OVERRIDE_REDIRECT:
    case EZ_SCROLL_TYPE:
    case EZ_DIAL_DISPLAY_VALUE:
    case EZ_DIAL_STYLE:
    case EZ_WRAP_TEXT:
    case EZ_BAR_ORIENTATION:
    case EZ_BAR_EDITABLE:
    case EZ_BAR_WIDTH:
    case EZ_HISTOGRAM_SAMPLE:
    case EZ_SEPARATOR_STYLE:
    case EZ_ROW_COLUMN_MAX_SIZE:
      ptr = car(args);
      if (!isNum(ptr)) error(inv_funarg,"mkwdgt: resource needs num val");
      rv = getInt(ptr);
      EZ_ConfigureWidget(w,resource,rv,0);
      break;
      /* (float) */
    case EZ_SLIDER_RESOLUTION:
    case EZ_SLIDER_INIT_VALUE:
    case EZ_SMETER_VALUE:
    case EZ_DIAL_VALUE:
    case EZ_DIAL_SPAN:
    case EZ_DIAL_RESOLUTION:
      ptr = car(args);
      if (!isNum(ptr)) error(inv_funarg,"mkwdgt: resource needs num val");
      frv1 = getNum(ptr);
      EZ_ConfigureWidget(w,resource,frv1,0);
      break;
      /* [int int] */
    case EZ_GRID_CELL_PLACEMENT:
    case EZ_LOCATION:
    case EZ_SIZE:
    case EZ_SIZE_HINT:
    case EZ_SMETER_STYLE:
    case EZ_WM_POSITION_HINT:
    case EZ_WM_SIZE_HINT:
    case EZ_WM_MAX_SIZE_HINT:
    case EZ_WM_MIN_SIZE_HINT:
    case EZ_INTERIOR_BORDER:
    case EZ_LABEL_SHADOW:
    case EZ_SPECIAL_EFFECT:
    case EZ_SHOW_TICK:
    case EZ_LED_PIXEL_SIZE:
    case EZ_SHEET_HEADER_SIZE:
    case EZ_SHEET_CELL_SIZE:
      if (listLen(args) != 1)
	error(inv_funarg,"mkwdgt: resource needs int[2]");
      ptr = car(args);
      if (!isNum(ptr) || nels(ptr) < 2)
	error(inv_funarg,"mkwdgt: resource needs int[2]");
      rv1 = getInt(ptr); rv2 = (int)vadd(ptr).np[1];
      EZ_ConfigureWidget(w,resource,rv1,rv2,0); 
      if (resource == EZ_WM_POSITION_HINT) {
	if (rv1 < 0) rv1 += ez_dw;
	if (rv2 < 0) rv2 += ez_dh;
	EZ_MoveWidgetWindow(w,rv1,rv2);
      }
      break;
      /* (int float) */
    case EZ_SMETER_VALUE_N:
      if (listLen(args) != 2)
	error(inv_funarg,"mkwdgt: resource needs int and float");
      ptr = car(args);
      if (!isNum(ptr)) error(inv_funarg,"mkwdgt: resource needs num");
      rv = getInt(ptr);
      if ((rv < 0) || (rv > 4))
	error (inv_funarg,"mkwdgt: resource need 1<=int<=5");
      ptr = cadr(args);
      if (!isNum(ptr)) error(inv_funarg,"mkwdgt: resource needs num");
      frv1 = getNum(ptr);
      EZ_ConfigureWidget(w,resource,rv,frv1,0);
      break;
      /* (int string) */
    case EZ_SMETER_FOREGROUND_N:
    case EZ_TERM_COLOR_N:
    case EZ_BAR_COLOR_N:
    case EZ_SPIN_VALUE:
      if (listLen(args) != 2)
	error(inv_funarg,"mkwdgt: resource needs int and float");
      ptr = car(args);
      if (!isNum(ptr)) error(inv_funarg,"mkwdgt: resource needs num");
      rv = getInt(ptr);
      if (resource != EZ_SPIN_VALUE && ((rv < 1) || (rv > 5)))
	error (inv_funarg,"mkwdgt: resource need 1<=int<=5");
      ptr = cadr(args);
      if (!isaChr(ptr)) error(inv_funarg,"mkwdgt: resource needs string");
      if (nels(ptr) > MAX_STR_RSRC_LEN)
	error (inv_funarg,"mkwdgt: string resource too long");
      memcpy(cr,vadd(ptr).sp,nels(ptr));
      cr[nels(ptr)] = 0; /* null termination of string */
      frv1 = getNum(ptr);
      EZ_ConfigureWidget(w,resource,rv,cr,0);
      break;
      /* [float float] */
    case EZ_SLIDER_RANGE:
    case EZ_SMETER_RANGE:
    case EZ_LOCATOR_XY:
    case EZ_DIAL_RANGE:
    case EZ_BAR_RANGE:
      if (listLen(args) != 1)
	error(inv_funarg,"mkwdgt: resource needs float[2]");
      ptr = car(args);
      if (!isNum(ptr) || nels(ptr) < 2)
	error(inv_funarg,"mkwdgt: resource needs float[2]");
      frv1 = vadd(ptr).np[0];  	frv2 = vadd(ptr).np[1];
      EZ_ConfigureWidget(w,resource,frv1,frv2,0);
      break;
      /* [int float float] */
    case EZ_SMETER_RANGE_N:
      error(not_imp,"EZ_METER_RANGE");
      break;
    case EZ_GRID_CELL_GEOMETRY:
    case EZ_GRID_ROW_CONSTRAINS:
    case EZ_GRID_COLUMN_CONSTRAINS:
      if ((listLen(args) != 1) || !isNum(ptr=car(args)) || nels(ptr)<4)
	error(inv_funarg,"mkwdgt: resource needs float[4]");
      rv1 = vadd(ptr).np[0];  	rv2 = vadd(ptr).np[1];
      rv3 = vadd(ptr).np[2];  	rv4 = vadd(ptr).np[3];
      EZ_ConfigureWidget(w,resource,rv1,rv2,rv3,rv4,0);
      break; 
    default: error(inv_funarg,"invalid resource");
    }
    conf = cdr(conf);
  }
}

pointer gmkwdgt(int n,pointer p) {

  EZ_Widget *w,*parent;
  pointer wt,wp,conf,res;
  int wtype;
  if (ez_fd < 0) return(pnil);
  wt    = arg1(p);
  wp    = arg2(p);
  conf  = arg3(p);
  if (!isNil(wp) && !isWidget(wp)) error(inv_funarg,"parent not nil or widget");
  wtype  = getInt(wt);
  parent = getWidget(wp);
  w = EZ_CreateWidget(wtype,parent,NULL);
  if (w == 0) return(pnil);
  /* This also ensures that w is eventually destroyed in case of error */
  // to protect callbacks: wp = cons(wp,conf); stick it all on a long list 
  res = allocWidget(wp,w);
  alpsCfgWidget(w,conf);
  return(res);
}

pointer gcfgwdgt(int n,pointer p) {
  EZ_Widget *w;
  pointer wt,wp,conf,res;
  if (ez_fd < 0) return(pnil);
  wp   = arg1(p);
  conf = arg2(p);
  w = getWidget(wp);
  if (w == 0) return(pnil);
  alpsCfgWidget(w,conf);
  return(ptru);
}

pointer gdispwdgt(integer n, pointer p) {
  EZ_Widget *w;
  if (ez_fd < 0) return(pnil);
  w = getWidget(arg1(p));
  EZ_DisplayWidget(w);
  return(ptru);
}

pointer ghidewdgt(integer n, pointer p) {
  EZ_Widget *w;
  if (ez_fd < 0) return(pnil);
  w = getWidget(arg1(p));
  EZ_HideWidget(w);
  return(ptru);
}

pointer gsldrval(integer n, pointer p) {
  EZ_Widget *w;
  float f;
  if (ez_fd < 0) return(pnil);
  w = getWidget(arg1(p));
  f = EZ_GetSliderValue(w);
  return(mkNum(f)); // protected by sendres
}

pointer gdialval(integer n, pointer p) {
  if (ez_fd < 0) return(pnil);
  EZ_Widget * w = getWidget(arg1(p));
  float       f = EZ_GetDialValue(w);
  return(mkNum(f)); // protected by sendres
}

pointer gfsval(integer n, pointer p) {
  if (ez_fd < 0) return(pnil);
  EZ_Widget *w = getWidget(arg1(p));
  char      *s = EZ_GetFileSelectorSelection(w);
  return(buildChr(s,strlen(s))); // protected by sendres
}

pointer gtxtval(integer n, pointer p) {
  if (ez_fd < 0) return(pnil);
  EZ_Widget *w = getWidget(arg1(p));
  char      *s = EZ_TextGetBufferString(w);
  return(buildChr(s,strlen(s))); // protected by sendres
}

pointer gtxtclr(integer n, pointer p) {
  if (ez_fd < 0) return(pnil);
  EZ_Widget *w = getWidget(arg1(p));
  //  EZ_TextBeginningOfBuffer(w);
  EZ_TextClear(w);
  //EZ_TextReDisplay(w);
  return(ptru);
}

pointer gtxtset(integer n, pointer p) {
  if (ez_fd < 0) return(pnil);
  EZ_Widget *w = getWidget(arg1(p));
  pointer lex  = arg2(p);
  int l        = nels(lex);
  char *s      = (char *)vadd(lex).sp;
  if (s[l]) { 
    alpsOutsn(alpserr,3,"warn non zero char at ",itos(l),"\n"); 
    alpsFlush(alpserr);
  }
  EZ_TextInsertString(w,s);
  return(lex);
}

pointer gcanvas(integer n, pointer p) {
  if (ez_fd < 0) return(pnil);
  EZ_Widget *w = getWidget(arg1(p));
  EZ_Set3DCanvas(w);
  return(ptru);
}
pointer gupdate(integer n, pointer p) {
  EZ_Widget *w;
  if (n > 1) error(inv_numarg,"(gupdate [canvas])");
  if (n == 1)  {
    EZ_Widget *w = getWidget(arg1(p));
    EZ_Set3DCanvas(w);
  }
  EZ_SwapBuffers();
  return(ptru);
}

pointer gload(integer n, pointer parms) {
  pointer lex;
  EZ_Bitmap  *pixmap,*npixmap;
  EZ_GLImage *glimage, *bkImage;
  Pixmap pix,shape;
  int ix,iy,iw,ih; // initial image offset & size
  int rw,rh;       // requested size (default = size of viewport)
  int xl,yl,xh,yh; // screen coordinates for whole viewport
  int vw,vh;       // viewport size
  int fw,fh;       // final size
  int darkness, smooth, depth, scale=0;
  idstr tline;
  float p[4],zl;
  number *imsp,sclh,sclw,scl;
  if (ez_fd < 0) return(pnil);
  if (n < 1 || n > 3) error(inv_numarg,"(gload <image-file-name> [<scale> [<imgspec>]])");
  if (!isChr(arg1(parms)) || ((n == 2) && (!isNum(arg2(parms)))))
    error(inv_funarg,"(gload <image-file-name> [<scale> [<imgspec>]])");
  if (n >= 2) {
    scale = getWhole(arg2(parms));
    if ((scale < 0) || (scale > 2)) error(inv_funarg,"scale = 0 | 1 | 2");
  }
  if ((n == 3) && ((nels(arg3(parms)) != 2) && (nels(arg3(parms)) != 4)))
    error(inv_funarg,"size of imgspec must be 2 or 4");
  lex=arg1(parms);
  pixmap = EZ_CreateLabelPixmapFromImageFile((char *)mkString(lex,tline,"gload"));
  if (!pixmap) error(inv_funarg,"Invalid image file");
  depth = EZ_GetLabelPixmapInfo(pixmap, &pix, &shape, &iw, &ih);
  fw = iw; fh = ih;
  /* Get size of viewport in pixels */
  p[0] = -1.0;
  p[1] = -1.0;
  p[2] = 0.0;
  p[3] = 1.0;
  EZ_World2Screen(p,&xl,&yl,&zl);
  p[0] = 1.0;
  p[1] = 1.0;
  EZ_World2Screen(p,&xh,&yh,&zl);
  vh = abs(yl-yh);
  vw = abs(xl-xh);
  if (n==3) { // select subimage
    imsp = vadd(arg3(parms)).np;
    /* initial x and y in image */
    ix = imsp[0];  
    iy = imsp[1];
    if (ix < 0) ix = 0;
    if (iy < 0) iy = 0;
    fw -= ix;
    fh -= iy;
    /* width and height within image */
    if (nels(arg3(parms)) == 4) {
      rw = imsp[2];
      rh = imsp[3];
      if ((rw > 0) && (rw < fw)) fw = rw; // crop to req size
      if ((rh > 0) && (rh < fh)) fh = rh;
    }
  } else {
    ix = 0;
    iy = 0;
  }
  if ((fw <= 0) || (fh <= 0)) return (lex);
  if (scale) {
    if (scale == 2) { /* keep aspect ratio */
      sclh = (number)vh/(number)fh;
      sclw = (number)vw/(number)fw;
      scl =  (sclh < sclw) ? sclh : sclw;
      vw = fw * scl;
      vh = fh * scl;
    } 
    smooth = (((vw / fw) >= 2) || ((vh / fh) >= 2));
    darkness = 100;
    npixmap = EZ_ScaleLabelPixmap2(pixmap, ix, iy, fw, fh, vw, vh, darkness, smooth);
    EZ_FreeLabelPixmap(pixmap);
    pixmap = npixmap;
    fw = vw; fh = vh; // scaled size 
    ix = iy = 0;      // ofsett already taken into account
  } else {
    if (fw > vw) fw = vw; // clip to viewport size
    if (fh > vh) fh = vh;
  }
  glimage = EZ_CreateGLImage(pixmap);
  EZ_FreeLabelPixmap(pixmap);
  EZ_PutGLImageScreen(glimage, ix, iy, fw, fh, -1.0, 1.0, 0.0); // upper left
  EZ_FreeGLImage(glimage);
  return(lex);
}

pointer gsave(integer n, pointer p) {
  idstr tline;
  if (ez_fd < 0) return(pnil);
  EZ_Widget *w = getWidget(arg1(p));
  pointer lex  = arg2(p);
  EZ_Save3DCanvas2PPMImage(w,(char *)mkString(lex,tline,"gsave"));
  return(lex);
}

pointer gwar(integer n, pointer p) { /* get widget aspect ration */
  pointer lex;
  int h,w;
  if (ez_fd < 0) return(pnil);
  if (n != 1) error(inv_numarg,"gwar expects only a widget as argument");
  EZ_Widget *wdgt = getWidget(arg1(p));
  EZ_GetWidgetDimension(wdgt, &w, &h);
  lex = mkNum((number)w/(number) h);
  return(lex);
}

pointer wdgtdata(integer n, pointer p) {
  if (ez_fd < 0) return(pnil);
  EZ_Widget *w = getWidget(arg1(p));
  return(mkNum((double)EZ_GetWidgetIntData(w)));
}

static void markWidgetTreeWork(EZ_Widget *w, int flag) {
  pointer lex;
  if(w) {
    EZ_Widget *children = EZ_GetChildrenWidget(w);
    lex.vd = EZ_GetWidgetPtrData(w);
    if (widMarker(lex)) return; // already marked
    markWidgetTreeWork(children, 1);
    if(flag) {
      EZ_Widget *sibling = EZ_GetSiblingWidget(w);
      markWidgetTreeWork(sibling, 1);
    }
  }
}

void markWidgetTree(EZ_Widget *w) {
  if(w) {
    markWidgetTreeWork(w, 0); 
  }
}


#endif /* GRAF */
