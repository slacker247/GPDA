extern "C" void gropen_();
extern "C" void grclos_();
extern "C" void gflush_();
extern "C" void grbegin_(int *listid);
extern "C" void grend_();
extern "C" void set_(float *XLEFT, float *XRITE, float *YBOT, float *YTOP,
                     float *XMIN,  float *XMAX,  float *YMIN, float *YMAX, int *LT);
extern "C" void resets_(float *XMIN, float *XMAX, float *YMIN, float *YMAX);
extern "C" void getset_(float *XLEFT, float *XRITE, float *YBOT, float *YTOP,
                        float *XMIN,  float *XMAX,  float *YMIN, float *YMAX, int *LT);
extern "C" void mxmy_(float *X, float *Y);
extern "C" void perim_(int *I1, int *I2, int *I3, int *I4);
extern "C" void poly4_(float *x, float *y, int *color);
extern "C" void frst3d_(float *x, float *y, float *z);
extern "C" void frstpt_(float *x, float *y);
extern "C" void vect3d_(float *x, float *y, float *z);
extern "C" void vector_(float *x, float *y);
extern "C" void last3d_();
extern "C" void lastpt_();
extern "C" void pnt3d_(float *x, float *y, float *z);
extern "C" void point_(float *x, float *y);
extern "C" void curv3d_(float *x, float *y, float *z, int *n);
extern "C" void rectangle_(float *x, float *y, float *width, float *height);
extern "C" void circle_(float *x, float *y, float *radius, int *fill);
extern "C" void arc_(float *x, float *y, float *radius, float *sa, float *ea, int *fill);
extern "C" void dashln_(int *style);
extern "C" void wideln_(int *width);
extern "C" void clrvue_();
extern "C" void color_(int *kolor);
extern "C" void scolor_(int *Kolor, int *Mode, int *Index);
extern "C" int  ncolor_();
extern "C" void fwrit_(float *IX, float *IY, char *CHMSG, int *NCHAR, int *I, int *J, int *K);
extern "C" void marker_(int *IMark, char *Ch, int *JColor, int *NSize, float *XArray, float *YArray);

extern "C" void makeRasterFont(void);
extern "C" void printString(GLuint base, char *s);
