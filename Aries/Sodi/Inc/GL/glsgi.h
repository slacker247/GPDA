#define MVIEWING        GL_MODELVIEW
#define MPROJECTION     GL_PROJECTION

#define GR_LEFTMOUSE    1
#define GR_MIDDLEMOUSE  2
#define GR_RIGHTMOUSE   3

#define GD_TEXTURE      GL_TEXTURE_2D
#define GD_BLEND        GL_BLEND
#define GD_ZMAX         GL_DEPTH_CLEAR_VALUE

#define TX_TEXTURE_0    GL_TEXTURE_2D
#define TX_MINFILTER    GL_TEXTURE_MIN_FILTER
#define TX_POINT        GL_NEAREST
#define TX_MAGFILTER    GL_TEXTURE_MAG_FILTER
#define TX_BILINEAR     GL_LINEAR
#define TX_WRAP         14.0
#define TX_CLAMP        GL_CLAMP
#define TX_REPEAT       GL_REPEAT
#define TX_WRAP_S       GL_TEXTURE_WRAP_S
#define TX_WRAP_T       GL_TEXTURE_WRAP_T
#define TX_TILE         19.0
#define TX_NULL         0.0

#define TV_ENV0         GL_TEXTURE_ENV
#define TV_MODULATE     GL_MODULATE
#define TV_BLEND        GL_BLEND
#define TV_DECAL        GL_DECAL
#define TV_COLOR        GL_REPLACE
#define TV_NULL         0.0

#define BF_ZERO         GL_ZERO
#define BF_ONE          GL_ONE
#define BF_DC           GL_DST_COLOR
#define BF_MDC          GL_ONE_MINUS_DST_COLOR
#define BF_SA           GL_SRC_ALPHA
#define BF_MSA          GL_ONE_MINUS_SRC_ALPHA
#define BF_DA           GL_DST_ALPHA
#define BF_MDA          GL_ONE_MINUS_DST_ALPHA
#define BF_SC           GL_SRC_COLOR
#define BF_MSC          GL_ONE_MINUS_SRC_COLOR

typedef struct {
    unsigned short imagic;
    unsigned short type;
    unsigned short dim;
    unsigned short xsize;
    unsigned short ysize;
    unsigned short zsize;
    unsigned int   min;
    unsigned int   max;
    unsigned int   wastebytes;
    char           name[80];
    unsigned int   colormap;
    int            file;
    unsigned short flags;
    short          dorev;
    short          x;
    short          y;
    short          z;
    short          cnt;
    unsigned char  *tmp;
    unsigned char  *tmpR;
    unsigned char  *tmpG;
    unsigned char  *tmpB;
    unsigned long  rleend;              // for RLE images
    unsigned int   *rowstart;           // for RLE images
    int            *rowsize;            // for RLE images
} IMAGE;

