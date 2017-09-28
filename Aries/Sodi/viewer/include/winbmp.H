/*
 *
 *   Device Independent Bitmap definitions for OpenGL.
 *
 */

/*
 * Include necessary headers.
 */

#ifndef _BITMAP_H_
#define _BITMAP_H_

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

#include <GL/gl.h>

/* constants for the biCompression field */
#define BI_RGB        0L
#define BI_RLE8       1L
#define BI_RLE4       2L
#define BI_BITFIELDS  3L

typedef short         WORD;
typedef unsigned long DWORD;
typedef unsigned long LONG;
typedef unsigned char BYTE;
typedef char *        LPVOID;

/* Bitmap Header Definition */
typedef struct BITMAP {
        LONG        bmType;
        LONG        bmWidth;
        LONG        bmHeight;
        LONG        bmWidthBytes;
        WORD        bmPlanes;
        WORD        bmBitsPixel;
        LPVOID      bmBits;
};

typedef struct RGBTRIPLE {
        BYTE    rgbtBlue;
        BYTE    rgbtGreen;
        BYTE    rgbtRed;
};

typedef struct RGBQUAD {
        BYTE    rgbBlue;
        BYTE    rgbGreen;
        BYTE    rgbRed;
        BYTE    rgbReserved;
};

typedef struct BITMAPFILEHEADER {
        char        bfType[2];               // "BM"
        DWORD       bfSize;                  // Size of file in bytes
        WORD        bfReserved1;             // Reserved, always 0
        WORD        bfReserved2;             // Reserved, always 0
        DWORD       bfOffBits;               // Offset to image in bytes
};

typedef struct BITMAPINFOHEADER {
        DWORD       biSize;                  // Size of BITMAPINFOHEADER in bytes
        DWORD       biWidth;                 // Width of image in pixels
        DWORD       biHeight;                // Height of image in pixels
        WORD        biPlanes;                // # of color planes (always 1)
        WORD        biBitCount;              // # of color bits
        DWORD       biCompression;           // Type of compression used
        DWORD       biSizeImage;             // Size of image in bytes
        DWORD       biXPelsPerMeter;         // Horizontal pixels per meter
        DWORD       biYPelsPerMeter;         // Vertical pixels per meter
        DWORD       biClrUsed;               // Number of colors used
        DWORD       biClrImportant;          // Number of 'important' colors
};

typedef struct BITMAPINFO {
        BITMAPINFOHEADER    bmiHeader;
        RGBQUAD             bmiColors[1];
};

typedef struct GLINFO {
        void        *bits;
        BITMAPINFO  *head;
};

typedef struct BITMAPV4HEADER {
        DWORD        bV4Size;
        LONG         bV4Width;
        LONG         bV4Height;
        WORD         bV4Planes;
        WORD         bV4BitCount;
        DWORD        bV4V4Compression;
        DWORD        bV4SizeImage;
        LONG         bV4XPelsPerMeter;
        LONG         bV4YPelsPerMeter;
        DWORD        bV4ClrUsed;
        DWORD        bV4ClrImportant;
        DWORD        bV4RedMask;
        DWORD        bV4GreenMask;
        DWORD        bV4BlueMask;
        DWORD        bV4AlphaMask;
        DWORD        bV4CSType;
  //CIEXYZTRIPLE bV4Endpoints;
        DWORD        bV4GammaRed;
        DWORD        bV4GammaGreen;
        DWORD        bV4GammaBlue;
};

/* structures for defining DIBs */
typedef struct BITMAPCOREHEADER {
        DWORD   bcSize;                 /* used to get to color table */
        WORD    bcWidth;
        WORD    bcHeight;
        WORD    bcPlanes;
        WORD    bcBitCount;
};

typedef struct BITMAPCOREINFO {
    BITMAPCOREHEADER    bmciHeader;
    RGBTRIPLE           bmciColors[1];
};

extern void	*LoadDIBitmap(char *filename, BITMAPINFO **info);
extern int	SaveDIBitmap(char *filename, BITMAPINFO *info, void *bits);
extern GLINFO	*GetGLBitmap();
extern GLubyte	*ConvertRGB(BITMAPINFO *info, void *bits);

#endif /* !_BITMAP_H_ */
