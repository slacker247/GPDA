/*
 *   Device Independent Bitmap functions for OpenGL.
 *
 * Contents:
 *
 *   LoadDIBitmap()  - Load a DIB/BMP file from disk.
 *   SaveDIBitmap()  - Save a bitmap to a DIB/BMP file on disk.
 *   GetGLBitmap()   - Read the current OpenGL viewport into a 24-bit RGB bitmap.
 *   ConvertRGB()    - Convert a DIB/BMP image to 24-bit RGB pixels.
 *
 * Revision History:
 *
 */

/*
 * Include necessary headers.
 */
#include "string.h"

#include "winbmp.H"

/*
 * 'LoadDIBitmap()' - Load a DIB/BMP file from disk.
 *
 * Returns a pointer to the bitmap if successful, NULL otherwise...
 */

void *
LoadDIBitmap(char	*filename,	/* I - File to load */
             BITMAPINFO	**info)		/* O - Bitmap information */
{
  FILE			*fp;		/* Open file pointer */
  void			*bits;		/* Bitmap pixel bits */
  long			bitsize,	/* Size of bitmap */
			infosize;	/* Size of header information */
  BITMAPFILEHEADER	header;		/* File header */


 /*
  * Try opening the file; use "rb" mode to read this *binary* file.
  */

  if ((fp = fopen(filename, "rb")) == NULL)
    return (NULL);

/*
 * Read the file header and any following bitmap information...
 */

   if (fread(&header, sizeof(BITMAPFILEHEADER), 1, fp) < 1) {
      fclose(fp);                 // Couldn't read the file header - return NULL
      return (NULL);
   };

   if (strstr(header.bfType, "MB") == NULL) {   // Check for BM reversed
      fclose(fp);                 // Not a bitmap file - return NULL...
      return (NULL);
   };

   infosize = header.bfOffBits - sizeof(BITMAPFILEHEADER);
   if ((*info = (BITMAPINFO *)malloc(infosize)) == NULL) {
      fclose(fp);                 // Couldn't allocate memory for bitmap info - return NULL
      return (NULL);
   };

   if (fread(*info, 1, infosize, fp) < infosize) {
     free(*info);                 // Couldn't read the bitmap header - return NULL
      fclose(fp);
      return (NULL);
   };

/*
 * Now that we have all the header info read in, allocate memory for the
 * bitmap and read *it* in...
 */

   if ((bitsize = (*info)->bmiHeader.biSizeImage) == 0)
      bitsize = ((*info)->bmiHeader.biWidth *
                (*info)->bmiHeader.biBitCount + 7) / 8 *
  	        abs((*info)->bmiHeader.biHeight);

   if ((bits = malloc(bitsize)) == NULL) {
      free(*info);                // Couldn't allocate memory - return NULL
      fclose(fp);
      return (NULL);
   };

   if (fread(bits, 1, bitsize, fp) < bitsize) {
      free(*info);                // Couldn't read bitmap - free memory and return NULL
      free(bits);
      fclose(fp);
      return (NULL);
   };

/*
 * OK, everything went fine - return the allocated bitmap...
 */

   fclose(fp);
   return (bits);
}

void LSBFirstWriteLong(unsigned char *buffer, int index, long value)
{
  buffer[index+0] = value;
  buffer[index+1] = value >> 8;
  buffer[index+2] = value >> 16;
  buffer[index+3] = value >> 24;
}
void LSBFirstWriteShort(unsigned char *buffer, int index, short value)
{
  buffer[index+0] = value;
  buffer[index+1] = value >> 8;
}

/*
 * 'SaveDIBitmap()' - Save a bitmap to a DIB/BMP file on disk.
 *
 * Returns 0 if successful, non-zero otherwise...
 */
int
SaveDIBitmap(char       *filename,	/* I - File to save to */
	     BITMAPINFO *info,		/* I - Bitmap information */
             void       *bits)		/* I - Bitmap pixel bits */
{
  FILE			*fp;		/* Open file pointer */
  long			size,		/* Size of file */
			infosize,	/* Size of bitmap info */
			bitsize;	/* Size of bitmap pixels */
  BITMAPFILEHEADER	header;		/* File header */
  int                   hdrsize = 14;
  unsigned char         *image;         /* Header area for write */
  int                   iptr;

/*
 * Try opening the file; use "wb" mode to write this *binary* file.
 */
   if ((fp = fopen(filename, "wb")) == NULL)
      return (-1);

   if (info->bmiHeader.biSizeImage == 0)	/* Figure out the bitmap size */
      bitsize = (info->bmiHeader.biWidth *
                 info->bmiHeader.biBitCount + 7) / 8 *
	         abs(info->bmiHeader.biHeight);
   else
      bitsize = info->bmiHeader.biSizeImage;

   infosize = sizeof(BITMAPINFOHEADER);
   switch (info->bmiHeader.biCompression) {
     case BI_BITFIELDS :
        infosize += 12;			/* Add 3 RGB doubleword masks */
        if (info->bmiHeader.biClrUsed == 0)
	  break;
     case BI_RGB :
        if (info->bmiHeader.biBitCount > 8 &&
            info->bmiHeader.biClrUsed == 0)
	  break;
     case BI_RLE8 :
     case BI_RLE4 :
        if (info->bmiHeader.biClrUsed == 0)
          infosize += (1 << info->bmiHeader.biBitCount) * 4;
	else
          infosize += info->bmiHeader.biClrUsed * 4;
     break;
   };
/*
 * Build the header for output in a portable, architecture-independent
 * way.
 */
   size = hdrsize + infosize + bitsize;

   image = (unsigned char *)malloc(hdrsize + infosize);

   strncpy(header.bfType, "BM", 2);	  // This is an MSWindows BMP file
   header.bfSize      = size;             // File size
   header.bfReserved1 = 0;
   header.bfReserved2 = 0;
   header.bfOffBits   = hdrsize + infosize;

   iptr = 0;
   strncpy((char *)image, header.bfType, 2);                         iptr = iptr + 2;
   LSBFirstWriteLong(image,  iptr, header.bfSize);                   iptr = iptr + 4;
   LSBFirstWriteShort(image, iptr, header.bfReserved1);              iptr = iptr + 2;
   LSBFirstWriteShort(image, iptr, header.bfReserved2);              iptr = iptr + 2;
   LSBFirstWriteLong(image,  iptr, header.bfOffBits);                iptr = iptr + 4;
   LSBFirstWriteLong(image,  iptr, info->bmiHeader.biSize);          iptr = iptr + 4;
   LSBFirstWriteLong(image,  iptr, info->bmiHeader.biWidth);         iptr = iptr + 4;
   LSBFirstWriteLong(image,  iptr, info->bmiHeader.biHeight);        iptr = iptr + 4;
   LSBFirstWriteShort(image, iptr, info->bmiHeader.biPlanes);        iptr = iptr + 2;
   LSBFirstWriteShort(image, iptr, info->bmiHeader.biBitCount);      iptr = iptr + 2;
   LSBFirstWriteLong(image,  iptr, info->bmiHeader.biCompression);   iptr = iptr + 4;
   LSBFirstWriteLong(image,  iptr, info->bmiHeader.biSizeImage);     iptr = iptr + 4;
   LSBFirstWriteLong(image,  iptr, info->bmiHeader.biXPelsPerMeter); iptr = iptr + 4;
   LSBFirstWriteLong(image,  iptr, info->bmiHeader.biYPelsPerMeter); iptr = iptr + 4;
   LSBFirstWriteLong(image,  iptr, info->bmiHeader.biClrUsed);       iptr = iptr + 4;
   LSBFirstWriteLong(image,  iptr, info->bmiHeader.biClrImportant);
   /*
   printf(" %s\n", header.bfType);
   printf(" %d\n", header.bfSize);
   printf(" %d\n", header.bfReserved1);
   printf(" %d\n", header.bfReserved2);
   printf(" %d\n", header.bfOffBits);
   printf(" %d\n", info->bmiHeader.biSize);
   printf(" %d\n", info->bmiHeader.biWidth);
   printf(" %d\n", info->bmiHeader.biHeight);
   printf(" %d\n", info->bmiHeader.biPlanes);
   printf(" %d\n", info->bmiHeader.biBitCount);
   printf(" %d\n", info->bmiHeader.biCompression);
   printf(" %d\n", info->bmiHeader.biSizeImage);
   printf(" %d\n", info->bmiHeader.biXPelsPerMeter);
   printf(" %d\n", info->bmiHeader.biYPelsPerMeter);
   printf(" %d\n", info->bmiHeader.biClrUsed);
   printf(" %d\n", info->bmiHeader.biClrImportant);
   */
/*
 * Write the file header, bitmap information, and bitmap pixel data...
 */
   if (fwrite(image, 1, hdrsize+infosize, fp) < hdrsize+infosize) {
      fclose(fp);                 // Couldn't write the file/bitmap header - return
      free(image);
      return (-1);
   };
   /*
   if (fwrite(&header, 1, sizeof(BITMAPFILEHEADER), fp) < sizeof(BITMAPFILEHEADER)) {
      fclose(fp);                 // Couldn't write the file header - return
      free(image);
      return (-1);
   };

   if (fwrite(info, 1, infosize, fp) < infosize) {
      fclose(fp);                 // Couldn't write the bitmap header - return
      return (-1);
   };
   */
   if (fwrite(bits, 1, bitsize, fp) < bitsize) {
      fclose(fp);                 // Couldn't write the bitmap - return
      free(image);
      return (-1);
   };

/*
 * OK, everything went fine - return...
 */

   fclose(fp);
   free(image);
   return (0);
}


/*
 * 'GetGLBitmap()' - Read the current OpenGL viewport into a
 *                    24-bit RGB bitmap.
 *
 * Returns the bitmap pixels if successful and NULL otherwise.
 */

GLINFO *
GetGLBitmap()		/* O - Bitmap information */
{
  long		i, j,			/* Looping var */
  		bitsize,		/* Total size of bitmap */
		width;			/* Aligned width of a scanline */
  GLint		viewport[4];		/* Current viewport */
  void		*bits;			/* RGB bits */
  GLubyte	*rgb,			/* RGB looping var */
		temp;			/* Temporary var for swapping */
  BITMAPINFO    *info;
  GLINFO        *glinfo;

/*
 * Grab the current viewport...
 */
   glGetIntegerv(GL_VIEWPORT, viewport);
/*
 * Allocate memory for the header and bitmap...
 */
   if ((info = (BITMAPINFO *)malloc(sizeof(BITMAPINFOHEADER))) == NULL) {
      return (NULL);              // Couldn't allocate memory for bitmap info - return NULL
   };

   width   = viewport[2] * 3;	  // Real width of scanline
   width   = (width + 3) & ~3;	  // Aligned to 4 bytes
   bitsize = width * viewport[3]; // Size of bitmap, aligned */

   if ((bits = calloc(bitsize, 1)) == NULL) {
      free(info);                 // Couldn't allocate memory for bitmap pixels - return NULL
      return (NULL);
   };

   glinfo = (GLINFO *)malloc(sizeof(GLINFO));
   glinfo->head = info;
   glinfo->bits = bits;
/*
 * Read pixels from the framebuffer...
 */

   glFinish();				/* Finish all OpenGL commands */
   glPixelStorei(GL_PACK_ALIGNMENT, 4);	/* Force 4-byte alignment */
   glPixelStorei(GL_PACK_ROW_LENGTH, 0);
   glPixelStorei(GL_PACK_SKIP_ROWS, 0);
   glPixelStorei(GL_PACK_SKIP_PIXELS, 0);

   glReadPixels(0, 0, viewport[2], viewport[3], GL_RGB, GL_UNSIGNED_BYTE, bits);
/*
 * Swap red and blue for the bitmap...
 */
   for (i = 0; i < viewport[3]; i ++)
     for (j = 0, rgb = ((GLubyte *)bits) + i * width;
         j < viewport[2];
	 j ++, rgb += 3) {
        temp   = rgb[0];
        rgb[0] = rgb[2];
        rgb[2] = temp;
     };
/*
 * Finally, initialize the bitmap header information...
 */
   info->bmiHeader.biSize          = sizeof(BITMAPINFOHEADER);
   info->bmiHeader.biWidth         = viewport[2];
   info->bmiHeader.biHeight        = viewport[3];
   info->bmiHeader.biPlanes        = 1;
   info->bmiHeader.biBitCount      = 24;
   info->bmiHeader.biCompression   = BI_RGB;
   info->bmiHeader.biSizeImage     = bitsize;
   info->bmiHeader.biXPelsPerMeter = 2952; /* 75 DPI */
   info->bmiHeader.biYPelsPerMeter = 2952; /* 75 DPI */
   info->bmiHeader.biClrUsed       = 0;
   info->bmiHeader.biClrImportant  = 0;

   return (glinfo);
}
/*
 * 'ConvertRGB()' - Convert a DIB/BMP image to 24-bit RGB pixels.
 *
 * Returns an RGB pixel array if successful and NULL otherwise.
 */
GLubyte *
ConvertRGB(BITMAPINFO *info,		/* I - Original bitmap information */
           void       *bits)		/* I - Original bitmap pixels */
{
  int		i, j,			/* Looping vars */
  		bitsize,		/* Total size of bitmap */
		width;			/* Aligned width of bitmap */
  GLubyte	*newbits;		/* New RGB bits */
  GLubyte	*from, *to,		/* RGB looping vars */
		temp;			/* Temporary var for swapping */

 /*
  * Allocate memory for the RGB bitmap...
  */

  width   = 3 * info->bmiHeader.biWidth;
  width   = (width + 3) & ~3;	
  bitsize = width * info->bmiHeader.biHeight;
  if ((newbits = (GLubyte *)calloc(bitsize, 1)) == NULL)
    return (NULL);

 /*
  * Copy the original bitmap to the new array, converting as necessary.
  */

  switch (info->bmiHeader.biCompression)
  {
    case BI_RGB :
        if (info->bmiHeader.biBitCount == 24)
	{
         /*
          * Swap red & blue in a 24-bit image...
          */

          for (i = 0; i < info->bmiHeader.biHeight; i ++)
	    for (j = 0, from = ((GLubyte *)bits) + i * width,
	             to = newbits + i * width;
		 j < info->bmiHeader.biWidth;
		 j ++, from += 3, to += 3)
            {
              to[0] = from[2];
              to[1] = from[1];
              to[2] = from[0];
            };
	};
	break;
    case BI_RLE4 :
    case BI_RLE8 :
    case BI_BITFIELDS :
        break;
  };

  return (newbits);
}

