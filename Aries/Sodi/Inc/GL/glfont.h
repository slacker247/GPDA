#ifndef GLFONTS_H
#define GLFONTS_H

#define GL_RASTER     1
#define GL_STROKE     2
#define GL_OUTLINE    4
#define GL_FILLED     8
#define GL_LCD        16
#define GL_NATIVE     32
#define GL_ALLFONTS   0xff

extern void  glfontMake(GLuint fontlist); // Make all OpenGL fonts
extern void  glfontSet(GLuint font);      // Set current font
extern void  glfontPrint(char *s);        // Print text string using current font
extern void  glfontBitmap(char *s);       // Print text using Bitmap font
extern void  glfontStroke(char *s);       // Print text using Stroke font
extern void  glfontFilled(char *s);       // Print text using Filled font
extern void  glfontOutine(char *s);       // Print text using Outline font
extern void  glfontLcd(char *s);          // Print text using LCD font

extern void  printString(char *s);

#endif
