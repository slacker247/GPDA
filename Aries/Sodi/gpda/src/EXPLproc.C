
#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <math.h> 
#include <time.h>
#include <signal.h>
#include <sys/ioctl.h>
#include <errno.h>

#include "Globals.h"
#include "forms.h"
#include "EXPLforms.h"

/* --------------------------------------------------------------------- */

#ifndef FALSE
#define FALSE   0
#endif
#ifndef TRUE
#define TRUE    1
#endif

#define PUSHED  1

/* --------------------------------------------------------------------- */

int             explInit  = 0;
int             explFrame = 0;
int             explCount = 6;
int             explFiles = 1;
int             explDelay = 1000;
int             explStop  = 0;
int             explAudio = 0;
char            explFormat[64];
char            explWavFile[128];
char            expltemp[1024];
char            explLabel[32];

FD_explain      *fd_explain;

/* --------------------------------------------------------------------- */

void EXPLinit();
void EXPLshow(int xpos, int ypos, int width, int height, Window winid,
              int mode, char *fn);

void explerrorCB(FL_IMAGE *im, const char *s);
int  explstatCB(FL_IMAGE *im, const char *s);
void FrameUpdate(int frameno, char* filename);

extern int      EraseActiveEntry(char *text);
extern int      StoreActiveEntry(char *text);
extern int      FinishUp();
/*                                                                       */
/* --------------------------------------------------------------------- */
/*              M A I N   P R O G R A M   S T A R T S   H E R E          */
/* --------------------------------------------------------------------- */
/*                                                                       */
void EXPLinit()
{
int             i, j, k;

   if (explInit) return;

   fd_explain = create_form_explain();

   strcpy(explLabel, "Explanation");

   //flimage_enable_xpm();
   //flimage_enable_gif();
   //flimage_enable_bmp();
   //flimage_enable_sgi();

   return;
}

void EXPLexitCB(FL_OBJECT *ob, long data)
{

   fl_hide_form(fd_explain->explain);
   EraseActiveEntry(explLabel);

   FinishUp();
 
   return;
}
 
void EXPLnoneCB(FL_OBJECT *ob, long data)
{
 
   return;
}

int EXPLclose(FL_FORM *form, void *data)
{
   EXPLexitCB(NULL, 0);

   return (0);
}

void EXPLshow(int xpos, int ypos, int width, int height, Window mainwinID,
	      int mode, char *fname)
{
FILE*           INITfp;
int             i, k, i1, item = 0;
int             etype;
char            filename[128];
Window          winid;
FLIMAGE_SETUP   mysetup;
//
//   Pop up the window
//
   if(!fl_form_is_visible(fd_explain->explain) ) {
      fl_transient();
      fl_winposition(xpos, ypos);
      fl_initial_winsize(width, height);
      winid = fl_prepare_form_window(fd_explain->explain,
                                     FL_PLACE_POSITION,FL_TRANSIENT, explLabel);
      fl_winreshape(winid, xpos, ypos, width, height);
      fl_show_form_window(fd_explain->explain);
      fl_set_form_atclose(fd_explain->explain, EXPLclose, 0);
      StoreActiveEntry(explLabel);
   }

   if ((INITfp = fopen(fname, "r")) == NULL) return;

   // Read text explanation filename
   do fgets(expltemp, 128, INITfp); while (expltemp[0] == '#');
   sscanf(expltemp, "%s %d", filename, &etype);

   // Read audio explanation filename
   do fgets(expltemp, 128, INITfp); while (expltemp[0] == '#');
   sscanf(expltemp, "%s %d", explWavFile, &explAudio);

   if (etype == 0) {
     fl_load_browser(fd_explain->ds_explain, filename);

     // Read visual explanation filename count
     do fgets(expltemp, 128, INITfp); while (expltemp[0] == '#');
     sscanf(expltemp, "%d", &explFiles);
     do fgets(expltemp, 128, INITfp); while (expltemp[0] == '#'); // Read filename or template
     sscanf(expltemp, "%s", explFormat);
     //
     explCount = explFiles;
     if (explFiles < 0) {
       explCount = -explCount;
     }
     fclose(INITfp);
   } else {
     fclose(INITfp);
     return;
   }

   //mysetup.visual_cue = explstatCB;
   //mysetup.error_message = explerrorCB;
   //mysetup.app_data = fd_explain;
   //flimage_setup(&mysetup);

   fl_set_object_helper(fd_explain->dsex_rew,  "Rewind");
   fl_set_object_helper(fd_explain->dsex_rev,  "Reverse");
   fl_set_object_helper(fd_explain->dsex_back, "Backward");
   fl_set_object_helper(fd_explain->dsex_stop, "Stop");
   fl_set_object_helper(fd_explain->dsex_step, "Forward");
   fl_set_object_helper(fd_explain->dsex_play, "Play");
   fl_set_object_helper(fd_explain->dsex_end,  "End");

   explFrame = 0;

   strcpy(filename, explFormat);
   for (i=0; i<explCount; i++) {
     explFrame++;
     if (explFiles < 0) {
       //strcpy(explFormat, "../Weather/explPict-%03d.gif");
       sprintf(filename, explFormat, explFrame);
     }
     //fprintf(stderr, "Image %d (of %d) from %s\n", explFrame, explCount, filename);
     FrameUpdate(explFrame, filename);
     explDelay = (int)fl_get_dial_value(fd_explain->dsim_delay);
     fl_msleep(explDelay);
   }

   return;
}

void FrameUpdate(int frameno, char* filename)
{
int             i, i1;
char            *BITMAPDIR;
char            pixname[128];
char            xpmfiles[10][8] = { "0.xpm", "1.xpm", "2.xpm", "3.xpm", "4.xpm",
                                    "5.xpm", "6.xpm", "7.xpm", "8.xpm", "9.xpm" };
FL_IMAGE        *image;
FL_OBJECT       *ipipm;

   if ((BITMAPDIR = getenv("BITMAPDIR")) == NULL) BITMAPDIR = "../BitMaps";
 
   for (i=0; i<3; i++) {
     ipipm = fd_explain->ds_frameno[i];
     fl_show_object(ipipm);
     fl_free_pixmap_pixmap(ipipm);
     i1 = (int)((double)frameno/pow(10,i));
     sprintf (pixname, "%s/%s", BITMAPDIR, xpmfiles[i1]);
     fl_set_pixmap_file(ipipm, pixname);
   }

   for (i=0; i<6; i++) {
     ipipm = fd_explain->ds_frametime[i];
     fl_show_object(ipipm);
     fl_free_pixmap_pixmap(ipipm);
     sprintf (pixname, "%s/%s", BITMAPDIR, xpmfiles[0]);
     fl_set_pixmap_file(ipipm, pixname);
   }

   //fprintf(stderr, "Loading image from %s\n", filename); fflush(stderr);

   image = flimage_load(filename);
   flimage_sdisplay(image, FL_ObjWin(fd_explain->ds_image));

   fl_update_display(0);
}
/*                                                                       */
/* --------------------------------------------------------------------- */
/*      C A L L B A C K    R O U T I N E S    S T A R T    H E R E       */
/* --------------------------------------------------------------------- */
/*                                                                       */
void explerrorCB(FL_IMAGE *im, const char *s)
{
   return;
}

int explstatCB(FL_IMAGE *im, const char *s)
{
   fl_set_slider_value(fd_explain->dsim_percent,
		      ((double)im->completed/(double)im->total)*100.0);
   //fl_update_display(0);

   return 0;
}

void dsimdelayCB(FL_OBJECT *object, long item_no)
{
int             item = 0;
int             explDelay;

   explDelay = (int)fl_get_dial_value(fd_explain->dsim_delay);
   sprintf(expltemp, "Delay is\n%d ms", explDelay);
   fl_set_object_label(fd_explain->dsim_delay, expltemp);
}

void dscontrolCB(FL_OBJECT *object, long item_no)
{
int             i, item = 0;
char            filename[128];

   strcpy(filename, explFormat);

   switch (item_no)
   {
     case 0:
       explFrame = 1;
       if (explFiles < 0) {
         sprintf(filename, explFormat, explFrame);
       }
       FrameUpdate(explFrame, filename);
       explStop = TRUE;
       break;

     case 1:
       explFrame = explCount+1;
       for (i=0; i<explCount; i++) {
         explFrame--;
         if (explFiles < 0) {
           sprintf(filename, explFormat, explFrame);
         }
         FrameUpdate(explFrame, filename);
         fl_msleep(explDelay);
       }
       break;

     case 2:
       if (explFrame > 1) {
         explFrame--;
         if (explFiles < 0) {
           sprintf(filename, explFormat, explFrame);
         }
         FrameUpdate(explFrame, filename);
       } else explStop = TRUE;
       break;

     case 3:
       explStop = TRUE;
       break;

     case 4:
       if (explFrame < explCount) {
         explFrame++;
         if (explFiles < 0) {
           sprintf(filename, explFormat, explFrame);
         }
         FrameUpdate(explFrame, filename);
       } else explStop = TRUE;
       break;

     case 5:
       explFrame = 0;
       for (i=0; i<explCount; i++) {
         explFrame++;
         if (explFiles < 0) {
           sprintf(filename, explFormat, explFrame);
         }
         FrameUpdate(explFrame, filename);
         fl_msleep(explDelay);
       }
       break;

     case 6:
       explFrame = explCount;
       if (explFiles < 0) {
         sprintf(filename, explFormat, explFrame);
       }
       FrameUpdate(explFrame, filename);
       explStop = TRUE;
       break;

     default:
       break;
   }
}

void dsimscrollCB(FL_OBJECT *object, long item_no)
{
int             item = 0;

   switch (item_no)
   {
     case 0:
       break;

     case 1:
       break;

     default:
       break;
   }
}

void explaudioCB(FL_OBJECT *object, long item_no)
{
   if (explAudio) {
     printf("Playing audio explanation from %s\n", explWavFile);
     sprintf(expltemp, "wavplay %s", explWavFile);
     system(expltemp);
   } else {
     printf("Sorry, no audio explanation available!\n");
   }

   return;
}
