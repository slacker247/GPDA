#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <math.h>
#include <time.h>
#include <signal.h>
/*
 *   Include the network/socket stuff
 */
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <string.h>
#include <unistd.h>
#include <stream.h>
#define SIGCHILD SIGCLD

#include "DataParser.H"

#include "forms.h"
#include "JTAMCforms.h"

#define FALSE   0
#define TRUE    1

typedef int     Boolean;

FILE            *menufile;

Boolean         AUTORUN, AUTOEXIT;
Boolean         TEXTURED, GRIDLINE, BOUNDARY;
Boolean         ASSETS;
Boolean         DEBUGING, TIMING, STATISTICS, LOGGING;
Boolean         TRACKS, TRAILS, IMPACTS;
float           StepTime;
int             StartRegion;
int             PortID;
char            Class[24];
char            SrcInput[8];
char            TexFile[128];
char            HostID[64];

Boolean         TRACKLABEL, DROPTRACK, SHOWR2, KEEPLINKS;
int             SecDelay;
float           StartTime = 80000.0;
float           EndTime = 100000.0;
char            JtamvInputFile[128];
char            JtampInputFile[128];

float           JtampStartTime = 59000.0;
float           JtampEndTime   = 63000.0;
float           JtampFirstTime = 1000000.00001;
float           JtampCycleTime = 600.0;
int             CpuTime   = 0;
Boolean         READALL   = FALSE;
Boolean         INFILE    = FALSE;
Boolean         OTFILE    = FALSE;
Boolean         ROLLBACK  = FALSE;
char            OutputFile[128];

char            chtemp[64];
char            filenames[4][24];
int             UtilVisible = FALSE;

int             n_items;
int             c_item;
int             VMODE[10], LAT[10], LON[10], ALT[10], FOV[10], AZI[10];
char            ITEMNAME[10][24];

FL_OBJECT       *scipm;
FL_OBJECT       *bmdoipm, *jntfipm;

FD_ODI          *fd_ODI;
FD_config       *fd_config;
FD_regionedit   *fd_regionedit;

char *
strsub(char *istr, char och, char nch)
{
int    i;
 
   for (i=0; i<strlen(istr); i++) if (istr[i] == och) istr[i] = nch;
   return (istr);
}
/*                                                                       */
/* --------------------------------------------------------------------- */
/*              M A I N   P R O G R A M   S T A R T S   H E R E          */
/* --------------------------------------------------------------------- */
/*                                                                       */
int main(int argc, char *argv[])
{
int             i;
char            *BITMAPDIR;
char            xpmfile[128];

   fl_initialize(&argc, argv, 0, 0, 0);
   fd_ODI = create_form_ODI();
   fd_config = create_form_config();
   fd_regionedit = create_form_regionedit();

   fl_hide_object(fd_ODI->config_button);
   fl_hide_object(fd_ODI->scenario_button);
   fl_hide_object(fd_ODI->planner_button);
/* 
   if ((BITMAPDIR = getenv("BITMAPDIR")) == NULL)
     BITMAPDIR = "../BitMaps";
   sprintf (xpmfile, "%s%s", BITMAPDIR, "/NMDback.xpm");
   scipm = fd_ODI->background;
   fl_show_object(scipm);
   fl_free_pixmap_pixmap(scipm);
   fl_set_pixmap_file(scipm, xpmfile);
   sprintf (xpmfile, "%s%s", BITMAPDIR, "/BMDOlogo.xpm");
   bmdoipm = fd_ODI->BMDOxpm;
   fl_show_object(bmdoipm);
   fl_free_pixmap_pixmap(bmdoipm);
   fl_set_pixmap_file(bmdoipm, xpmfile);

   sprintf (xpmfile, "%s%s", BITMAPDIR, "/JNTFlogo.xpm");
   jntfipm = fd_ODI->JNTFxpm;
   fl_show_object(jntfipm);
   fl_free_pixmap_pixmap(jntfipm);
   fl_set_pixmap_file(jntfipm, xpmfile);
*/

   fl_show_form(fd_ODI->ODI, FL_PLACE_CENTER,FL_FULLBORDER, "ODI");

   fl_do_forms();
   return 0;
}

void
WriteFile(char *filename, int itemno)
{
FILE            *fp;

    fp = fopen(filename, "w+");
    fprintf(fp, "parameters {\n");

   switch (itemno) {
     case 0:
       break;

     case 1:
       fprintf(fp, "  string class %s\n",            Class);
       fprintf(fp, "  float step_time %f\n",         StepTime);
          // GVT update interval (for PLAYBACK mode)
       fprintf(fp, "  int region %d\n",              StartRegion);
          // Start with display of this region
       fprintf(fp, "  logical autorun %s\n",         (AUTORUN==FALSE ? "F" : "T"));
          // Start running without user action
       fprintf(fp, "  logical autoexit %s\n",        (AUTOEXIT==FALSE ? "F" : "T"));
          // Exit at end of run
       fprintf(fp, "  logical textured %s\n",        (TEXTURED==FALSE ? "F" : "T"));
          // Start with textured earth, else with Blue?
       fprintf(fp, "  logical gridline %s\n",        (GRIDLINE==FALSE ? "F" : "T"));
          // Start with gridlines every 10 degrees?
       fprintf(fp, "  logical boundary %s\n",        (BOUNDARY==FALSE ? "F" : "T"));
          // Start with political boundary lines?
       fprintf(fp, "  logical assets %s\n",          (ASSETS==FALSE ? "F" : "T"));
          // Start with assets displayed?
       fprintf(fp, "  logical debuging %s\n",        (DEBUGING==FALSE ? "F" : "T"));
          // Is this a debugging run?
       fprintf(fp, "  logical timing %s\n",          (TIMING==FALSE ? "F" : "T"));
          // Collect timing information?
       fprintf(fp, "  logical loging %s\n",          (LOGGING==FALSE ? "F" : "T"));
          // Generate log file?
       fprintf(fp, "  logical statistics %s\n",      (STATISTICS==FALSE ? "F" : "T"));
       fprintf(fp, "  logical tracks %s\n",          (TRACKS==FALSE ? "F" : "T"));
          // Draw model at (x,y,z) position?
       fprintf(fp, "  logical trails %s\n",          (TRAILS==FALSE ? "F" : "T"));
          // Connect the dots?
       fprintf(fp, "  logical impacts %s\n",         (IMPACTS==FALSE ? "F" : "T"));
          // Show the impact ellipses?
       fprintf(fp, "  string input_source %s\n",     SrcInput);
          // Can be 'file' or 'socket' or 'speedes'
       fprintf(fp, "  string tex_file %s\n",         TexFile);
          // File name of globe texture file
       fprintf(fp, "  string hostid %s\n",           HostID);
       fprintf(fp, "  int portid %d\n",              PortID);
       break;

     case 2:
       fprintf(fp, "  string input_file %s\n",       JtamvInputFile);
       fprintf(fp, "  logical label_track %s\n",     (TRACKLABEL==FALSE ? "F" : "T"));
       fprintf(fp, "  logical drop_track %s\n",      (DROPTRACK==FALSE ? "F" : "T"));
       fprintf(fp, "  logical show_r2 %s\n",         (SHOWR2==FALSE ? "F" : "T"));
       fprintf(fp, "  logical keep_links %s\n",      (KEEPLINKS==FALSE ? "F" : "T"));
       fprintf(fp, "  int delay_secs %d\n",          SecDelay);
       fprintf(fp, "  float starttime %f\n",         StartTime);
       fprintf(fp, "  float endtime %f\n",           EndTime);
       break;

     case 3:
       fprintf(fp, "  string name %s\n",             "GRAPHICS000");
       fprintf(fp, "  float start_time %f\n",        JtampStartTime);
       fprintf(fp, "  float first_time %f\n",        JtampFirstTime);
       fprintf(fp, "  float cycle_time %f\n",        JtampCycleTime);
       fprintf(fp, "  float tend %f\n",              JtampEndTime);
       fprintf(fp, "  int cpu_time %d\n",            CpuTime);

       fprintf(fp, "  logical readall %s\n",         (READALL==FALSE ? "F" : "T"));
       fprintf(fp, "  logical input_file %s\n",      (INFILE==FALSE ? "F" : "T"));
       fprintf(fp, "  logical output_file %s\n",     (OTFILE==FALSE ? "F" : "T"));
       fprintf(fp, "  logical test_rollback %s\n",   (ROLLBACK==FALSE ? "F" : "T"));
       fprintf(fp, "  string input_file_name %s\n",  JtampInputFile);
       fprintf(fp, "  string output_file_name %s\n", "TESTxx");
       break;

     default:
       break;
   }

   fprintf(fp, "}\n");
   fclose(fp);
}
/*                        */
/* callbacks for form ODI */
/*                        */
void utilitiesCB(FL_OBJECT *ob, long data)
{
   if (UtilVisible) {
      fl_hide_object(fd_ODI->config_button);
      fl_hide_object(fd_ODI->scenario_button);
      fl_hide_object(fd_ODI->planner_button);
   } else {
      fl_show_object(fd_ODI->config_button);
      fl_show_object(fd_ODI->scenario_button);
      fl_show_object(fd_ODI->planner_button);
   }

   UtilVisible = !UtilVisible;
}

void configCB(FL_OBJECT *ob, long data)
{
char            *TADILJFILE;
char            *INPUTSRC;
char            *CLASS;
char            *texfile;
char            *hostid;
static int      pars_loaded = FALSE;
int             i;

   if (!pars_loaded) {
      strcpy(filenames[0], "\0");
      strcpy(filenames[1], "jtamv.par\0");
      strcpy(filenames[2], "input.par\0");
      strcpy(filenames[3], "graphics.par\0");

      fl_clear_menu(fd_config->region_menu);
      if ((menufile = fopen("regions.dat", "r")) != NULL) {
         fscanf(menufile, "%d\n", &n_items);
         if (n_items > 10) n_items = 10;
         for (i=0; i<n_items; i++) {
            fscanf(menufile, "%d %d %d %d %d %d %s\n", &VMODE[i],
                   &LAT[i], &LON[i], &ALT[i], &FOV[i], &AZI[i], ITEMNAME[i]);
            strsub(ITEMNAME[i], '_', ' ');
            fl_addto_menu(fd_config->region_menu, ITEMNAME[i]);
            fl_set_menu_item_mode(fd_config->region_menu, i+1, FL_PUP_BOX);
         }
         fclose(menufile);
      } else fprintf(stderr, " Warning: Using default region views!!!\n"); 
//
//   Get the current 'graphics.par' parameter values for editting
//
      INFILE = TRUE;
      strcpy(JtampInputFile, "TestGBI.in");
      fl_set_input(fd_config->pinfile, JtampInputFile);
      sprintf(chtemp, "%f", JtampStartTime);
      fl_set_input(fd_config->p_starttime, chtemp);
      sprintf(chtemp, "%f", JtampEndTime);
      fl_set_input(fd_config->p_endtime, chtemp);
      sprintf(chtemp, "%f", JtampFirstTime);
      fl_set_input(fd_config->p_synctime, chtemp);
      sprintf(chtemp, "%f", JtampCycleTime);
      fl_set_input(fd_config->p_cycletime, chtemp);
      fl_set_button(fd_config->siminfile, INFILE);
//
//   Get the current 'input.par' parameter values for editting
//
      DATA_PARSER tadiljparser("input.par");
      tadiljparser.GoTo("parameters", NULL);
      TADILJFILE = tadiljparser.GetString("input_file");
      TRACKLABEL = tadiljparser.GetLogical("label_track");
      DROPTRACK  = tadiljparser.GetLogical("drop_track");
      SHOWR2     = tadiljparser.GetLogical("show_r2");
      KEEPLINKS  = tadiljparser.GetLogical("keep_links");
      SecDelay   = tadiljparser.GetInt("delay_secs");
      StartTime  = tadiljparser.GetFloat("starttime");
      EndTime    = tadiljparser.GetFloat("endtime"); 
      //
      strcpy(JtamvInputFile, TADILJFILE);
      fl_set_input(fd_config->vinfile, JtamvInputFile);
      sprintf(chtemp, "%d", SecDelay);
      fl_set_input(fd_config->delay, chtemp);
      sprintf(chtemp, "%f", StartTime);
      fl_set_input(fd_config->starttime, chtemp);
      sprintf(chtemp, "%f", EndTime);
      fl_set_input(fd_config->endtime, chtemp);
      fl_set_button(fd_config->tracklabel, TRACKLABEL);
      fl_set_button(fd_config->trackdrop, DROPTRACK);
      fl_set_button(fd_config->r2links, SHOWR2);
      fl_set_button(fd_config->keeplinks, KEEPLINKS);
//
//   Get the current 'jtamv.par' parameter values for editting
//
      DATA_PARSER gispparser("jtamv.par");
      gispparser.GoTo("parameters", NULL);
      CLASS       = gispparser.GetString("class");
      StepTime    = (double)gispparser.GetFloat("step_time");
      AUTORUN     = gispparser.GetLogical("autorun");
      AUTOEXIT    = gispparser.GetLogical("autoexit");
      StartRegion = gispparser.GetInt("region");
      TEXTURED    = gispparser.GetLogical("textured");
      GRIDLINE    = gispparser.GetLogical("gridline");
      BOUNDARY    = gispparser.GetLogical("boundary");
      ASSETS      = gispparser.GetLogical("assets");
      TRACKS      = gispparser.GetLogical("tracks");
      TRAILS      = gispparser.GetLogical("trails");
      IMPACTS     = gispparser.GetLogical("impacts");
      texfile     = gispparser.GetString("tex_file");
      INPUTSRC    = gispparser.GetString("input_source"); 
      PortID      = gispparser.GetInt("portid");
      hostid      = gispparser.GetString("hostid"); 
      //
      DEBUGING    = FALSE;
      TIMING      = FALSE;
      STATISTICS  = FALSE;
      LOGGING     = FALSE;
      //
      strcpy(HostID, hostid);
      strcpy(SrcInput, INPUTSRC);
      strcpy(Class, CLASS);
      strcpy(TexFile, texfile);
      //
      fl_set_input(fd_config->classification, Class);
      fl_set_input(fd_config->texfile, TexFile);
      fl_set_input(fd_config->hostid, HostID);
      sprintf(chtemp, "%d", PortID);
      fl_set_input(fd_config->portid, chtemp);
      fl_set_menu_item_mode(fd_config->region_menu, StartRegion+1, FL_PUP_CHECK);
      //sprintf(chtemp, "%d", StartRegion);
      //fl_set_input(fd_config->region, chtemp);
      fl_addto_choice(fd_config->source, "file");
      fl_addto_choice(fd_config->source, "socket");
      fl_addto_choice(fd_config->source, "speedes");
      fl_set_button(fd_config->autorun, AUTORUN);
      fl_set_button(fd_config->autoexit, AUTOEXIT);
      fl_set_button(fd_config->textured, TEXTURED);
      fl_set_button(fd_config->coverage, ASSETS);
      fl_set_button(fd_config->gridlines, GRIDLINE);
      fl_set_button(fd_config->boundary, BOUNDARY);
      fl_set_button(fd_config->timestats, TIMING);
      fl_set_button(fd_config->simstats, STATISTICS);
      fl_set_button(fd_config->tracks, TRACKS);
      fl_set_button(fd_config->trails, TRAILS);
      fl_set_button(fd_config->impacts, IMPACTS);

      pars_loaded = TRUE;
   }

   fl_hide_object(fd_ODI->config_button);
   fl_hide_object(fd_ODI->scenario_button);
   fl_hide_object(fd_ODI->planner_button);
   UtilVisible = FALSE;

   fl_show_form(fd_config->config, FL_PLACE_CENTER, FL_FULLBORDER,"Configuration Editor");
}

void scenarioCB(FL_OBJECT *ob, long data)
{
int      pid;

   fl_hide_object(fd_ODI->config_button);
   fl_hide_object(fd_ODI->scenario_button);
   fl_hide_object(fd_ODI->planner_button);
   UtilVisible = FALSE;

   switch (pid = fork()) {
   case 0:
     execlp("compose", "compose", NULL);
     perror("compose");
     exit (255);
     break;
   case -1:
     printf("Child process startup failed. PID = %d\n", pid);
     break;
   }
}

void schemeCB(FL_OBJECT *ob, long data)
{
int      pid;

   switch (pid = fork()) {
   case 0:
     execlp("scc", "scc", NULL);
     perror("scc");
     exit (255);
     break;
   case -1:
     printf("Child process startup failed. PID = %d\n", pid);
     break;
   }
}

void plannerCB(FL_OBJECT *ob, long data)
{
int      pid;

   fl_hide_object(fd_ODI->config_button);
   fl_hide_object(fd_ODI->scenario_button);
   fl_hide_object(fd_ODI->planner_button);
   UtilVisible = FALSE;

   switch (pid = fork()) {
   case 0:
     execlp("jtamp", "jtamp", NULL);
     perror("jtamp");
     exit (255);
     break;
   case -1:
     printf("Child process startup failed. PID is %d\n", pid);
     break;
   }
}

void battleCB(FL_OBJECT *ob, long data)
{
int      pid;

   switch (pid = fork()) {
   case 0:
     execlp("bmc3", "planner", NULL);
     perror("bmc3");
     exit (255);
     break;
   case -1:
     printf("Child process startup failed. PID is %d\n", pid);
     break;
   }
}

void visualCB(FL_OBJECT *ob, long data)
{
int      pid;

   switch (pid = fork()) {
   case 0:
     execlp("jtamv", "jtamv", NULL);
     perror("jtamv");
     exit (255);
     break;
   case -1:
     printf("Child process startup failed. PID = %d\n", pid);
     break;
   }
}

void quitCB(FL_OBJECT *object, long item_no)
{
int item = 0;

   switch (item)
   {
   case 0:
       exit(0);
       break;

     default:
       break;
   }
}

/* callbacks for form config */
void saveCB(FL_OBJECT *ob, long data)
{
int    itemno;

   itemno = data;
   WriteFile(filenames[itemno], itemno);
}

void saveasCB(FL_OBJECT *ob, long data)
{
int        itemno;
const char title[64]     = { "Save configuration file" };
const char directory[12] = { "./" };
const char pattern[12]   = { "*.par" };
const char initial[128]  = { "\0" };
const char *filename;

   itemno = data;

   strcpy((char *)initial,   filenames[itemno]);

   filename = fl_show_fselector(title, directory, pattern, initial);
   if (filename != NULL) WriteFile((char *)filename, itemno);
}

void classCB(FL_OBJECT *ob, long data)
{
   strncpy(Class, fl_get_input(fd_config->classification), 24);
}

void jtamvCB(FL_OBJECT *ob, long data)
{
   switch (data) {
     case 0:
       AUTORUN = !AUTORUN;
       break;
     case 1:
       AUTOEXIT = !AUTOEXIT;
       break;
     case 2:
       TEXTURED = !TEXTURED;
       break;
     case 3:
       ASSETS = !ASSETS;
       break;
     case 4:
       GRIDLINE = !GRIDLINE;
       break;
     case 5:
       BOUNDARY = !BOUNDARY;
       break;
     case 6:
       TIMING = !TIMING;
       break;
     case 7:
       STATISTICS = !STATISTICS;
       break;
     case 8:
       TRACKS = !TRACKS;
       break;
     case 9:
       TRAILS = !TRAILS;
       break;
     case 10:
       IMPACTS = !IMPACTS;
       break;
     case 11:
       LOGGING = !LOGGING;
       break;
     case 20:
       break;
     case 21:
       strncpy(TexFile, fl_get_input(fd_config->texfile), 128);
       break;
     case 22:
       strncpy(HostID, fl_get_input(fd_config->hostid), 64);
       break;
     case 23:
       PortID = atoi(fl_get_input(fd_config->portid));
       break;
     default:
       break;
   }
}

void sourceCB(FL_OBJECT *ob, long data)
{
int   item;

   item = fl_get_choice(fd_config->source)-1;
   //fl_set_object_label(fd_config->source, );
   strcpy(SrcInput, fl_get_choice_text(fd_config->source) );
}

void regionCB(FL_OBJECT *ob, long data)
{
int   item;

   item = fl_get_menu(fd_config->region_menu)-1;
   fprintf(stderr, "Selected menu item %d\n", item);
   StartRegion = item;
}

void infileCB(FL_OBJECT *ob, long data)
{
   switch (data) {
     case 0:
       strncpy(JtamvInputFile, fl_get_input(fd_config->vinfile), 128 );
       break;
     case 1:
       strncpy(JtampInputFile, fl_get_input(fd_config->pinfile), 128 );
       break;
     default:
       break;
   }
}

void inputCB(FL_OBJECT *ob, long data)
{
   switch (data) {
     case 0:
       TRACKLABEL = !TRACKLABEL;
       break;
     case 1:
       DROPTRACK = !DROPTRACK;
       break;
     case 2:
       SHOWR2 = !SHOWR2;
       break;
     case 3:
       KEEPLINKS = !KEEPLINKS;
       break;
     case 20:
       SecDelay = atoi(fl_get_input(fd_config->delay));
       break;
     default:
       break;
   }
}

void siminputCB(FL_OBJECT *ob, long data)
{
   switch (data) {
     case 0:
       INFILE = !INFILE;
       break;
     case 1:
       DROPTRACK = !DROPTRACK;
       break;
     case 2:
       SHOWR2 = !SHOWR2;
       break;
     case 3:
       KEEPLINKS = !KEEPLINKS;
       break;
     case 20:
       SecDelay = atoi(fl_get_input(fd_config->delay));
       break;
     default:
       break;
   }
}

void timeCB(FL_OBJECT *ob, long data)
{
   switch (data) {
     case 0:
       StartTime = atof(fl_get_input(fd_config->starttime));
       break;
     case 1:
       EndTime = atof(fl_get_input(fd_config->endtime));
       break;
     default:
       break;
   }
}

void simtimeCB(FL_OBJECT *ob, long data)
{
   switch (data) {
     case 0:
       JtampStartTime = atof(fl_get_input(fd_config->p_starttime));
       break;
     case 1:
       JtampEndTime = atof(fl_get_input(fd_config->p_endtime));
       break;
     case 2:
       JtampFirstTime = atof(fl_get_input(fd_config->p_synctime));
       break;
     case 3:
       JtampCycleTime = atof(fl_get_input(fd_config->p_cycletime));
       break;
     default:
       break;
   }
}

void doneCB(FL_OBJECT *ob, long data)
{
   fl_hide_form(fd_config->config);
}

void regeditCB(FL_OBJECT *ob, long data)
{
int             i;

   fl_clear_browser(fd_regionedit->region_browser);

   for (i=0; i<n_items; i++) {
      fl_add_browser_line(fd_regionedit->region_browser, ITEMNAME[i]);
   }

   fl_show_form(fd_regionedit->regionedit, FL_PLACE_CENTER, FL_FULLBORDER,"Region Editor");
}

void regioneditCB(FL_OBJECT *ob, long data)
{
FILE            *outfile;
int             i;

   switch (data) {
     case 0:
       strcpy(ITEMNAME[c_item], fl_get_input(fd_regionedit->region_name));
       fl_replace_browser_line(fd_regionedit->region_browser, c_item+1, ITEMNAME[c_item]);
       break;

     case 1:
       LAT[c_item] = atoi(fl_get_input(fd_regionedit->region_lat));
       break;

     case 2:
       LON[c_item] = atoi(fl_get_input(fd_regionedit->region_lon));       
       break;

     case 3:
       ALT[c_item] = atoi(fl_get_input(fd_regionedit->region_alt));       
       break;

     case 4:
       FOV[c_item] = atoi(fl_get_input(fd_regionedit->region_fov));       
       break;

     case 5:
       AZI[c_item] = atoi(fl_get_input(fd_regionedit->region_azi));       
       break;

     case 6:
       VMODE[c_item] = 0;       
       break;

     case 7:
       VMODE[c_item] = 1;       
       break;

     case 10:
       if ((outfile = fopen("regions.dat", "w")) != NULL) {
          fprintf(outfile, "%5d\n", n_items);
          for (i=0; i<n_items; i++) {
	     strsub(ITEMNAME[i], ' ', '_');
             fprintf(outfile, "%5d %5d %5d %10d %5d %5d   %s\n", VMODE[i],
                    LAT[i], LON[i], ALT[i], FOV[i], AZI[i], ITEMNAME[i]);
          }
          fprintf(outfile, "\n");
          fprintf(outfile, " mode   lat   lon        alt   fov   azi   menu_name\n\n");
          fprintf(outfile, "NOTE!! This file should have at most 10 entries in it!!\n\n");
          fprintf(outfile, "Also, don't use blanks in menu names and limit\n");
          fprintf(outfile, "  menu names to 20 characters\n");
          fclose(outfile);
       } else fprintf(stderr, " Warning: Could not open region output file.\n");
       fl_hide_form(fd_regionedit->regionedit);
       break;

     case 11:
       fl_hide_form(fd_regionedit->regionedit);
       break;

     default:
       break;
   }
}

void regbrowserCB(FL_OBJECT *ob, long data)
{
int             i, lineno;
char            chvalue[16];

   lineno = fl_get_browser(fd_regionedit->region_browser);
   i = lineno - 1;
   c_item = i;

   fl_set_input(fd_regionedit->region_name, ITEMNAME[i]);

   sprintf(chvalue, "%d", LAT[i]);
   fl_set_input(fd_regionedit->region_lat, chvalue);
   sprintf(chvalue, "%d", LON[i]);
   fl_set_input(fd_regionedit->region_lon, chvalue);
   sprintf(chvalue, "%d", ALT[i]);
   fl_set_input(fd_regionedit->region_alt, chvalue);
   sprintf(chvalue, "%d", FOV[i]);
   fl_set_input(fd_regionedit->region_fov, chvalue);
   sprintf(chvalue, "%d", AZI[i]);
   fl_set_input(fd_regionedit->region_azi, chvalue);
}

void nothingCB(FL_OBJECT *ob, long data)
{
}

