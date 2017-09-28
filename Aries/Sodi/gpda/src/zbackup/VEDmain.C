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
#include "VEDforms.h"

/* --------------------------------------------------------------------- */
 
#ifndef FALSE
#define FALSE   0
#endif
#ifndef TRUE
#define TRUE    1
#endif
 
/* --------------------------------------------------------------------- */
  
typedef int     Boolean;

FILE            *menufile;

Boolean         AUTORUN, AUTOEXIT, NOEXIT;
Boolean         TEXTURED, GRIDLINE, BOUNDARY;
Boolean         ASSETS;
Boolean         DEBUGING, TIMING, STATISTICS, LOGGING;
Boolean         TRACKS, TRAILS, IMPACTS, SHOWERROR;
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
char            filenames[24];

int             n_items;
int             c_item;
int             VMODE[10], LAT[10], LON[10], ALT[10], FOV[10], AZI[10];
char            ITEMNAME[10][24];

FL_OBJECT       *scipm;
FL_OBJECT       *bmdoipm, *jntfipm;

FD_config       *fd_config;
FD_regionedit   *fd_regionedit;

void util_configCB(FL_OBJECT *ob, long data);
 
extern int  EraseActiveEntry(char *text);
extern int  StoreActiveEntry(char *text);
 
/* --------------------------------------------------------------------- */

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
static int      pars_loaded = FALSE;
char            *BITMAPDIR;
char            *TADILJFILE;
char            *INPUTSRC;
char            *CLASS;
char            *texfile;
char            *hostid;
char            xpmfile[128];

   fl_initialize(&argc, argv, 0, 0, 0);
   fd_config = create_form_config();
   fd_regionedit = create_form_regionedit();

   if (!pars_loaded) {
      strcpy(filenames, "graphics.par\0");

      fl_clear_choice(fd_config->region_choice);

      if ((menufile = fopen("regions.dat", "r")) != NULL) {
         fscanf(menufile, "%d\n", &n_items);
         if (n_items > 10) n_items = 10;
         for (i=0; i<n_items; i++) {
            fscanf(menufile, "%d %d %d %d %d %d %s\n", &VMODE[i],
                   &LAT[i], &LON[i], &ALT[i], &FOV[i], &AZI[i], ITEMNAME[i]);
            strsub(ITEMNAME[i], '_', ' ');
            fl_addto_choice(fd_config->region_choice, ITEMNAME[i]);
            fl_set_choice_item_mode(fd_config->region_choice, i+1, FL_PUP_BOX|FL_PUP_RADIO);
         }
         fclose(menufile);
      } else fprintf(stderr, " Warning: Using default region views!!!\n"); 
//
//   Get the current 'display' parameter values for editting
//
      DATA_PARSER gispparser("graphics.par");
                    gispparser.GoTo("display", NULL);
      CLASS       = gispparser.GetString("class");
      INPUTSRC    = gispparser.GetString("input_source");
      StartRegion = gispparser.GetInt("region");
      AUTORUN     = gispparser.GetLogical("autorun");
      AUTOEXIT    = gispparser.GetLogical("autoexit");
      NOEXIT      = gispparser.GetLogical("noexit");
      TEXTURED    = gispparser.GetLogical("textured");
      GRIDLINE    = gispparser.GetLogical("gridline");
      BOUNDARY    = gispparser.GetLogical("boundary");
      ASSETS      = gispparser.GetLogical("assets");
      TRACKS      = gispparser.GetLogical("tracks");
      TRAILS      = gispparser.GetLogical("trails");
      IMPACTS     = gispparser.GetLogical("impacts");
      IMPACTS     = gispparser.GetLogical("impacts");
      SHOWERROR   = gispparser.GetLogical("trkerror");
      DROPTRACK   = gispparser.GetLogical("drop_track");
      SHOWR2      = gispparser.GetLogical("show_r2");
      KEEPLINKS   = gispparser.GetLogical("keep_links");
      DEBUGING    = gispparser.GetLogical("debuging");
      TIMING      = gispparser.GetLogical("timing");
      STATISTICS  = gispparser.GetLogical("statistics");
      LOGGING     = gispparser.GetLogical("logging");
      texfile     = gispparser.GetString("tex_file");
      StepTime    = (double)gispparser.GetFloat("step_time");
      //
      strcpy(Class, CLASS);
      fl_set_input(fd_config->classification, Class);
      strcpy(SrcInput, INPUTSRC);
      if ( strcmp(SrcInput, "File") == 0 ) fl_set_choice(fd_config->source, 1);
      if ( strcmp(SrcInput, "Socket") == 0 ) fl_set_choice(fd_config->source, 2);
      if ( strcmp(SrcInput, "Speedes") == 0 ) fl_set_choice(fd_config->source, 3);

      fl_set_choice(fd_config->region_choice, StartRegion+1);
      fl_set_choice_item_mode(fd_config->region_choice, StartRegion+1, FL_PUP_RADIO);
      //sprintf(chtemp, "%d", StartRegion);
      //fl_set_input(fd_config->region, chtemp);
      fl_set_button(fd_config->autorun, AUTORUN);
      fl_set_button(fd_config->autoexit, AUTOEXIT);
      //fl_set_button(fd_config->noexit, NOEXIT);
      fl_set_button(fd_config->textured, TEXTURED);
      fl_set_button(fd_config->gridlines, GRIDLINE);
      fl_set_button(fd_config->boundary, BOUNDARY);
      fl_set_button(fd_config->coverage, ASSETS);
      fl_set_button(fd_config->tracks, TRACKS);
      fl_set_button(fd_config->trails, TRAILS);
      fl_set_button(fd_config->impacts, IMPACTS);
      //fl_set_button(fd_config->impacts, SHOWERROR);
      fl_set_button(fd_config->tracklabel, TRACKLABEL);
      fl_set_button(fd_config->trackdrop, DROPTRACK);
      fl_set_button(fd_config->r2links, SHOWR2);
      fl_set_button(fd_config->keeplinks, KEEPLINKS);
      fl_set_button(fd_config->timestats, TIMING);
      fl_set_button(fd_config->simstats, STATISTICS);
      strcpy(TexFile, texfile);
      fl_set_input(fd_config->texfile, TexFile);
//
//   Get the current 'input' parameter values for editting
//
                    gispparser.GoTo("input", NULL);
      TADILJFILE  = gispparser.GetString("data_file");
      SecDelay    = gispparser.GetInt("delay_secs");
      PortID      = gispparser.GetInt("portid");
      hostid      = gispparser.GetString("hostid");
      //
      strcpy(JtamvInputFile, TADILJFILE);
      fl_set_input(fd_config->vinfile, JtamvInputFile);
      sprintf(chtemp, "%d", SecDelay);
      fl_set_input(fd_config->delay, chtemp);
      strcpy(HostID, hostid);
      fl_set_input(fd_config->hostid, HostID);
      sprintf(chtemp, "%d", PortID);
      fl_set_input(fd_config->portid, chtemp);
//
//   Get the current 'graphics' parameter values for editting
//
                       gispparser.GoTo("parameters", NULL);
      JtampStartTime = gispparser.GetFloat("start_time");
      JtampFirstTime = gispparser.GetFloat("first_time");
      JtampCycleTime = gispparser.GetFloat("cycle_time");
      JtampEndTime   = gispparser.GetFloat("tend");
      CpuTime        = gispparser.GetInt("cpu_time");;
      READALL        = gispparser.GetLogical("readall");
      INFILE         = gispparser.GetLogical("input_file");
      OTFILE         = gispparser.GetLogical("output_file");
      ROLLBACK       = gispparser.GetLogical("test_rollback");
      //
      sprintf(chtemp, "%f", JtampStartTime);
      fl_set_input(fd_config->starttime, chtemp);
      sprintf(chtemp, "%f", JtampFirstTime);
      fl_set_input(fd_config->synctime, chtemp);
      sprintf(chtemp, "%f", JtampCycleTime);
      fl_set_input(fd_config->cycletime, chtemp);
      sprintf(chtemp, "%f", JtampEndTime);
      fl_set_input(fd_config->endtime, chtemp);

      fl_set_button(fd_config->siminfile, INFILE);

      strcpy(JtampInputFile, "TestGBI.in");
      fl_set_input(fd_config->pinfile, JtampInputFile);

      pars_loaded = TRUE;
   }

   fl_show_form(fd_config->config, FL_PLACE_CENTER,FL_FULLBORDER, "JtamvEd");

   fl_do_forms();

   return 0;
}
 
void
WriteFile(char *filename, int itemno)
{
FILE            *fp;

    fp = fopen(filename, "w+");

    fprintf(fp, "display {\n");
       fprintf(fp, "  string class %s\n",            Class);
       fprintf(fp, "  string input_source %s\t\n", SrcInput,
               "// Can be 'File' or 'Socket' or 'Speedes'");
       fprintf(fp, "  int region %d\t\t\t\n",      StartRegion,
	       "// Start with display of this region");
       fprintf(fp, "  logical autorun %s\t\t\n",   (AUTORUN==FALSE ? "F" : "T"),
	       "// Start running without user action");
       fprintf(fp, "  logical noexit %s\t\t\n",    (NOEXIT==FALSE ? "F" : "T"),
	       "// Don't exit if EXIT button pushed");
       fprintf(fp, "  logical autoexit %s\t\t\n",  (AUTOEXIT==FALSE ? "F" : "T"),
	       "// Exit at end of run");
       fprintf(fp, "  logical textured %s\t\t\n",  (TEXTURED==FALSE ? "F" : "T"),
	       "// Start with textured earth, else with Blue?");
       fprintf(fp, "  logical gridline %s\t\t\n",  (GRIDLINE==FALSE ? "F" : "T"),
	       "// Start with gridlines every 10 degrees?");
       fprintf(fp, "  logical boundary %s\t\t\n",  (BOUNDARY==FALSE ? "F" : "T"),
	       "// Start with political boundary lines?");
       fprintf(fp, "  logical assets %s\t\t\n",    (ASSETS==FALSE ? "F" : "T"),
	       "// Start with assets displayed?");
       fprintf(fp, "  logical tracks %s\t\t\n",    (TRACKS==FALSE ? "F" : "T"),
	       "// Draw model at (x,y,z) position?");
       fprintf(fp, "  logical trails %s\t\t\n",    (TRAILS==FALSE ? "F" : "T"),
	       "// Connect the dots?");
       fprintf(fp, "  logical impacts %s\t\t\n",   (IMPACTS==FALSE ? "F" : "T"),
	       "// Show the impact ellipses?");
       fprintf(fp, "  logical trkerror %s\t\t\n",  (SHOWERROR==FALSE ? "F" : "T"),
	       "// Show the impact ellipses?");
       fprintf(fp, "  logical label_track %s\n",    (TRACKLABEL==FALSE ? "F" : "T"));
       fprintf(fp, "  logical drop_track %s\n",     (DROPTRACK==FALSE ? "F" : "T"));
       fprintf(fp, "  logical show_r2 %s\n",        (SHOWR2==FALSE ? "F" : "T"));
       fprintf(fp, "  logical keep_links %s\n",     (KEEPLINKS==FALSE ? "F" : "T"));
       fprintf(fp, "  logical debuging %s\t\t\n",  (DEBUGING==FALSE ? "F" : "T"),
	       "// Is this a debugging run?");
       fprintf(fp, "  logical timing %s\t\t\n",    (TIMING==FALSE ? "F" : "T"),
	       "// Collect timing information?");
       fprintf(fp, "  logical logging %s\t\t\n",   (LOGGING==FALSE ? "F" : "T"),
	       "// Generate log file?");
       fprintf(fp, "  logical statistics %s\n",    (STATISTICS==FALSE ? "F" : "T"));
       fprintf(fp, "  string tex_file %s\n",       TexFile);
          // File name of globe texture file
       fprintf(fp, "  float step_time %f\t\n",     StepTime,
	       "// GVT update interval (for PLAYBACK mode)");

    fprintf(fp, "}\n\n");
    fprintf(fp, "input {\n");
       fprintf(fp, "  string data_file %s\n",        JtamvInputFile);
       fprintf(fp, "  int delay_secs %d\n",          SecDelay);
       fprintf(fp, "  string hostid %s\n",           HostID);
       fprintf(fp, "  int portid %d\n",              PortID);

    fprintf(fp, "}\n\n");
    fprintf(fp, "parameters {\n");
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

   fprintf(fp, "}\n");
   fclose(fp);
}
/*                                                                       */
/* --------------------------------------------------------------------- */
/*      C A L L B A C K    R O U T I N E S    S T A R T    H E R E       */
/* --------------------------------------------------------------------- */
/*                                                                       */
void ODIexitCB(FL_OBJECT *ob, long data)
{
   fl_hide_form(fd_config->config);
 
   exit(0);
}

void ODInoneCB(FL_OBJECT *ob, long data)
{
 
   return;
}

void nothingCB(FL_OBJECT *ob, long data)
{
}

void saveCB(FL_OBJECT *ob, long data)
{
int    itemno;

   itemno = data;
   WriteFile(filenames, itemno);
}

void saveasCB(FL_OBJECT *ob, long data)
{
int        itemno;
const char title[64]     = { "Save Parameter File" };
const char directory[12] = { "./" };
const char pattern[12]   = { "*.par" };
const char initial[128]  = { "new_graphics.par\0" };
const char *filename;

   itemno = data;

   filename = fl_show_fselector(title, directory, pattern, initial);
   if (filename != NULL) WriteFile((char *)filename, itemno);
}

void odifilesCB(FL_OBJECT *ob, long data)
{
int        itemno;
const char title[64]     = { "Data Input file" };
const char directory[12] = { "./" };
const char pattern[12]   = { "*" };
const char initial[128]  = { "arcticsubs.jdn\0" };
const char *filename;

   itemno = data;

   filename = fl_show_fselector(title, directory, pattern, initial);
   if (filename != NULL) {
      strcpy(JtamvInputFile, filename);
      fl_set_input(fd_config->vinfile, JtamvInputFile);
   }
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

   item = fl_get_choice(fd_config->region_choice)-1;
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
       JtampStartTime = atof(fl_get_input(fd_config->starttime));
       break;
     case 1:
       JtampEndTime = atof(fl_get_input(fd_config->endtime));
       break;
     case 2:
       JtampFirstTime = atof(fl_get_input(fd_config->synctime));
       break;
     case 3:
       JtampCycleTime = atof(fl_get_input(fd_config->cycletime));
       break;
     default:
       break;
   }
}

void doneCB(FL_OBJECT *ob, long data)
{
   fl_hide_form(fd_config->config);
   exit(0);
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
