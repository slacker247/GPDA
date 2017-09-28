
#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <math.h> 
#include <time.h>
#include <signal.h>
#include <sys/ioctl.h>
#include <errno.h>
#ifdef LINKSC
#include <rpc/xdr.h>
#endif
/*
 *   Include the Sim Commander stuff
 */
#include "demo_opcodes.h"
#ifdef LINKSC
#include "demo_header.h"
#include "demo_msg_body.h"
#include "demo_players.h"
#include "demo_strings.h"
#endif

#include "Globals.h"
#include "forms.h"
#include "ITELforms.h"
#include "CLUforms.h"

/* --------------------------------------------------------------------- */

#ifndef FALSE
#define FALSE   0
#endif
#ifndef TRUE
#define TRUE    1
#endif

#define PUSHED  1

/* --------------------------------------------------------------------- */

typedef struct {
  char          ChType[40];
  char          filebase[128];
} ITELTYPE;

typedef struct LEXICAL {
  float         lower;
  float         upper;
  char          desc[16];
};

/* --------------------------------------------------------------------- */

int             ITELinited = 0;
char            iteltemp[1024];
char            ITELlabel[32];

FILE            *itelfp;
FILE            *fp;
int             Itel_filled = FALSE;
int             OutFileOpen = FALSE;
ITELTYPE        iteltypes[20];

extern char     *SIMTEST;
extern char     *DEBUG;
extern char     *SOCKET;
extern char     UserName[48];
extern int      LexCount;;
extern LEXICAL  LexTable[9];

int             FieldCount = 52;
char            FieldNames[52][16] = {
                  "Case_ID",    "Hypothesis",   "Belief",      "Disbelief",
                  "Report Org", "Report Affil", "Report Desc", "Report Name",
                  "Enemy Org",  "Enemy Affil",  "Enemy Desc",  "Enemy Name",
                  "Ally Org",   "Ally Affil",   "Ally Desc",   "Ally Name",
                  "Other Org",  "Other Affil",  "Other Desc",  "Other Name",
                  "Region",     "Nation",       "Locale",      "City",    
                  "Structure",  "Latitude",     "Longitude",   "Altitude",
	  	  "Subject",    "Action",       "Objective",
                  "Strategic",  "Operation",    "Tactical",    "Task",
                  "Method",     "Desc_1",       "Desc_2",      "Desc_3",
                  "Casualties", "Injuries",     "Cost",        "Adjective",
                  "Start_Time", "End_Time",     "Duration",    "Time_Frame",
		  " ",
                  "Info-ID",    "Prepared",     "Prep-Date",   "Mission" };
 
FD_bmcintel     *fd_bmcintel;
extern FD_itelfields *fd_itelfields;
 
extern int      EraseActiveEntry(char *text);
extern int      StoreActiveEntry(char *text);
extern int      FinishUp();  

/* --------------------------------------------------------------------- */

void ITELinit();
void ITELnext();
void ITELshow(int xpos, int ypos, int width, int height, Window winid, int mode, char *fn);
void ClearFields();
void FillField(FILE *fp);
/*
extern "C" void SC_intel_time(int day, float hours);
extern "C" void SC_intel_text(char *str);
extern "C" void SC_intel_src(char *chline);
*/
#ifdef LINKSC
extern     void Net_init(int port, char *host);
extern     void Net_write(char *buf, int bytes);
extern     int  Net_read(char *buf, int bufsize);
extern     void Net_close();
extern "C" void encode_header (XDR *xdrs_en, struct header *en, int *nargs);
extern "C" void decode_header (XDR *xdrs_de, struct header *de, int *nargs);
extern "C" int  encode_body (XDR *xdrs_en, struct SEND_ALL *en, int *opcode, int *nargs);
extern "C" int  decode_body (XDR *xdrs_de, struct RTRN_ALL *de, int *opcode, int *nargs);
extern "C" void encode_header (XDR *xdrs_en, struct header *en, int *nargs);
extern "C" void decode_header (XDR *xdrs_de, struct header *de, int *nargs);
extern "C" int  encode_body (XDR *xdrs_en, struct SEND_ALL *en, int *opcode, int *nargs);
extern "C" int  decode_body (XDR *xdrs_de, struct RTRN_ALL *de, int *opcode, int *nargs);
extern "C" void translator (char *messages, char *decisions, unsigned int *xdrsize);
#endif
/*                                                                       */
/* --------------------------------------------------------------------- */
/*              M A I N   P R O G R A M   S T A R T S   H E R E          */
/* --------------------------------------------------------------------- */
/*                                                                       */
void ITELinit()
{
FILE*           typefp;
int             i, j, k;
int             num_types;

   if (ITELinited) return;

   fd_bmcintel = create_form_bmcintel();

   strcpy(ITELlabel, "Evidence-Manager"); 

   fl_set_browser_fontsize(fd_bmcintel->intel_source, 10);
   fl_set_browser_fontstyle(fd_bmcintel->intel_source, FL_FIXED_STYLE|FL_BOLD_STYLE);

   for (i=0; i<LexCount; i++) { 
     fl_addto_choice(fd_bmcintel->itel_belief, LexTable[i].desc);
     fl_addto_choice(fd_bmcintel->itel_disbelief, LexTable[i].desc);
   }

   typefp = fopen("DSBinit.dat", "r");
   do fgets(iteltemp, 128, typefp); while (iteltemp[0] == '#');    // Read # assessments
   sscanf(iteltemp, "%d", &num_types);

   for (j=0; j<num_types; j++) {
     do fgets(iteltemp, 128, typefp); while (iteltemp[0] == '#');
     // 
     // Read assessment type and file base names
     //
     sscanf(iteltemp, "%s %s", iteltypes[j].ChType, iteltypes[j].filebase);
     strsub(iteltypes[j].ChType, '_', ' ');
     fl_addto_choice(fd_bmcintel->itel_mission, iteltypes[j].ChType);
   }
   fclose(typefp);

   itelmissionCB(NULL, 0);

   ITELinited = TRUE;
   typefp = fopen("GPDAinfo.log", "a");
   fprintf(typefp, "Data Manager algorithm initialization complete.\n");
   fclose(typefp);

   return;
}

void ITELexitCB(FL_OBJECT *ob, long data)
{
   if (OutFileOpen) fclose(fp);

   fl_hide_form(fd_bmcintel->bmcintel);
   EraseActiveEntry(ITELlabel);

   FinishUp();
 
   return;
}
 
void ITELnoneCB(FL_OBJECT *ob, long data)
{
 
   return;
}

extern "C" { 
void
SC_intel_time(int day, float hours)
{
char   chline[16];
/*
   sprintf(chline, "%d", day);
   fl_set_input(fd_bmcintel->intel_day, chline);
   sprintf(chline, "%f", hours);
   fl_set_input(fd_bmcintel->intel_hour, chline);
*/     
}

void
SC_intel_text(char *str)
{
 
   fl_clear_browser(fd_bmcintel->intel_browser);
   fl_add_browser_line(fd_bmcintel->intel_browser, str);
}

void
SC_intel_src(char *chline)
{
   fl_clear_browser(fd_bmcintel->intel_source);   
   fl_addto_browser(fd_bmcintel->intel_source, chline);
}
}  // "C" linkages 

void ITELnext()
{
int             item = 0;
static int      opened = FALSE;
static FILE     *infile;
static int      icount = 0;

int             i,j,l,n;
int             nsource, nlines, srcid, nargs_in;
int             day;
float           hours;
float           x, y;
char            ch;
char            chsource[5][16] = { "Unknown", "Open Source", "Human Intel",
                                    "Signal Intel", "Image Intel" };
char            chline[80];
char            ipline[1280];
char            chsrc[16];
char            chout[5][120];

int             nargs;
unsigned int    xferbytes;
char            evid_lab1[8], evid_lab2[8], evid_lab3[8];
char            buf[2000], src[5], dst[5], dea[5];
#ifdef LINKSC
char            *xdrbuf;
int             sp=BtlP, dp=BMDC1_DR1;
unsigned int    four = 4, bytecnt, xdrsize;
XDR             xdrs;
struct header   BPen, BPde;
struct SEND_ALL BPen_body;
struct RTRN_ALL BPde_body;
#endif  


   if (!opened) {
      infile = fopen("intel.in", "r+");
      opened = TRUE;
   }
//
//   Get the next INTEL record
//
   if (fscanf(infile, "%d %f", &day, &hours) <= 0) {
      fclose(infile);
      opened = FALSE;
      return;
   }
   SC_intel_time(day, hours);

   fscanf(infile, "%d", &nsource);        // get number of sources
   strcpy(ipline, "\0");
   fl_clear_browser(fd_bmcintel->itel_sources);

   fl_set_browser_fontsize(fd_bmcintel->intel_source, FL_SMALL_SIZE);
   fl_set_browser_fontstyle(fd_bmcintel->intel_source, FL_FIXED_STYLE);
   fl_clear_browser(fd_bmcintel->intel_source);
   sprintf(iteltemp, "@b@i@C4@N@_%-16s %-33s   %6s   %-10s   %-8s   %-8s   %-8s",
           "Evidence Source", "    Belief           Disbelief", "Time", "Duration",
           evid_lab1, evid_lab2, evid_lab3);
   fl_addto_browser(fd_bmcintel->intel_source, iteltemp);
//
//   Build the message header for the SC
//
#ifdef LINKSC
   if (SIMTEST) {
      xdrbuf = (char *)malloc(2000);
      xdrmem_create(&xdrs, xdrbuf, (unsigned)2000, XDR_ENCODE);
 
      BPen.SrcID     = BtlP;
      BPen.DstID     = BMDC1_DR1;
      BPen.SCID      = Sim_Cmdr_ID;
      BPen.opcode    = OP_INTEL_INFO;
      BPen.SCactive  = 0;
      BPen.gvt       = 0.0;
      BPen.reserved7 = 0;
      BPen.reserved8 = 0;
      BPen.reserved9 = 0;
      encode_header (&xdrs, &BPen, &nargs);

      BPen_body.intel_info_body_struct.II_Day     = day;
      BPen_body.intel_info_body_struct.II_Hrs     = hours;
      BPen_body.intel_info_body_struct.II_Nam_Num = nsource;
   }
#endif

   for (i=0; i<nsource; i++) {
      j = 0;
      fscanf(infile, "%s %d %f %f\n", chsrc, &srcid, &x, &y);
      if (chsrc[0] == 'O') j = 1;
      else if (chsrc[0] == 'H') j = 2;
      else if (chsrc[0] == 'S') j = 3;
      else if (chsrc[0] == 'I') j = 4;
      fl_addto_browser(fd_bmcintel->itel_sources, chsource[j]);
      sprintf(chline, "%-16s %10d   %10.4f   %10.4f\n", chsource[j], srcid, x, y);
      strcat(ipline, chline);
#ifdef LINKSC
      if (SIMTEST) {
	//if (PLANDBUG)
	//fprintf(stderr, "%s %d %f %f\n", chsrc, srcid, x, y);         
         strcpy (BPen_body.intel_info_body_struct.II_Nam_Src[i], chsrc);
         BPen_body.intel_info_body_struct.II_Nam_Loc[i]        = srcid;
         BPen_body.intel_info_body_struct.II_Val[i]            = x;
         BPen_body.intel_info_body_struct.II_Per[i]            = y;
      }
#endif
   }
   //SC_intel_src(ipline);

   fscanf(infile, "%d", &nlines);         // get number of lines
   fgetc(infile);                         // get newline char
   strcpy(chout[0], " ");
   strcpy(chout[1], " ");
   strcpy(chout[2], " ");
   strcpy(chout[3], " ");
   strcpy(chout[4], " ");
   for (i=0; i<nlines; i++) {
      ch = fgetc(infile); l = 0;          // get 1st char
      while (ch != '\n') {
        chline[l] = ch;
        l = l+1;
        ch = fgetc(infile);
      }
      chline[l] = '\0';
      strcpy(chout[i], chline);
   }
   sprintf(ipline, "%s\n%s\n%s\n%s\n%s", chout[0], chout[1], chout[2], chout[3], chout[4]);
   SC_intel_text(ipline);
//
//   Send the message to the SC
//
#ifdef LINKSC
   if (SIMTEST) {
      for (i = 0; i < 5; i++) {
         strcpy (BPen_body.intel_info_body_struct.II_Txt[i], chout[i]);
      } 
      if (encode_body (&xdrs, &BPen_body, &BPen.opcode, &nargs_in) != 0)
         fprintf (stderr, "Encode failed for opcode %d .\n", BPen.opcode); 
      xdrsize = xdr_getpos(&xdrs);
      //
      //  Send the request to the SC and cleanup
      //
      if (SOCKET) {
         Net_write(xdrbuf, xdrsize);
         //
         //  Wait for the response from the SC
         //
         //if (PLANDBUG)
	 //fprintf(stderr, "Planner: waiting for response. Opcode %d\n", BPen.opcode);
         xferbytes = Net_read(buf, 2000);
      } else {
         translator(xdrbuf, buf, &xferbytes);
      }
      free(xdrbuf);
      xdr_destroy(&xdrs);
   }
#endif 
}

int ITELclose(FL_FORM *form, void *data)
{
   ITELexitCB(NULL, 0);

   return (0);
}

void ITELshow(int xpos, int ypos, int width, int height, Window mainwinID,
	      int mode, char *fname)
{
int             i, j;
char            chflag;
char            chtemp[32];
Window          winid;
//
//   Pop up the window
//
   if(!fl_form_is_visible(fd_bmcintel->bmcintel) ) {
      fl_transient();
      fl_winposition(xpos, ypos);
      fl_initial_winsize(width, height);
      winid = fl_prepare_form_window(fd_bmcintel->bmcintel,
                                     FL_PLACE_POSITION,FL_TRANSIENT, ITELlabel);
      //fl_set_form_geometry(fd_bmcintel->bmcintel, xpos, ypos, width, height);
      fl_winreshape(winid, xpos, ypos, width, height);
      fl_show_form_window(fd_bmcintel->bmcintel);
      fl_set_form_atclose(fd_bmcintel->bmcintel, ITELclose, 0);
      StoreActiveEntry(ITELlabel);
      ClearFields();
   }

   Itel_filled = FALSE;
//
//   Re-label the widgets according to the mission
//
   j = fl_get_choice(fd_bmcintel->itel_mission) - 1;

   if (strcmp(iteltypes[j].filebase, "*") == 0) {
     strcpy(iteltemp, fl_show_fselector("RQ Label file", "DSBFiles/", "*.rq", NULL));
     if (strlen(iteltemp) == 0) return;
   } else {
     strcpy(iteltemp, "DSBFiles/");
     strcat(iteltemp, iteltypes[j].filebase);
     strcat(iteltemp, ".rq");
   }
 
   if ((fp = fopen(iteltemp, "r")) != NULL) {
     i = -1;
     j = -1;
     while (!feof(fp)) {
       do fgets(iteltemp, 128, fp); while ((iteltemp[0] == '#') && !feof(fp));
       if (feof(fp)) break;
       sscanf(iteltemp, "%c %s", &chflag, chtemp);
       strsub(chtemp, '_', ' ');
       if (chflag == '-') {
	 i++;
	 strcpy(FieldNames[i], chtemp);
       }
       else if (chflag == '+') {
	 j++;
	 fl_set_object_label(fd_bmcintel->itel_frame[j], chtemp);
       }
       else if (chflag == '>') {
	 i++;
	 fl_set_object_label(fd_bmcintel->itel_field[i], chtemp);
	 strcpy(FieldNames[i], chtemp);
       }
     }
     fclose(fp);
   }
//
//   Activate the necessary buttons
//
   fl_deactivate_object(fd_bmcintel->itel_add);
   fl_set_object_lcol(fd_bmcintel->itel_add, FL_INACTIVE_COL);
   fl_activate_object(fd_bmcintel->itel_newfile);
   fl_set_object_lcol(fd_bmcintel->itel_newfile, FL_GREEN);
   fl_activate_object(fd_bmcintel->itel_openfile);
   fl_set_object_lcol(fd_bmcintel->itel_openfile, FL_GREEN);
   fl_deactivate_object(fd_bmcintel->itel_firstrec);
   fl_set_object_lcol(fd_bmcintel->itel_firstrec, FL_INACTIVE_COL);
   fl_deactivate_object(fd_bmcintel->itel_nextrec);
   fl_set_object_lcol(fd_bmcintel->itel_nextrec, FL_INACTIVE_COL);
//
//   Set up the form according to the input mode
//
   switch (mode) {
   case 0:
     fl_set_button(fd_bmcintel->itel_evidmode, PUSHED);
     break;

   case 1:
     fl_set_button(fd_bmcintel->itel_datamode, PUSHED);
     break;

   case 2:
     fl_set_button(fd_bmcintel->itel_datamode, PUSHED);
     if ((itelfp = fopen(fname, "r")) != NULL) {
       FillField(itelfp);                              // Fill up the form with the record
       fclose(itelfp);                                 // Close evidence file
       Itel_filled = TRUE;
     } else
       fprintf(stderr, "Can't open input file %s!\n", fname);     
     break;

   default:
     break;
   }
 
   return;
}
/*                                                                       */
/* --------------------------------------------------------------------- */
/*      C A L L B A C K    R O U T I N E S    S T A R T    H E R E       */
/* --------------------------------------------------------------------- */
/*                                                                       */
void ITELtimingCB(FL_OBJECT *ob, long data)
{
 
   return;
}

void itelsubjectCB(FL_OBJECT *ob, long data)
{
 
   return;
}

void itelocationCB(FL_OBJECT *ob, long data)
{
 
   return;
}

void itelevidenceCB(FL_OBJECT *ob, long data)
{

   fl_set_input(fd_bmcintel->itel_field[1],
	        fl_get_choice_text(fd_bmcintel->itel_source));
   return;
}

void ClearFields()
{
int             i, k;

   for (i=4; i<FieldCount-9; i++) {
     fl_set_input(fd_bmcintel->itel_field[i],    "?");
   }

   return;
}

void FillField(FILE *fp)
{
int             i, j, k;
int             noEOF = TRUE;
long            irc;
char            chname[1024];
char            *str;
time_t          clock;
struct tm       *loctime;
//
//   Get all field values for form fill-in
//
   fscanf(fp, "%s ", chname);                // Get Case #
   if (feof(fp)) return;
   fscanf(fp, "%s ", chname);                // Get Hypothesis
   fl_set_input(fd_bmcintel->itel_field[1], chname);
   fscanf(fp, "%s ", chname);                // Get Belief
   fscanf(fp, "%s ", chname);                // Get Disbelief

   for (i=4; i<FieldCount-9; i++) {
     fscanf(fp, "%s ", chname);
     fl_set_input(fd_bmcintel->itel_field[i], chname);
   }

   fscanf(fp, "%s ", chname);                // Get Unused
   fscanf(fp, "%s ", chname);                // Get Start Time
   fscanf(fp, "%s ", chname);                // Get End Time
   fscanf(fp, "%s ", chname);                // Get Duration
   fscanf(fp, "%s ", chname);                // Get Time Frame
   fscanf(fp, "%s ", chname);                // Get Info ID
   fl_set_input(fd_bmcintel->itel_field[48], chname);
   fscanf(fp, "%s ", chname);                // Get Preparer
   fl_set_input(fd_bmcintel->itel_field[49], chname);
   fscanf(fp, "%s ", chname);                // Get (or set) Prepared Date
   if (strlen(chname) < 2) {
     time(&clock);
     loctime = gmtime(&clock);
     str = asctime(loctime);
     fl_set_input(fd_bmcintel->itel_field[50], str);
   } else {
     fl_set_input(fd_bmcintel->itel_field[50], chname);
   }
   fscanf(fp, "%s\n", chname);               // Get Mission
   fl_set_input(fd_bmcintel->itel_field[51], chname);

   if (fl_get_button(fd_bmcintel->itel_rules) == PUSHED) {
     //
     //   Get hypothesis according to rules if possible
     //
   }

   return;
}

void ProcessField(FILE *fp, const char *str)
{
   if ((str == NULL) || (strcmp(str, " ") == 0)) {
     strcpy(iteltemp, "?");
   } else {
     strcpy(iteltemp, str);
     strsub(iteltemp, ' ', '_');
   }
   fprintf(fp, "%s ", iteltemp);

   return;
}

void itelbuttonCB(FL_OBJECT *ob, long data)
{
FILE            *outfp;
FILE            *grafp;
FILE            *lnkfp;
int             i, j, k, m;
int             noEOF = TRUE;
int             Vn;
int             n_Fields;
long            irc;
float           belief, disbelief, Tin, duration;
const char      *fname;
char            chtemp[1024];
char            chname[64];
char            chclass[64];

   switch (data) {
     case 0: // New
       //
       //   If 'New', allow 'Add', but not 'Append' or 'Browse'
       //
       fl_activate_object(fd_bmcintel->itel_add);
       fl_set_object_lcol(fd_bmcintel->itel_add, FL_GREEN);
       fl_deactivate_object(fd_bmcintel->itel_newfile);
       fl_set_object_lcol(fd_bmcintel->itel_newfile, FL_INACTIVE_COL);
       fl_deactivate_object(fd_bmcintel->itel_openfile);
       fl_set_object_lcol(fd_bmcintel->itel_openfile, FL_INACTIVE_COL);
       fl_deactivate_object(fd_bmcintel->itel_browse);
       fl_set_object_lcol(fd_bmcintel->itel_browse, FL_INACTIVE_COL);
       //
       //   Open a flat file to hold the data for Subdue
       //
       fname = fl_show_fselector("New file", "./", "*.tab", NULL);
       if ((itelfp = fopen(fname, "w+")) == NULL) {
	 fprintf(stderr, "Can't open new file %s for writing!\n", fname);
	 return;
       }

       strcpy(iteltemp, fl_get_choice_text(fd_bmcintel->itel_mission));
       strsub(iteltemp, ' ', '-');
       n_Fields = FieldCount;
       fprintf(itelfp, "%s  %d\n", iteltemp, n_Fields);

       ClearFields();

       for (i=0; i<FieldCount; i++) {
	 strcpy(iteltemp, FieldNames[i]);
	 strsub(iteltemp, ' ', '-');
	 fprintf(itelfp, "%s ", iteltemp);
       }
       fprintf(itelfp, "\n");
       break;

     case 1: // Open for append
       //
       //   If 'Append', allow 'Add', but not 'New' or 'Browse'
       //
       fl_activate_object(fd_bmcintel->itel_add);
       fl_set_object_lcol(fd_bmcintel->itel_add, FL_GREEN);
       fl_deactivate_object(fd_bmcintel->itel_newfile);
       fl_set_object_lcol(fd_bmcintel->itel_newfile, FL_INACTIVE_COL);
       fl_deactivate_object(fd_bmcintel->itel_openfile);
       fl_set_object_lcol(fd_bmcintel->itel_openfile, FL_INACTIVE_COL);
       fl_deactivate_object(fd_bmcintel->itel_browse);
       fl_set_object_lcol(fd_bmcintel->itel_browse, FL_INACTIVE_COL);
       //
       fname = fl_show_fselector("Append file", "./", "*.tab", NULL);
       if ((itelfp = fopen(fname, "r")) == NULL) return;

       fl_set_input(fd_itelfields->clu_classfn, fname);
       j = fl_get_choice(fd_bmcintel->itel_mission) - 1;

       if (Itel_filled == FALSE) {                     // If not already filled, load form fields
	 fscanf(itelfp, "%s", iteltemp);               // Get Mission domain name
	 fscanf(itelfp, "%d\n", &n_Fields);            // Get fields/record
	 fgets(iteltemp, 1024, itelfp);                // Get field names
	 FillField(itelfp);                            // Fill up the form with 1st record
       }
       fclose(itelfp);                                 // Close evidence file

       itelfp = fopen(fname, "a+");                    // Re-open for appending
       break;

     case 2: // Add
       ProcessField(itelfp, "0");
       strcpy(iteltemp, fl_get_input(fd_bmcintel->itel_field[1]));
       strsub(iteltemp, ' ', '_');
       ProcessField(itelfp, iteltemp);
       ProcessField(itelfp, fl_get_choice_text(fd_bmcintel->itel_belief));
       ProcessField(itelfp, fl_get_choice_text(fd_bmcintel->itel_disbelief));
       //
       for (i=4; i<FieldCount-9; i++) {
	 strcpy(iteltemp, fl_get_input(fd_bmcintel->itel_field[i]));
	 strsub(iteltemp, ' ', '_');
	 ProcessField(itelfp, iteltemp);
       }
       //
       fprintf(itelfp, " %d:%d:%d:%d:%d",
	       (int)fl_get_counter_value(fd_bmcintel->itel_start_year),
	       (int)fl_get_counter_value(fd_bmcintel->itel_start_mon),
	       (int)fl_get_counter_value(fd_bmcintel->itel_start_day),
	       (int)fl_get_counter_value(fd_bmcintel->itel_start_hour),
	       (int)fl_get_counter_value(fd_bmcintel->itel_start_min) );
       fprintf(itelfp, " %d:%d:%d:%d:%d",
	       (int)fl_get_counter_value(fd_bmcintel->itel_end_year),
	       (int)fl_get_counter_value(fd_bmcintel->itel_end_mon),
	       (int)fl_get_counter_value(fd_bmcintel->itel_end_day),
	       (int)fl_get_counter_value(fd_bmcintel->itel_end_hour),
	       (int)fl_get_counter_value(fd_bmcintel->itel_end_min) );
       fprintf(itelfp, " %d:%d:%d:%d:%d ",
	       (int)fl_get_counter_value(fd_bmcintel->itel_dur_year),
	       (int)fl_get_counter_value(fd_bmcintel->itel_dur_mon),
	       (int)fl_get_counter_value(fd_bmcintel->itel_dur_day),
	       (int)fl_get_counter_value(fd_bmcintel->itel_dur_hour),
	       (int)fl_get_counter_value(fd_bmcintel->itel_dur_min) );
       ProcessField(itelfp, fl_get_choice_text(fd_bmcintel->itel_tframe));
       //
       ProcessField(itelfp, "?");
       ProcessField(itelfp, fl_get_input(fd_bmcintel->itel_field[48]));
       ProcessField(itelfp, fl_get_input(fd_bmcintel->itel_field[49]));
       ProcessField(itelfp, fl_get_input(fd_bmcintel->itel_field[50]));
       strcpy(iteltemp, fl_get_input(fd_bmcintel->itel_field[51]));
       strsub(iteltemp, ' ', '_');
       ProcessField(itelfp, iteltemp);
       //
       fprintf(itelfp, "\n");
       break;

     case 3: // First Record
       rewind(itelfp);
       fscanf(itelfp, "%s", iteltemp);                 // Get Mission domain name
       fscanf(itelfp, "%d\n", &n_Fields);              // Get fields/record
       fgets(iteltemp, 1024, itelfp);                  // Get field names
       FillField(itelfp);                              // Fill up the form with 1st field
       break;

     case 4: // Apply
       fname = fl_show_fselector("Evidence File", "./DSBFiles", "*.evid", NULL);
       if (fname == NULL) return;
       if ((outfp = fopen(fname, "a")) == NULL) return;
       //
       duration = fl_get_counter_value(fd_bmcintel->itel_dur_min) +
	          fl_get_counter_value(fd_bmcintel->itel_dur_hour)*60.0 +
	          fl_get_counter_value(fd_bmcintel->itel_dur_day)*24.0*60.0 +
	          fl_get_counter_value(fd_bmcintel->itel_dur_mon)*30.0*24.0*60.0 +
	          fl_get_counter_value(fd_bmcintel->itel_dur_year)*12.0*30.0*24.0*60.0;

       i = fl_get_choice(fd_bmcintel->itel_belief) - 1;
       belief = LexTable[i].lower + (LexTable[i].upper - LexTable[i].lower)/2.0;
       i = fl_get_choice(fd_bmcintel->itel_disbelief) - 1;
       disbelief = LexTable[i].lower + (LexTable[i].upper - LexTable[i].lower)/2.0;

       strcpy(chname, fl_get_choice_text(fd_bmcintel->itel_source));
       strsub(chname, ' ', '_');

       Tin = (float)drand48()*100.0;
       //
       fprintf(outfp, "%4d %-24s %6.3f %6.3f %10.1f %c  %5.1f %c  %s  %s  %s  %-20s %4d %4d %-64s\n",
	       0,                                           // Evidence type
	       chname,                                      // Node name
	       belief,                                      // Belief
	       disbelief,                                   // Disbelief
	       Tin,                                         // Evidence time
	       'M',                                         // Time units
	       duration,                                    // Duration
	       'M',                                         // Duration units
	       fl_get_input(fd_bmcintel->itel_field[25]),   // Comment 1
	       fl_get_input(fd_bmcintel->itel_field[26]),   // Comment 2
	       fl_get_input(fd_bmcintel->itel_field[27]),   // Comment 3
	       "Intel_Processor",                           // User supplied description
	       0,                                           // External node #
	       0,                                           // External level #
	       "None");                                     // External network file name
       /*
       GetLexical(belief, chtemp);
       GetLexical(disbelief, chtmp);
       sprintf(iteltemp, "%11s (%4.1f)  %11s (%4.1f)",
	       chtemp, belief*100.0,
	       chtmp, disbelief*100.0);
       strncpy(chsource, evidences[i].chsource, 15);
       chsource[15] = '\0';                                 // Force NULL, in case not there
       sprintf(chtemp, "%-16s %-34s %8.2f %8.2f %10s %10s %10s",
	       chsource, dsbtemp, Tin, duration,
	       evidences[i].latitude, evidences[i].longitude, evidences[i].altitude);
       fl_addto_browser(fd_assess->input_browser, chtemp);
       */
       fclose(outfp);
       //
       //   Make sure evidence is in ascending time order
       //
       sprintf(iteltemp, "mv %s unsorted.temp", fname);
       system(iteltemp);
       sprintf(iteltemp, "sort -k 5 -g -o %s unsorted.temp", fname);
       system(iteltemp);
       sprintf(iteltemp, "rm unsorted.temp");
       system(iteltemp);
       break;

     case 5: // Browse
       //
       //   If 'Browse', do not allow 'Add', 'New', or 'Append'
       //
       fl_deactivate_object(fd_bmcintel->itel_add);
       fl_set_object_lcol(fd_bmcintel->itel_add, FL_INACTIVE_COL);
       fl_deactivate_object(fd_bmcintel->itel_newfile);
       fl_set_object_lcol(fd_bmcintel->itel_newfile, FL_INACTIVE_COL);
       fl_deactivate_object(fd_bmcintel->itel_openfile);
       fl_set_object_lcol(fd_bmcintel->itel_openfile, FL_INACTIVE_COL);
       fl_deactivate_object(fd_bmcintel->itel_browse);
       fl_set_object_lcol(fd_bmcintel->itel_browse, FL_INACTIVE_COL);
       fl_activate_object(fd_bmcintel->itel_firstrec);
       fl_set_object_lcol(fd_bmcintel->itel_firstrec, FL_GREEN);
       fl_activate_object(fd_bmcintel->itel_nextrec);
       fl_set_object_lcol(fd_bmcintel->itel_nextrec, FL_GREEN);
       //
       fname = fl_show_fselector("Browse file", "./", "*.tab", NULL);
       if ((itelfp = fopen(fname, "r")) == NULL) {
	 fprintf(stderr, "Can't open file %s for browsing!\n", fname);
	 return;
       }
       //
       fscanf(itelfp, "%s", iteltemp);                 // Get Mission domain name
       fscanf(itelfp, "%d\n", &n_Fields);              // Get fields/record
       fgets(iteltemp, 1024, itelfp);                  // Get field names
       FillField(itelfp);                              // Fill up the form with 1st record
       break;

     case 6: // Next
       FillField(itelfp);                              // Fill up the form with next record
       break;

     case 7: // Classify
       //fl_set_button(fd_itelfields->clu_classify, PUSHED);

       for (i=0; i<FieldCount-4; i++) {
	 fl_set_object_label(fd_itelfields->itel_fldselect[i], FieldNames[i]);
	 if (strlen(FieldNames[i]) > 1)
           fl_set_button(fd_itelfields->itel_fldselect[i], PUSHED);
       }

       fl_clear_browser(fd_itelfields->clu_classes);
       fl_set_input(fd_itelfields->clu_closest, " ");
       fl_show_form(fd_itelfields->itelfields, FL_PLACE_CENTER,FL_FULLBORDER,
		    "Field Select");
       break;

     case 8: // Clear
       ClearFields();
       break;

     case 9: // Close
       fclose(itelfp);

       fl_deactivate_object(fd_bmcintel->itel_add);
       fl_set_object_lcol(fd_bmcintel->itel_add, FL_INACTIVE_COL);
       fl_activate_object(fd_bmcintel->itel_newfile);
       fl_set_object_lcol(fd_bmcintel->itel_newfile, FL_GREEN);
       fl_activate_object(fd_bmcintel->itel_openfile);
       fl_set_object_lcol(fd_bmcintel->itel_openfile, FL_GREEN);
       fl_activate_object(fd_bmcintel->itel_browse);
       fl_set_object_lcol(fd_bmcintel->itel_browse, FL_GREEN);
       fl_deactivate_object(fd_bmcintel->itel_firstrec);
       fl_set_object_lcol(fd_bmcintel->itel_firstrec, FL_INACTIVE_COL);
       fl_deactivate_object(fd_bmcintel->itel_nextrec);
       fl_set_object_lcol(fd_bmcintel->itel_nextrec, FL_INACTIVE_COL);
       break;

     default:
       break;
   }

   return;
}

void itelmissionCB(FL_OBJECT *ob, long data)
{
FILE            *outfp;
int             i, j, k, m;
const char      *fname;
char            chtemp[128];
char            chname[64];
char            chclass[64];
//
//   Get all node names for the selected mission
//
   if (data == 1) 
     fl_set_input(fd_bmcintel->itel_field[51],
		  fl_get_choice_text(fd_bmcintel->itel_mission));
  
   j = fl_get_choice(fd_bmcintel->itel_mission) - 1;

   if (strcmp(iteltypes[j].filebase, "*") == 0) {
     strcpy(chtemp, fl_show_fselector("Node file", "DSBFiles/", "*.dsbn", NULL));
     if (strlen(chtemp) == 0) return;
   } else {
     strcpy(chtemp, "DSBFiles/");
     strcat(chtemp, iteltypes[j].filebase);
     strcat(chtemp, ".dsbn");
   }

   fl_clear_choice(fd_bmcintel->itel_source);

   if ((outfp = fopen(chtemp, "r")) == NULL) return;
   fgets(iteltemp, 128, outfp);
   fscanf(outfp, "%d %s %*d %*f %*f %*s", &k, chclass);
   for (i=0; i<k; i++) {
     fscanf(outfp, "%d %s", &m, chclass);
     for (j=0; j<m; j++) {
       fscanf(outfp, "%s %*s %*f %*f", chname);
       strsub(chname, '_', ' ');
       fl_addto_choice(fd_bmcintel->itel_source, chname);
    }
   }
   fclose(outfp);
}
