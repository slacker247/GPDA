#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <math.h>
#include <time.h>
#include <sys/types.h>

#include "Globals.h"

#include "forms.h"
#include "TGTforms.h"

#define GREEN   0
#define YELLOW  1
#define RED     2

/* --------------------------------------------------------------------- */

typedef struct {
  int           Value;                     // Importance (1 = critical, 4 = low)
  int           Class;                     // Missile, Air base, Sub Base, ...
  int           Mobile;                    // Is target mobile? (1 = yes)
  int           Stat;                      // Target status 
  float         Hard;                      // Target Hardness (0.0 - 1.0)
  float         Weight;                    // Weighted value for rDE calculation
  float         Latitude;                  // Target latitude (decimal degrees)
  float         Longitude;                 // Target Longitude (decimal degrees)
  char          Name[32];                  // Name assigned to target
  char          Locale[32];                // Locale of target
} TGT;

typedef struct {
  int           SiteType[15];              // Type of sites at locale (with counts)
  float         Latitude;                  // Locale latitude (decimal degrees)
  float         Longitude;                 // Locale Longitude (decimal degrees)
  char          Locale[32];                // Name of Site Locale
  char          Country[24];               // Country of locale 
} SITE;

/* --------------------------------------------------------------------- */

int             TGTwinX, TGTwinY;
int             TGTwinW, TGTwinH;
char            TGTlabel[32];

int             n_locales;
int             n_targets;
char            *tgtBITMAPDIR;
char            country[64];
char            tgttemp[128];
char            blankxpm[128];
SITE            locales[100];
TGT             targets[200];

char            fnames[11][16] = { "Missile", "Missile",  "Airbase",    "Subbase",
                                   "Nuclear", "Chemical", "Biological", 
                                   "Comm",    "City",     "TCT",
                                   "blank" };

FD_targetstatus *fd_targetstatus;

/* --------------------------------------------------------------------- */

void        TGTinit();
void        TGTshow(int xpos, int ypos, int width, int height, Window mainwinID);
extern int  EraseActiveEntry(char *text);
extern int  StoreActiveEntry(char *text);
extern int  FinishUp();

/*                                                                       */
/* --------------------------------------------------------------------- */
/*              M A I N   P R O G R A M   S T A R T S   H E R E          */
/* --------------------------------------------------------------------- */
/*                                                                       */
void TGTinit()
{
int             i;

   fd_targetstatus = create_form_targetstatus();

   strcpy(TGTlabel, "Target Assessment");

   strcpy(country, fl_get_choice_text(fd_targetstatus->country_menu));
   //targetselectCB(fd_targetstatus->target_select[0], 0L);

   if ((tgtBITMAPDIR=getenv("BITMAPDIR")) == NULL) {
        tgtBITMAPDIR = "../BitMaps";
   }

   fl_set_browser_fontstyle(fd_targetstatus->tgt_active, FL_FIXED_STYLE|FL_BOLD_STYLE);

   sprintf (blankxpm, "%s/%s.xpm", tgtBITMAPDIR, "blank"); 
   for (i=0; i<10; i++) {
      fl_show_object(fd_targetstatus->tgt_pixmap[i]);
      fl_free_pixmap_pixmap(fd_targetstatus->tgt_pixmap[i]);
      fl_set_pixmap_file(fd_targetstatus->tgt_pixmap[i], blankxpm);
   }

   return;
}

int TGTclose(FL_FORM *form, void *data)
{
   TGTexitCB(NULL, 0);

   return(0);
}

void TGTshow(int xpos, int ypos, int width, int height, Window mainwinID)
{
Window          winid;

   if(!fl_form_is_visible(fd_targetstatus->targetstatus) ) {
      fl_transient();
      fl_winposition(xpos, ypos);
      fl_initial_winsize(width, height);
      winid = fl_prepare_form_window(fd_targetstatus->targetstatus,
                                     FL_PLACE_POSITION,FL_TRANSIENT, "Target-Assess");
      fl_winreshape(winid, xpos, ypos, width, height);
      fl_get_wingeometry(winid, &TGTwinX, &TGTwinY, &TGTwinW, &TGTwinH);
      fl_show_form_window(fd_targetstatus->targetstatus);
      fl_set_form_atclose(fd_targetstatus->targetstatus, TGTclose, 0);
      StoreActiveEntry("Target-Assess");
   }

   return;
}
/*                                                                       */
/* --------------------------------------------------------------------- */
/*      C A L L B A C K    R O U T I N E S    S T A R T    H E R E       */
/* --------------------------------------------------------------------- */
/*                                                                       */
void TGTexitCB(FL_OBJECT *ob, long data)
{
   fl_hide_form(fd_targetstatus->targetstatus);
   EraseActiveEntry("Target-Assess");

   FinishUp();

   return;
}

void TGTnoneCB(FL_OBJECT *ob, long data)
{

   return;
}

void targetselectCB(FL_OBJECT *ob, long data)
{
FILE            *sitefp;
int             item = (int)data;
int             i, j, k, t;
int             ideglat, iminlat, ideglon, iminlon;
int             mobile, value, stat;
float           hard;
char            chtgt[32];

   item = (int)data;
   switch (item) {
     case 0: // Country
       strcpy(country, fl_get_choice_text(fd_targetstatus->country_menu));
       fl_set_object_label(fd_targetstatus->tgt_country, country);
       break;

     case 1: // Class
       fl_set_object_label(fd_targetstatus->tgt_locale, " ");
       fl_set_object_label(fd_targetstatus->target_sites,
                           fl_get_choice_text(fd_targetstatus->class_menu));

       fl_clear_browser(fd_targetstatus->site_browser);
       fl_clear_browser(fd_targetstatus->target_browser); 

       for (i=0; i<10; i++) {
          fl_free_pixmap_pixmap(fd_targetstatus->tgt_pixmap[i]);
          fl_set_pixmap_file(fd_targetstatus->tgt_pixmap[i], blankxpm);
       }

       t = -1;
       i = -1;
       sprintf(tgttemp, "Targets_%s.dat", country);
       if ((sitefp = fopen(tgttemp, "r")) != NULL) {
          do fgets(tgttemp, 128, sitefp); while ((tgttemp[0] == '#') && !feof(sitefp));
          while (!feof(sitefp)) {
	     //   Load and display the locale information 
             do fgets(tgttemp, 128, sitefp); while ((tgttemp[0] == '#') && !feof(sitefp));
	     i = i + 1;
             sscanf(tgttemp, "%s %d %d %d %d %d %d %d %d %d %d %d %d %d %d",
                locales[i].Locale, &ideglat, &iminlat, &ideglon, &iminlon,
                &locales[i].SiteType[1],  &locales[i].SiteType[2], &locales[i].SiteType[3],
		&locales[i].SiteType[4],  &locales[i].SiteType[5], &locales[i].SiteType[6],
		&locales[i].SiteType[7],  &locales[i].SiteType[8], &locales[i].SiteType[9],
                &locales[i].SiteType[10] );
	     locales[i].SiteType[0] = 0;
             locales[i].Latitude    = (float)ideglat + (float)iminlat/60.0;
	     locales[i].Longitude   = (float)ideglon + (float)iminlon/60.0;
	     strcpy(locales[i].Country, country);
             if (fl_get_choice(fd_targetstatus->class_menu) == 12)
                fl_add_browser_line(fd_targetstatus->site_browser, locales[i].Locale);
             else if (locales[i].SiteType[fl_get_choice(fd_targetstatus->class_menu)-1] != 0)
                fl_add_browser_line(fd_targetstatus->site_browser, locales[i].Locale);
	     //   Load the target information
             for (j=1; j<11; j++) {
	       if (locales[i].SiteType[j] > 0) {
	         for (k=0; k<locales[i].SiteType[j]; k++) {
		   do fgets(tgttemp, 128, sitefp); while ((tgttemp[0] == '#'));
		   sscanf(tgttemp, "%s %d %d %d %d %f %d %d %d", chtgt, 
                          &ideglat, &iminlat, &ideglon, &iminlon,
                          &hard, &mobile, &value, &stat);
		   t = t+1;
		   targets[t].Hard      = hard;
		   targets[t].Class     = j;
		   targets[t].Mobile    = mobile;
		   targets[t].Value     = value;
		   targets[t].Weight    = 0.0;
		   targets[t].Stat      = stat;
                   targets[t].Latitude  = (float)ideglat + (float)iminlat/60.0;
	           targets[t].Longitude = (float)ideglon + (float)iminlon/60.0;
		   strcpy(targets[t].Name, chtgt);
		   strcpy(targets[t].Locale, locales[i].Locale);
	         }
	       }
	     }
          }
          n_locales = i + 1;
	  n_targets = t + 1;
          fclose(sitefp);
       } else {
          fl_show_messages("Error: Unable to open Target file");
       }
       break;

     default:
       break;
   }   
}

void localebrowserCB(FL_OBJECT *ob, long data)
{
int             item = (int)data;
int             i, itype;
const char      *chline;

   item = fl_get_browser(fd_targetstatus->site_browser);
   chline = fl_get_browser_line(fd_targetstatus->site_browser, item);

   for (i=0; i<10; i++) {
      fl_free_pixmap_pixmap(fd_targetstatus->tgt_pixmap[i]);
      fl_set_pixmap_file(fd_targetstatus->tgt_pixmap[i], blankxpm);
   }

   fl_clear_browser(fd_targetstatus->target_browser); 
   fl_set_input(fd_targetstatus->tgt_latitude, " ");
   fl_set_input(fd_targetstatus->tgt_longitude, " ");
   fl_set_object_label(fd_targetstatus->tgt_type, " ");
   fl_set_input(fd_targetstatus->tgt_name, " ");
   fl_set_thumbwheel_bounds(fd_targetstatus->tgt_sethard, 0.0, 1.0);
   fl_set_thumbwheel_value(fd_targetstatus->tgt_sethard, (double)0.0);
   fl_set_object_label(fd_targetstatus->tgt_hardness, "0.0");
   //
   //   Find which locale selected
   //
   for (i=0; i<n_locales; i++)
     if (strcmp(chline, locales[i].Locale) == 0) { item = i; break; }
   //
   //   Fill in display
   //
   fl_set_object_label(fd_targetstatus->tgt_locale, locales[item].Locale);
   fl_set_object_label(fd_targetstatus->tgt_country, locales[item].Country);
   /*
   sprintf(tgttemp, "%8.4f %c", fabs(locales[item].Latitude),
                                (locales[item].Latitude>=0.0 ? 'N' : 'S'));
   fl_set_object_label(fd_targetstatus->tgt_latitude, tgttemp);
   sprintf(tgttemp, "%8.4f %c", fabs(locales[item].Longitude),
                                (locales[item].Longitude>=0.0 ? 'E' : 'W'));
   fl_set_object_label(fd_targetstatus->tgt_longitude, tgttemp);
   */
   //
   //   Display target class icons
   //
   for (i=0; i<10; i++) {
      if (locales[item].SiteType[i] != 0) {
         fl_free_pixmap_pixmap(fd_targetstatus->tgt_pixmap[i]);
         sprintf (tgttemp, "%s/%s.xpm", tgtBITMAPDIR, fnames[i]);
         fl_set_pixmap_file(fd_targetstatus->tgt_pixmap[i], tgttemp);
      }
   }
   //
   //   Find and display targets at locale and/or class
   //
   itype = fl_get_choice(fd_targetstatus->class_menu)-1;
   for (i=0; i<n_targets; i++)
     if ((strcmp(locales[item].Locale, targets[i].Locale) == 0) && 
          targets[i].Class == itype) {
       //if (targets[i].Stat == YELLOW) strcpy(tgttemp, "@C3");
       //else if (targets[i].Stat == RED) strcpy(tgttemp, "@C1");
       //else strcpy(tgttemp, "@C2");
        strcpy(tgttemp, targets[i].Name);
        fl_add_browser_line(fd_targetstatus->target_browser, tgttemp);
     }
}

void targetbrowserCB(FL_OBJECT *ob, long data)
{
int             item = (int)data;
int             i;
const char      *chline;

   item = fl_get_browser(fd_targetstatus->target_browser);
   chline = fl_get_browser_line(fd_targetstatus->target_browser, item);

   for (i=0; i<n_targets; i++)
     if (strcmp(chline, targets[i].Name) == 0) { item = i; break; }

   sprintf(tgttemp, "%8.4f", fabs(targets[item].Latitude));
   fl_set_input(fd_targetstatus->tgt_latitude, tgttemp);
   sprintf(tgttemp, "%8.4f", fabs(locales[item].Longitude));
   fl_set_input(fd_targetstatus->tgt_longitude, tgttemp);

   fl_set_object_label(fd_targetstatus->tgt_type,
                           fl_get_choice_text(fd_targetstatus->class_menu));

   fl_set_choice(fd_targetstatus->tgt_mobility, targets[item].Mobile);
   fl_set_choice(fd_targetstatus->tgt_value, targets[item].Value);

   fl_set_input(fd_targetstatus->tgt_name, targets[i].Name);

   fl_set_thumbwheel_bounds(fd_targetstatus->tgt_sethard, 0.0, 1.0);
   fl_set_thumbwheel_value(fd_targetstatus->tgt_sethard, (double)targets[item].Hard);
   sprintf(tgttemp, "%f", targets[item].Hard);
   fl_set_object_label(fd_targetstatus->tgt_hardness, tgttemp);
}

void tgttypeCB(FL_OBJECT *ob, long data)
{
   fl_set_object_label(fd_targetstatus->tgt_type, fnames[(int)data]);
}

void tgtactiveCB(FL_OBJECT *ob, long data)
{
float           lat, lon;
int             i, item, line;
char            chline[120];
const char      *casefilename;
FILE            *casefp;
extern char     *strsub(char *istr, char och, char nch);

   item = (int)data;
   switch (item) {
     case 0: // Delete
       line = fl_get_browser(fd_targetstatus->tgt_active);
       if (line > 0)
          fl_delete_browser_line(fd_targetstatus->tgt_active, line);
       break;

     case 1: // Add
       sscanf(fl_get_input(fd_targetstatus->tgt_latitude),  "%f", &lat);
       sscanf(fl_get_input(fd_targetstatus->tgt_longitude), "%f", &lon);
       sprintf(chline, "%-16s %-16s  %8.3f  %8.3f  %5d  %5d  %6.4f  %5d",
           fl_get_input(fd_targetstatus->tgt_name),
           fl_get_object_label(fd_targetstatus->tgt_locale),
           lat, lon,
	   0, /*fl_get_object_label(fd_targetstatus->tgt_type),*/
           fl_get_choice(fd_targetstatus->tgt_value),
           fl_get_thumbwheel_value(fd_targetstatus->tgt_sethard),
           fl_get_choice(fd_targetstatus->tgt_mobility)-1 );
       fl_addto_browser(fd_targetstatus->tgt_active, chline);
       break;

     case 2: // Save
       casefilename = fl_show_fselector("Active Targets", "./ActiveTargets.dat", "*", NULL);
       if (casefilename != NULL) {
         if ((casefp = fopen(casefilename, "w")) != NULL) {
           line = fl_get_browser_maxline(fd_targetstatus->tgt_active);
           fprintf(casefp, "%5d\n", line);
           for (i=1; i<=line; i++) {
	     fprintf(casefp, "%s\n", fl_get_browser_line(fd_targetstatus->tgt_active, i));
           }
           fclose(casefp);
	 }
       } else {
          fl_show_messages("Error: Unable to open Active Targets file");
       }
       break;

     case 3: // Load
       casefilename = fl_show_fselector("Active Targets", "./ActiveTargets.dat", "*", NULL);
       if (casefilename != NULL) {
         if ((casefp = fopen(casefilename, "r")) != NULL) {
           do fgets(tgttemp, 128, casefp); while (tgttemp[0] == '#');
           sscanf(tgttemp, "%d", &line);
           while (1) {
             do fgets(tgttemp, 128, casefp); while (tgttemp[0] == '#');
             if (feof(casefp)) break;
             strsub(tgttemp, '\n', ' ');
             fl_addto_browser(fd_targetstatus->tgt_active, tgttemp);
           }
           fclose(casefp);
	 }
       } else {
          fl_show_messages("Error: Unable to open Active Targets file");
       }
       break;

     default:
       break;
   }
}

void tgthardCB(FL_OBJECT *ob, long data)
{
char            chline[16];

   double factor = fl_get_thumbwheel_value(fd_targetstatus->tgt_sethard);
   sprintf(chline, "%f", factor);
   fl_set_object_label(fd_targetstatus->tgt_hardness, chline);
}

void targetcanvasCB(FL_OBJECT *ob, long data)
{
  /* fill-in code for callback */
}

void targetstatusCB(FL_OBJECT *ob, long data)
{
  /* fill-in code for callback */
}

void arrivalCB(FL_OBJECT *ob, long data)
{
  /* fill-in code for callback */
}

void damageCB(FL_OBJECT *ob, long data)
{
  /* fill-in code for callback */
}


