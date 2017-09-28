#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <math.h> 
#include <time.h>
#include <signal.h>
#include <ctype.h>
#include <sys/ioctl.h>
#include <unistd.h>

#include "Globals.h"
#include "forms.h"
#include "CBPforms.h"

#define    FALSE         0
#define    TRUE          1
#define    LOW           0
#define    MEDIUM        1
#define    HIGH          2
#define    WEIGHT        3
#define    MUSTHAVE      4
#define    CANNOT        5
#define    DEFAULT_LOW   0.2
#define    DEFAULT_MED   0.5
#define    DEFAULT_HIGH  0.8

/* --------------------------------------------------------------------- */

typedef struct {
  int        Enabled;                             // Indice "enabled"
  int        Weighting;                           // Indice Weighting method
  int        Category;                            // Strategy, Threat, Asset
  int        Class;                               // Sub-level class
  int        n_constraints;                       // No. of constraints for this indice
  int        n_synonyms;                          // No. of synonyms for this indice
  float      Weight;                              // Indice Weight
  char       Name[40];                            // Descriptive text for indice
  char       CaseKeys[8];                         // Flag for matching keyword in cases
  char       **Constraints;                       // Constraints
  char       **Synonyms;                          // Synonyms
} INDICES;

typedef struct {
  int        Active;                              // Selected for inclusion in ActiveOptions file
  int        Value;                               // Importance (1 = critical, 4 = low)
  int        Class;                               // Missile, Air base, Sub Base, ...
  float      SVbr;                                // Probability of Pre-launch survivability
  float      ARbr;                                // Probability of Arrival
  float      DMbr;                                // Probability of Damage
  float      SVrb;                                // Probability of Pre-launch survivability
  float      ARrb;                                // Probability of Arrival
  float      DMrb;                                // Probability of Damage
  float      rDE;                                 // Estimated rDE
  float      Weight;                              // Weighted value      
  char       Weapon[4];                           // (C)onventional, (N)uclear, ...
  char       Arena[16];                           // Tactical | Strategic
  char       Type[16];                            // Preempt, Deter, Deny, Retailiate, ...
  char       Where[32];                           // Target locale name
  char       Option[64];                          // Response Option name
  char       COA[64];                             // Parent COA id
} DEFACTORS;

/* --------------------------------------------------------------------- */

int             COAwinX, COAwinY;
int             COAwinW, COAwinH;
int             COAinited = 0;
char            COAlabel[32];
Window          COAwinid;

char            *SEARCH;
char            CaseID[128];                      // Case ID
char            CaseOrder[128];                   // JCS Warning Order File Name
char            CaseExecute[128];                 // JCS Execute Order File Name
char            CaseEstimate[128];                // Commander's Estimate File Name
char            CaseDesc[128];                    // Case Description
char            CasePrep[32];                     // Case Preparer
char            CaseBilt[32];                     // Date case built
char            CaseLast[32];                     // Date case last used
int             CaseUsed = 0;                     // No. of times case used
int             CasePercent = 0;                  // Percent of indices addressed
int             CaseNhdr = 12;                    // No. of lines of Header
float           CaseLow = 0.2;                    // Default 'Low' weight
float           CaseMed = 0.5;                    // Default 'Medium' weight
float           CaseHigh = 0.8;                   // Default 'High' weight
char            CaseName[8][64];
int             CaseMatch[8];

int             CatCount;
int             CatStart[20];

int             n_JcsWo, OldJCSn, OldCOAn[3];
float           OldJCSPercent[10], OldCOAPercent[10][3];
char            OldJCSOrder[10][64];
char            OldCOAEst[10][3][64];

int             RespCount;
int             RespActive;
DEFACTORS       RespOptions[60];
char            RespClass[11][24] = { "Not Assigned", 
                         "Missile Site", "Air Base", "Sub Base", "Nuclear Facility",
                         "Chemical Facility", "Biological Facility", "C&C Facility",
                         "Population Center", "Time Critical Target", "Other" };
char            RespWeight[5][12] = { "None", "Low", "Medium", "High", "Critical" };

int             Indice;                           // Criteria Index
int             KeyCounts[5][5];
int             KeyActive[5][5];
int             KeyMatchs[5][5];
int             TotalActive, TotalMatchs[8];
char const      *Indicetxt;
int             WeightMethod[6] = { LOW, MEDIUM, HIGH, WEIGHT, MUSTHAVE, CANNOT };
INDICES         *indices;

int             n_cats = 3;
int             n_menus;
int             n_keys;
int             n_words;
int             n_class[5];
int             BaseGeneral;
int             BaseThreats;
int             BaseAssets;
int             MenuGeneral = 0;
int             MenuThreats = 0;
int             MenuAssets  = 0;
int             SubGeneral[5] = { 0, 0, 0, 0, 0 };
int             SubThreats[5] = { 0, 0, 0, 0, 0 };
int             SubAssets[5]  = { 0, 0, 0, 0, 0 };

char            cbptemp[128];
char            cmdopts[2400];

FD_cbpinput     *fd_cbpinput;
FD_caseinfo     *fd_caseinfo;
FD_cbpview      *fd_cbpview;
FD_cbpcoaview   *fd_cbpcoaview;
FD_cbpmapview   *fd_cbpmapview;
FD_cbprdeinput  *fd_cbprdeinput;
FD_cbprdehelp   *fd_cbprdehelp;
FD_cbprdetext   *fd_cbprdetext;
FD_cbpplan      *fd_cbpplan;

/* --------------------------------------------------------------------- */

extern char     UserName[48];

int  COAinit();
void COAshow(int xpos, int ypos, int width, int height, Window winid, char *fn);
void COAJcsExec();

void LoadKeys(FILE *casefp);
void ShowKeys();
void LoadCOAs(FILE *casefp);
void SaveCase(char* filename, int nwords);

extern int  EraseActiveEntry(char *text);
extern int  StoreActiveEntry(char *text);
extern int  FinishUp();
extern char *strsub(char *istr, char och, char nch);
extern char *strtrm(char *strin);

/*                                                                       */
/* --------------------------------------------------------------------- */
/*              M A I N   P R O G R A M   S T A R T S   H E R E          */
/* --------------------------------------------------------------------- */
/*                                                                       */
int COAinit()
{
FILE            *fp;
int             i;

   if (COAinited) return(0);

   SEARCH = getenv("SEARCH");

   fd_cbpinput    = create_form_cbpinput();
   fd_caseinfo    = create_form_caseinfo();
   fd_cbpview     = create_form_cbpview();
   fd_cbpcoaview  = create_form_cbpcoaview();
   fd_cbpmapview  = create_form_cbpmapview();
   fd_cbprdeinput = create_form_cbprdeinput();
   fd_cbprdehelp  = create_form_cbprdehelp();
   fd_cbprdetext  = create_form_cbprdetext();
   fd_cbpplan     = create_form_cbpplan();

   strcpy(COAlabel, "COA-Planning");

   //fl_hide_object(fd_cbpinput->cbp_save);
   fl_hide_object(fd_cbpinput->cbp_exec);
   fl_hide_object(fd_cbpinput->cbp_view);
   fl_hide_object(fd_cbpinput->view_order);
   fl_deactivate_object(fd_cbpinput->view_order);

   fl_clear_menu(fd_cbpinput->jcs_orders);

   fl_set_browser_fontsize(fd_cbprdeinput->ro_browser, 10);
   fl_set_browser_fontstyle(fd_cbprdeinput->ro_browser, FL_FIXED_STYLE);

   indices = new INDICES[500];

   COAinited = TRUE;
   fp = fopen("GPDAinfo.log", "a");
   fprintf(fp, "Case Based Planning algorithm initialization complete.\n");
   fclose(fp);

   return(0);
}

int COAclose(FL_FORM *form, void *data)
{
   COAexitCB(NULL, 0);

   return(0);
}

void COAshow(int xpos, int ypos, int width, int height, Window mainwinID, char *fn)
{
int             i;

   if(!fl_form_is_visible(fd_cbpinput->cbpinput) ) {
      fl_transient();
      fl_winposition(xpos, ypos);
      fl_initial_winsize(width, height);
      COAwinid = fl_prepare_form_window(fd_cbpinput->cbpinput,
                                     FL_PLACE_POSITION,FL_TRANSIENT, "Planning-Input");
      fl_winreshape(COAwinid, xpos, ypos, width, height);
      fl_get_wingeometry(COAwinid, &COAwinX, &COAwinY, &COAwinW, &COAwinH);
      COAwinX = COAwinX + 4;
      COAwinY = COAwinY + 24;
      fl_show_form_window(fd_cbpinput->cbpinput);
      fl_set_form_atclose(fd_cbpinput->cbpinput, COAclose, 0);
      StoreActiveEntry("Planning-Input");

      fl_clear_menu(fd_cbpinput->jcs_orders);
      //fl_addto_menu(fd_cbpinput->jcs_orders, fn);
      if (strcmp(getenv("GPDADOMAIN"), "iow") == 0)
	fl_addto_menu(fd_cbpinput->jcs_orders, "IDS-Attack-1234.5");
      else
	fl_addto_menu(fd_cbpinput->jcs_orders, fn /*"JCS-Order-1437.5"*/);
   }

   return;
}

void CBPexitCB(FL_OBJECT *ob, long data)
{

   fl_hide_form(fd_cbpinput->cbpinput);
   EraseActiveEntry("Planning-Input");

   FinishUp();

   return;
}

void CBPnoneCB(FL_OBJECT *object, long item_no)
{
int item = 0;

}
/*                                                                       */
/* --------------------------------------------------------------------- */
/*          S U P P O R T   R O U T I N E S   S T A R T   H E R E        */
/* --------------------------------------------------------------------- */
/*                                                                       */
void COAJcsExec()
{
FILE            *coafp;
char            filename[128];

   fl_show_object(fd_cbpinput->cbp_view);
}

void LoadKeys(FILE *casefp)
{
int             i, j, k, n, m, outer, Ii, il;
int             n_constrain, n_synon, mode, enabled;
int             n_items, n_matches;
int             pupID = -1, itemno;
int             p_match;
float           docno;
float           weight;
char            *ordername;
char            filename[128];
char            chconstrain[32];
char            chsynonym[32];
char            topmenu0[32];
char            chtmp[8];
char            chitem[40];
char            chtext[40];

   for (i=0; i<500; i++) {
     strcpy(indices[i].Name, " ");
     indices[i].Enabled       = 0;
     indices[i].Weighting     = 0;
     indices[i].Weight        = 1.0;
     indices[i].n_constraints = 0;
     indices[i].n_synonyms    = 0;
     indices[i].Constraints   = NULL;
     indices[i].Synonyms      = NULL;
   }

   do fgets(cbptemp, 128, casefp); while (cbptemp[0] == '#');
   sscanf(cbptemp, "%d", &n_cats);

   CatCount = n_cats;
   fl_clear_choice(fd_cbpinput->category);
   fl_clear_choice(fd_cbpplan->cattree);
   Ii = -1;
   for (outer=0; outer<n_cats; outer++) {
       //
       //   Load the Indice info
       //
       do fgets(cbptemp, 128, casefp); while (cbptemp[0] == '#');
       sscanf(cbptemp, "%s %d", topmenu0, &n_menus);
       n_class[outer] = n_menus;
       fl_addto_choice(fd_cbpinput->category, topmenu0);
       fl_addto_choice(fd_cbpplan->cattree, topmenu0);
       if (outer < 3) fl_set_object_label(fd_cbpinput->cbp_cats[outer], topmenu0);
       Ii = Ii + 1;
       strcpy(indices[Ii].Name, topmenu0);
       indices[Ii].Weighting     = 0;
       indices[Ii].Weight        = 1.0;
       indices[Ii].Category      = outer;
       indices[Ii].Class         = outer;
       indices[Ii].Enabled       = -(outer+1);
       indices[Ii].n_constraints = 0;
       indices[Ii].n_synonyms    = 0;
       CatStart[outer] = Ii;

       for (n=0; n<n_menus; n++) {
          do fgets(cbptemp, 128, casefp); while (cbptemp[0] == '#');       
          sscanf(cbptemp, "%16s %d", topmenu0, &n_items);
	  switch (outer) {
	  case 0:
	    if (n < 4) fl_set_object_label(fd_cbpinput->cbp_goallist[n], topmenu0);
	    fl_set_object_label(fd_cbpcoaview->coa_class[n], topmenu0);
	    break;
	  case 1:
	    if (n < 4) fl_set_object_label(fd_cbpinput->cbp_threatlist[n], topmenu0);
	    fl_set_object_label(fd_cbpcoaview->coasub_threat[n], topmenu0);
	    break;
	  case 2:
	    if (n < 4) fl_set_object_label(fd_cbpinput->cbp_assetlist[n], topmenu0);
	    fl_set_object_label(fd_cbpcoaview->coasub_assets[n], topmenu0);
	    break;
	  }
	  Ii = Ii + 1;
	  // printf("Processing %d Items for Sub-category %s\n", n_items, topmenu0);
	  strcpy(indices[Ii].Name, topmenu0);
          indices[Ii].Weighting     = 0;
          indices[Ii].Weight        = 1.0;
	  indices[Ii].Category      = outer;
	  indices[Ii].Class         = n;
	  indices[Ii].Enabled       = -(outer+1)*10;
          indices[Ii].n_constraints = 0;
          indices[Ii].n_synonyms    = 0;

          for (i=0; i<n_items; i++) {
            do fgets(cbptemp, 128, casefp); while (cbptemp[0] == '#');
	    sscanf(cbptemp, "%s %d %f %d %d %d", topmenu0, &mode, &weight,
		   &n_constrain, &n_synon, &enabled);
	    Ii = Ii + 1;
	    strsub(topmenu0, '_', ' ');
	    for (il=0; il<strlen(topmenu0); il++)
	          topmenu0[il] = (char)tolower((int)topmenu0[il]);
	    // printf("   Item %d is %s in slot %d\n", i+1, topmenu0, Ii);
	    strcpy(indices[Ii].Name, topmenu0);
            indices[Ii].Weighting     = mode;
            if (weight >= 0.0) indices[Ii].Weight = weight;
	    indices[Ii].Category      = outer;
	    indices[Ii].Class         = n;
	    indices[Ii].Enabled       = 0;
            indices[Ii].n_constraints = n_constrain;
	    KeyCounts[indices[Ii].Category][indices[Ii].Class]++;
	    //
	    //   Add the constraints to the constraint list for this indice
	    //
	    if (n_constrain > 0) {
               indices[Ii].Constraints = new char*[indices[Ii].n_constraints];
	       for (j=0; j<n_constrain; j++) {
                 do fgets(cbptemp, 128, casefp); while (cbptemp[0] == '#');
	         sscanf(cbptemp, "%32s", chconstrain);
		 indices[Ii].Constraints[j] = (char *)malloc(40);
		 strncpy(indices[Ii].Constraints[j], chconstrain, 32);
	       }
            }
	    //
	    //   Add the synonyms to the synonym list for this indice
	    //
	    if(n_synon > 0) {
               indices[Ii].Synonyms = new char*[indices[Ii].n_synonyms];
	       for (j=0; j<n_synon; j++) {
                 do fgets(cbptemp, 128, casefp); while (cbptemp[0] == '#');
	         sscanf(cbptemp, "%32s", chsynonym);
		 indices[Ii].Synonyms[j] = (char *)malloc(40);
		 strncpy(indices[Ii].Synonyms[j], chsynonym, 32);
	       } 
	    }
          }
       }

   }
   CatStart[n_cats] = Ii+1;

   do fgets(cbptemp, 128, casefp); while (cbptemp[0] == '#');
   if (strstr(cbptemp, "[EOF]") == NULL) fprintf(stderr, "Expecting [EOF]\n");

   n_keys = Ii;
}

void ShowKeys()
{
int             i, j, k, n, m, outer, Ii;
char            chtemp[64];
FL_COLOR        CatColor[3] = { FL_GREEN, FL_RED, FL_BLUE };

   n = fl_get_choice(fd_cbpinput->category)-1;
   j = CatStart[n];
   k = CatStart[n+1];
//
//   Add the keywords to the browser, bolding if enabled
//
   fl_clear_browser(fd_cbpinput->indice_browser);
   fl_clear_browser(fd_cbpplan->pindice_browser);

   for (i=j; i<k; i++) {
       if (indices[i].Enabled == 0) {             // If not enabled
	 sprintf(chtemp, "      ", CatColor[indices[i].Category]);
         sprintf(cbptemp, "%s%s (%5.2f)", chtemp, indices[i].Name, indices[i].Weight);
       } else  if (indices[i].Enabled < 0) {      // If menu title
	if (indices[i].Enabled < -9) {
	  sprintf(cbptemp, "@C%d@b@N   %s", CatColor[indices[i].Category], indices[i].Name);
	} else {                                  // 
	  sprintf(cbptemp, "@C%d@b@N%s", CatColor[indices[i].Category], indices[i].Name);
	} 
      } else {                                    // Enabled indice
	 sprintf(chtemp, "@C%d@b      ", CatColor[indices[i].Category]);
         sprintf(cbptemp, "%s%s (%5.2f)", chtemp, indices[i].Name, indices[i].Weight);
      }
      fl_add_browser_line(fd_cbpinput->indice_browser, cbptemp);
      fl_add_browser_line(fd_cbpplan->pindice_browser, cbptemp);
   }
}

void SaveCase(char* filename, int nwords)
{
FILE            *casefp;
int             j;
  /*
          //
          //   Save the Keyword info
          //
          Ii = -1;
	  for (iloop=0; iloop<3; iloop++) {
             n_menus = menu_counts[iloop];
             fprintf(casefp, "########\n%-15s%5d\n", menu_names[iloop], n_menus);
             for (n=0; n<n_menus; n++) {
	        if (iloop == 0) n_items = fl_getpup_items(SubGeneral[n]);
	        if (iloop == 1) n_items = fl_getpup_items(SubThreats[n]);
		if (iloop == 2) n_items = fl_getpup_items(SubAssets[n]);
	        Ii = Ii + 1;
	        fprintf(casefp, "#\n%-15s%5d\n", indices[Ii].Name, n_items);
                for (i=0; i<n_items; i++) {
	           Ii = Ii + 1;

	           //
	           //   Add the constraints
	           //
	           if (indices[Ii].n_constraints > 0) {
	              for (j=0; j<indices[Ii].n_constraints; j++) {
	                 fprintf(casefp, "  %-32s\n", indices[Ii].Constraints[j]);
	              }
                   }
	           //
	           //   Add the synonyms
	           //
	           if(indices[Ii].n_synonyms > 0) {
	              for (j=0; j<indices[Ii].n_synonyms; j++) {
	                 fprintf(casefp, "  %-32s\n", indices[Ii].Synonyms[j]);
	              } 
	           }
	        }
	     }
	  }
  */
}
/*                                                                       */
/* --------------------------------------------------------------------- */
/*           J C S   W A R N I N G   O R D E R   R O U T I N E S         */
/* --------------------------------------------------------------------- */
/*                                                                       */
void jcsordersCB(FL_OBJECT *ob, long data)
{
FILE            *inputfp;
FILE            *casefp;
FILE            *coafp;
FILE            *pipein, *pipeout;
int             i, j, k, l, n, item = data; 
int             n_items, n_matches;
int             p_match, caseno;
int             n_constrain, n_synon, mode, enabled, nwords;
float           weight;
float           docno;
const char      *menutext;
char            *p;
char            *ordername;
char            chword[32];
char            filename[128];
char            chtext[1024], chitem[64];
char            chtmp[8];
char            cmdline[2400];


   switch (item) {
     case 0: // Process JCS Menu 
       strcpy(CaseOrder, fl_get_menu_text(ob));
       sprintf(filename, "COAkeys.%s", getenv("GPDADOMAIN"));
       if ((casefp = fopen(filename, "r")) != NULL) {
	  LoadKeys(casefp);
	  fclose(casefp);
	  fl_set_input(fd_cbpinput->cbp_caseid, CaseOrder);
       } else {
          fl_show_messages("Error: Unable to open Keyword file");
          break;
       }      
       //
       //   Parse the JCS Warning Order using the loaded keyphrases.
       //   By using 'strstr' for comparison, we can have multiword phrases
       //     with embedded blanks, with the restriction that more than one
       //     similar multiword phrase in the same line will only be counted
       //     once.
       //
       sprintf(filename, "%s/%s", getenv("SEARCHDIR"), CaseOrder);
       fl_set_object_label(fd_cbprdeinput->rde_jcswo, CaseOrder);
       if ((inputfp = fopen(filename, "r")) != NULL) {
          while (feof(inputfp) == 0) {
	    n_words = 0;
	    do fgets(chtext, 1024, inputfp); while (chtext[0] == '#');
	    //fscanf(inputfp, "%s", chtext);
            if (strlen(chtext) > 3) {
	       for (i=0; i<strlen(chtext); i++)
	          chtext[i] = (char)tolower((int)chtext[i]);
               //chtext[strlen(chtext)] = '\0';
	       //
	       //   Check for each key present in line
	       //
               for (i=0; i<n_keys; i++) 
		 if ((strstr(chtext, indices[i].Name) != NULL) && (indices[i].Enabled >= 0)) {
		   //fprintf(stderr, "Match %s with index %d\n", indices[i].Name, i);
		   n_words++;
                   indices[i].Enabled = indices[i].Enabled + 1;        // Holds word count in WO
	           KeyActive[indices[i].Category][indices[i].Class]++;
		   // break;
		 }
	    }
          }
          fclose(inputfp);
	  //
	  for (n=0; n<4; n++) {
	    fl_clear_browser(fd_cbpinput->cbp_goallist[n]);
	    fl_clear_browser(fd_cbpinput->cbp_threatlist[n]);
	    fl_clear_browser(fd_cbpinput->cbp_assetlist[n]);
	  }
	  //
	  if (strcmp(getenv("GPDADOMAIN"), "iow") == 0)
	    strcpy(cmdopts, " NETBIOS NT NULL session ");
	  TotalActive = 0;
	  for (i=0; i<n_keys; i++) {
	    if (indices[i].Enabled > 0) {            // If enabled
	      n = indices[i].Class;
	      switch (indices[i].Category) {
	      case 0:
		if (n < 4) fl_addto_browser(fd_cbpinput->cbp_goallist[n], indices[i].Name);
		break;
	      case 1:
		if (n < 4) fl_addto_browser(fd_cbpinput->cbp_threatlist[n], indices[i].Name);
		break;
	      case 2:
		if (n < 4) fl_addto_browser(fd_cbpinput->cbp_assetlist[n], indices[i].Name);
		break;
	      }
	      strcat(cmdopts, indices[i].Name);
	      strcat(cmdopts, " ");
	      TotalActive++;
	    }
	  }
	  fl_set_input(fd_cbpinput->cbp_criteria, cmdopts);
	  //
          ShowKeys();
          fl_show_object(fd_cbpinput->view_order);
          fl_activate_object(fd_cbpinput->view_order);
          fl_show_object(fd_cbpinput->cbp_exec);
       } else fl_show_messages("Error: Unable to open Cases file");
       break;

     case 1: // View JCS Order
       fl_set_form_position(fd_cbpview->cbpview, COAwinX, COAwinY);
       fl_set_form_size(fd_cbpview->cbpview, COAwinW, COAwinH); 
       fl_show_form(fd_cbpview->cbpview, FL_PLACE_POSITION,FL_NOBORDER, "Planning-TextView");
       StoreActiveEntry("Planning-TextView");
       fl_raise_form(fd_cbpview->cbpview);
       sprintf(filename, "%s/%s", getenv("SEARCHDIR"), CaseOrder);
       fl_load_browser(fd_cbpview->view_browser, filename);
       break;

     case 4: // Save case
       //sprintf(filename, "%s/%s.ctl", getenv("SEARCHDIR"), CaseOrder);
       //SaveCase(filename, n_words);
       break;

     case 5: // Search Cases
       if (0 /*SEARCH != NULL*/) {
	  n_matches = 3;

	  strcpy(cmdline, SEARCH);
	  strcat(cmdline, " ");                         // Assure separator
	  strcat(cmdline, fl_get_input(fd_cbpinput->cbp_criteria));
	  //fprintf(stderr, "Doing: %s\n", cmdline);

          if ((pipein = popen(cmdline, "r")) == NULL)   // Start the search engine
             exit(777);
          fgets(cbptemp, 120, pipein);                  // Get title line
          fgets(cbptemp, 120, pipein);                  // Get text line
          fgets(cbptemp, 120, pipein);                  // Get blank line
          fgets(cbptemp, 120, pipein);                  // Get match line
          if (strstr("document", cbptemp) == 0)         // Get # of matching documents
	     sscanf(cbptemp, "%d", &n_matches);
          else
             fprintf(stderr, "Expecting document count line\n");
          //fprintf(stderr, "%d document(s) matched\n", n_matches);
          fgets(cbptemp, 120, pipein);                  // Get blank line
          fgets(cbptemp, 120, pipein);                  // Get header line

          if (n_matches > 8) n_matches = 8;             // We're only interested in the top 8
          //fprintf(stderr, "Will accept %d document(s).\n", n_matches);
          for (i=0; i<n_matches; i++) {
	     caseno = i;
	     //printf("Accept document %d\n", i+1); fflush(stdout);
	     fgets(cbptemp, 120, pipein);               // Get next line from pipe
	     fputs(cbptemp, stdout);                  // Echo it for now
	     strsub(cbptemp, '\n', '\0');               // Get rid of newline
	     //strtrm(cbptemp);                         // Get rid of trailing blanks
             sscanf(cbptemp, "%f %d", &docno, &CaseMatch[caseno]);// Get Match rating
	     //
	     //   Get the case file name
	     //
             ordername = strrchr(cbptemp, '/');         // Find last token following a '/'
             ordername++;                               // Skip the '/'
	     strcpy(CaseName[caseno], ordername);
	  }
	  while (!feof(pipein)) {
	    //fprintf(stderr, "Getting extra\n");
	    fgets(cbptemp, 120, pipein);
	  }
          pclose(pipein);
       } else {
	 n_matches = (int)fl_get_counter_value(fd_cbpinput->cbpcases);
	 for (i=0; i<n_matches; i++) {
	   sprintf(CaseName[i], "%s-COA-%d", CaseOrder, i+1);
	   CaseMatch[i] = 81;
	 }
       }
       //
       //   We have either gotten all the matches from the search engine, or set the defaults
       //
       for (i=0; i<8; i++) {
	 fl_deactivate_object(fd_cbpcoaview->case_name[i]);
	 fl_set_object_color(fd_cbpcoaview->case_name[i], FL_INACTIVE_COL, FL_COL1);
       }

       fl_clear_browser(fd_cbprdeinput->ro_browser);

       for (i=0; i<n_matches; i++) {
	  caseno = i;
	  fl_set_object_helper(fd_cbpcoaview->case_name[caseno], CaseName[caseno]);
	  fl_set_object_label(fd_cbprdeinput->rde_jcscoa[caseno], CaseName[caseno]);
	  fl_activate_object(fd_cbpcoaview->case_name[caseno]);
	  fl_set_object_color(fd_cbpcoaview->case_name[caseno], FL_WHITE, FL_WHITE);
	  //
	  //   Determine score by category for each case
	  //
	  if (strcmp(getenv("GPDADOMAIN"), "iow") == 0) {
            sprintf(cbptemp, "./IDSrules/%s", CaseName[caseno]);
	    if ((coafp = fopen(cbptemp, "r")) != NULL) { 
	      for (k=0; k<5; k++) for (l=0; l<5; l++) KeyMatchs[k][l] = 0;
              //
	      fgets(cbptemp, 1024, coafp);        // Get rule
	      //strsub(cbptemp, '\n', '\0');      // Get rid of trailing LF
	      fl_add_browser_line(fd_cbprdeinput->ro_browser, cbptemp);
	      strsub(cbptemp, '"', ' ');          // Make it tokenizable
	      p = strtok(cbptemp, " ");           // Get "alert"
	      while(1) {                          // Process all tokens
		p = strtok(NULL, " ");            // Get next token
		if (p == NULL) break;             // If there isn't one, done with rule
		strcpy(chtext, p);                // Put it where we can operate on it
		for (k=0; k<strlen(chtext); k++)  // It needs to be all lower case
		  chtext[k] = (char)tolower((int)chtext[k]);
		for (k=0; k<n_keys; k++)          // Search for dictionary keyword match
		  if ((strcmp(chtext, indices[k].Name) == 0) && (indices[k].Enabled > 0)) {
		    KeyMatchs[indices[k].Category][indices[k].Class]++;
		    indices[k].CaseKeys[caseno] = 'Y';
		    break;
		  }
	      }  // end while
	      fclose(coafp);
	    } else {
	      sprintf(chtext, "Error: Unable to open Case file %s", cbptemp);
	      fl_show_messages(chtext);
	      return;
	    }
	  } else {
	    fprintf(stderr, "Opening file: ./JCSorders/%s.ctl", CaseName[caseno]);
	    sprintf(cbptemp, "./JCSorders/%s.ctl", CaseName[caseno]);
	    if ((coafp = fopen(cbptemp, "r")) != NULL) {
	      for (k=0; k<5; k++) for (l=0; l<5; l++) KeyMatchs[k][l] = 0;
	      do fgets(cbptemp, 128, coafp); while (cbptemp[0] == '#');
	      sscanf(cbptemp, "%d", &nwords);
	      do fgets(cbptemp, 128, coafp); while (cbptemp[0] == '#');
	      sscanf(cbptemp, "%s", CaseOrder);
	      fl_set_input(fd_cbpinput->cbp_caseid, CaseOrder);
	      //strsub(cbptemp, '\n', '\0');               // Get rid of newline
	      fl_set_object_label(fd_cbprdeinput->rde_jcscoa[caseno], CaseOrder);
	      do fgets(cbptemp, 128, coafp); while (cbptemp[0] == '#');
	      while (strstr(cbptemp, "<EOD>") == NULL) {
		sscanf(cbptemp, "%s %d %f %d %d %d", chitem, &mode, &weight,
		       &n_constrain, &n_synon, &enabled);
		for (k=0; k<n_keys; k++)
		  if ((strcmp(chitem, indices[k].Name) == 0) && (indices[k].Enabled > 0)) {
		    KeyMatchs[indices[k].Category][indices[k].Class]++;
		    indices[k].CaseKeys[caseno] = 'Y';
		    break;
		  }
		do fgets(cbptemp, 128, coafp); while (cbptemp[0] == '#');
	      }
	      fclose(coafp);
	    } else {
	      sprintf(chtext, "Error: Unable to open Case file %s", cbptemp);
	      fl_show_messages(chtext);
	      return;
	    }
	  }
	  //
	  //   Display scores by case no, category, and class
	  //
	  if (caseno < 5) {
	    for (k=0; k<n_cats; k++)
	      for (l=0; l<n_class[k]; l++) {
		float score;
		int s = caseno*15 + k*5 + l;
		if (KeyActive[k][l] != 0)
		    score = 100.0 * (float)(KeyMatchs[k][l]/(float)KeyActive[k][l]);
		else
		    score = 0.0;
		//
		if (score <= 50.0)
		    fl_set_object_color(fd_cbpcoaview->cbp_score[s], FL_TOMATO, FL_TOMATO);
		if (score < 1.0)
		    fl_set_object_color(fd_cbpcoaview->cbp_score[s], FL_WHITE, FL_WHITE);
		else if (score > 85.0)
		    fl_set_object_color(fd_cbpcoaview->cbp_score[s], FL_PALEGREEN, FL_PALEGREEN);
		else
		    fl_set_object_color(fd_cbpcoaview->cbp_score[s], FL_WHEAT, FL_WHEAT);
		sprintf(cbptemp, "%5.1f%%", score);
		fl_set_object_label(fd_cbpcoaview->cbp_score[s], cbptemp);
	      }
	  }
          TotalMatchs[caseno] = 0;
          for (k=0; k<5; k++)
	    for (l=0; l<5; l++)
	      TotalMatchs[caseno] = TotalMatchs[caseno] + KeyMatchs[k][l];
	  sprintf(chtmp, "%5.1f%%", ((double)TotalMatchs[caseno]/(double)TotalActive)*100.0);
	  fl_set_object_label(fd_cbpcoaview->coa_score[caseno], chtmp);
	  fl_set_object_label(fd_cbprdeinput->rde_coaper[caseno], chtmp);
       }
       fl_show_object(fd_cbpinput->cbp_view);
       //
       //   Build input for COA Analyzer
       //
       coafp = fopen("ISPAN.ini", "w");
       //
       inputfp = fopen("MissionDesc.txt", "r");
       while (1) {
	 fgets(cbptemp, 128, inputfp);
	 if (feof(inputfp)) break;
	 fprintf(coafp, "%s", cbptemp);
       }
       fclose(inputfp);
       //
       fprintf(coafp, "[COAs]\n");
       fprintf(coafp, "%d\n", n_matches);
       //
       for (i=0; i<n_matches; i++) {
         fprintf(coafp, "# COA %d\n", i+1);
         fprintf(coafp, "%f\n", (float)(TotalMatchs[i])/(float)(TotalActive));
	 sprintf(cbptemp, "./JCSorders/%s.coa", CaseName[i]);
         inputfp = fopen(cbptemp, "r");
         while (1) {
	   fgets(cbptemp, 128, inputfp);
	   if (feof(inputfp)) break;
	   fprintf(coafp, "%s", cbptemp);
         }
         fclose(inputfp);
       }
       fclose(coafp);
       //
       coaviewCB(NULL, -1);
       break;
 
     default:
       break;
   }
}

void viewdoneCB(FL_OBJECT *ob, long data)
{
   fl_hide_form(fd_cbpview->cbpview);
   EraseActiveEntry("Planning-TextView");
}

void viewbrowserCB(FL_OBJECT *ob, long data)
{

}

void userdefaultCB(FL_OBJECT *ob, long data)
{
  /* fill-in code for callback */
}

void controlparmsCB(FL_OBJECT *ob, long data)
{
  /* fill-in code for callback */
}

void cbpscoreCB(FL_OBJECT *ob, long data)
{
   if (fl_mouse_button() == FL_RIGHTMOUSE) {
      fl_show_messages("Explanation:\nLogistics Annex is incomplete");
   }   
}

void genweightCB(FL_OBJECT *ob, long data)
{
int      Indice, item = data;
float    WeightTemp;

   Indice = fl_get_browser(fd_cbpinput->indice_browser)-1;
   if (Indice < 1) return;

   WeightTemp = indices[Indice].Weighting;

   switch (item) {
     case 0: // Weighting menu
       item = fl_get_choice(fd_cbpinput->gen_weighting)-1;
       indices[Indice].Weighting = WeightMethod[item];
       break;

     case 1: // Thumbwheel
       WeightTemp = (float)fl_get_slider_value(fd_cbpinput->gen_thumb);
       break;

     default:
       break;
   }

   switch (indices[Indice].Weighting) {
     case LOW:
       indices[Indice].Weight = DEFAULT_LOW;
       break;

     case MEDIUM:
       indices[Indice].Weight = DEFAULT_MED;
       break;

     case HIGH:
       indices[Indice].Weight = DEFAULT_HIGH;
       break;

     case WEIGHT:
       indices[Indice].Weight = WeightTemp;
       break;

     case MUSTHAVE:
       indices[Indice].Weight = 1.0;
       break;

     case CANNOT:
       indices[Indice].Weight = 0.0;
       break;

     default:
       break;
   }

   sprintf(cbptemp, "%f", indices[Indice].Weight);
   fl_set_slider_value(fd_cbpinput->gen_thumb, (double)indices[Indice].Weight);
}

void genbrowserCB(FL_OBJECT *ob, long data)
{
int             i, n, lineno;
FL_COLOR        CatColor[3] = { FL_GREEN, FL_RED, FL_BLUE };

   lineno = fl_get_browser(fd_cbpinput->indice_browser); 
   Indice = lineno-1+CatStart[fl_get_choice(fd_cbpinput->category)-1];
   indices[Indice].Enabled = !indices[Indice].Enabled;
   //
   //   Set the Weight indicator
   //
   fl_set_slider_value(fd_cbpinput->gen_thumb, (double)indices[Indice].Weight);
   //
   //   Toggle indice enabled
   //
   if (indices[Indice].Enabled == 0) {
     sprintf(cbptemp, "      %s (%5.2f)", indices[Indice].Name, indices[Indice].Weight);
   } else {
     sprintf(cbptemp, "@C%d@b      %s (%5.2f)", CatColor[indices[Indice].Category],
	     indices[Indice].Name, indices[Indice].Weight);
   }
   fl_replace_browser_line(fd_cbpinput->indice_browser, lineno, cbptemp);
   //
   //   Make sure the other windows and search line match
   //
   for (n=0; n<4; n++) {
     fl_clear_browser(fd_cbpinput->cbp_goallist[n]);
     fl_clear_browser(fd_cbpinput->cbp_threatlist[n]);
     fl_clear_browser(fd_cbpinput->cbp_assetlist[n]);
   }
   //
   strcpy(cmdopts, " ");
     for (i=0; i<n_keys; i++) {
       if (indices[i].Enabled > 0) {              // If enabled
	 n = indices[i].Class;
	 switch (indices[i].Category) {
	 case 0:
	   if (n < 4) fl_addto_browser(fd_cbpinput->cbp_goallist[n], indices[i].Name);
	   break;
	 case 1:
	   if (n < 4) fl_addto_browser(fd_cbpinput->cbp_threatlist[n], indices[i].Name);
	   break;
	 case 2:
	   if (n < 4) fl_addto_browser(fd_cbpinput->cbp_assetlist[n], indices[i].Name);
	   break;
	 }
	 strcat(cmdopts, indices[i].Name);
	 strcat(cmdopts, " ");
       }
     }
   fl_set_input(fd_cbpinput->cbp_criteria, cmdopts);
   /*
   fl_clear_browser(fd_cbpinput->constrain_list);
   fl_clear_browser(fd_cbpinput->synonym_list);
   //fl_clear_browser(fd_cbprdeinput->rde_constraints);

   if (indices[Indice].n_constraints != 0) {
     for (i=0; i<indices[Indice].n_constraints; i++) {
        fl_add_browser_line(fd_cbpinput->constrain_list, indices[Indice].Constraints[i]);
	//fl_add_browser_line(fd_cbprdeinput->rde_constraints, indices[Indice].Constraints[i]);
     }
   }

   if (indices[Indice].n_synonyms != 0) {
     for (i=0; i<indices[Indice].n_synonyms; i++)
        fl_add_browser_line(fd_cbpinput->synonym_list, indices[Indice].Synonyms[i]);
   }
   */
}

void genselectCB(FL_OBJECT *ob, long data)
{
int             Indice;
FL_COLOR        CatColor[3] = { FL_YELLOW, FL_RED, FL_BLUE };

   Indice = fl_get_browser(fd_cbpinput->indice_browser)-1;

   indices[Indice].Enabled = !indices[Indice].Enabled;

   if (indices[Indice].Enabled == 0) {
     sprintf(cbptemp, "      %s (%5.2f)", indices[Indice].Name, indices[Indice].Weight);
   } else {
     sprintf(cbptemp, "@C%d@b      %s (%5.2f)", CatColor[indices[Indice].Category],
	     indices[Indice].Name, indices[Indice].Weight);
   }
   fl_replace_browser_line(fd_cbpinput->indice_browser, Indice+1, cbptemp);
}

void morekeysCB(FL_OBJECT *ob, long data)
{
}

void lesskeysCB(FL_OBJECT *ob, long data)
{
}

void userviewCB(FL_OBJECT *ob, long data)
{
}

void catselectCB(FL_OBJECT *ob, long data)
{

   ShowKeys();
}

void goallistCB(FL_OBJECT *ob, long data)
{
}
/*                                                                       */
/* --------------------------------------------------------------------- */
/*                  U S E R   C A S E S   R O U T I N E S                */
/* --------------------------------------------------------------------- */
/*                                                                       */
void caseCB(FL_OBJECT *ob, long data)
{
int             item = data;
const char      *casefilename;
FILE            *casefp;
FILE            *coafp;
int             Ii;
int             i, j, k, n, m, outer;
int             n_constrain, n_synon, mode, enabled, nwords;
int             n_items, n_matches, iloop;
int             menu_counts[3] = { 3, 4, 4 };
char            menu_names[3][16]  = { "Mission", "Threats", "Assets" };
int             n_enable;
int             pupID = -1, itemno;
int             p_match;
float           docno;
float           weight;
char            *ordername;
char            filename[128];
char            chconstrain[32];
char            chsynonym[32];
char            topmenu0[32];
char            chtmp[8];
char            chitem[40];
char            chtext[40];

time_t          clock;
struct tm       *loctime;
char            *str;


   switch (item) {
     case 1: // New
       strcpy(CaseOrder, " ");
       strcpy(CaseExecute, " ");
       strcpy(CaseEstimate, " ");
       strcpy(CaseOrder, "New_Criteria");
       fl_set_input(fd_cbpinput->cbp_caseid, CaseOrder);

       sprintf(filename, "COAkeys.%s", getenv("GPDADOMAIN"));
       if ((casefp = fopen(filename, "r")) != NULL) {
	  LoadKeys(casefp);
	  fclose(casefp);
	  ShowKeys();
       } else {
          fl_show_messages("Error: Unable to open Keyword file");
       }
       break;

     case 2: // Info
       break;

     case 3: // Load
       sprintf(filename, "COAkeys.%s", getenv("GPDADOMAIN")); 
       if ((casefp = fopen(filename, "r")) != NULL) {
	  LoadKeys(casefp);
	  fclose(casefp);
       } else {
          fl_show_messages("Error: Unable to open Keyword file");
       }

       casefilename = fl_show_fselector("Case Base", "./SelCriteria", "*.ctl", NULL);
       if (casefilename != NULL) {
          if ((casefp = fopen(casefilename, "r")) != NULL) {

             do fgets(cbptemp, 128, casefp); while (cbptemp[0] == '#');
	     sscanf(cbptemp, "%d", &nwords);
             do fgets(cbptemp, 128, casefp); while (cbptemp[0] == '#');
             sscanf(cbptemp, "%s", CaseOrder);
             fl_set_input(fd_cbpinput->cbp_caseid, CaseOrder);
             fl_set_input(fd_caseinfo->info_caseid, CaseOrder);

             do fgets(cbptemp, 128, casefp); while (cbptemp[0] == '#');
	     n_words = 0;
	     while (strstr(cbptemp, "<EOD>") == NULL) {
	       sscanf(cbptemp, "%s %d %f %d %d %d", chitem, &mode, &weight,
		     &n_constrain, &n_synon, &enabled);
               for (i=0; i<n_keys; i++) 
		 if ((strcmp(chitem, indices[i].Name) == 0) && (indices[i].Enabled >= 0)) {
		    n_words++;
                    indices[i].Enabled       = indices[i].Enabled + 1;
                    indices[i].Weighting     = mode;
                    indices[i].Weight        = weight;
	            KeyActive[indices[i].Category][indices[i].Class]++;
                    break;
		 }
               do fgets(cbptemp, 128, casefp); while (cbptemp[0] == '#');
	     }
	     ShowKeys();
             // Get Case description
             do fgets(cbptemp, 128, casefp); while (cbptemp[0] == '#');
             strncpy(CaseDesc, cbptemp, 127);
             fl_set_input(fd_caseinfo->info_desc, CaseDesc);
             // Get Case Preparer
             do fgets(cbptemp, 128, casefp); while (cbptemp[0] == '#');
             // Get date case prepared
             do fgets(cbptemp, 128, casefp); while (cbptemp[0] == '#');
             strncpy(CaseBilt, cbptemp, 31);
             fl_set_object_label(fd_caseinfo->info_bilt, CaseBilt);
             // Get date case last used
             do fgets(cbptemp, 128, casefp); while (cbptemp[0] == '#');
             strncpy(CaseLast, cbptemp, 31);
             fl_set_object_label(fd_caseinfo->info_bilt, CaseLast);
             // Get number of times case used
             do fgets(cbptemp, 128, casefp); while (cbptemp[0] == '#');
             CaseUsed = atoi(cbptemp);
             fl_set_object_label(fd_caseinfo->info_used, cbptemp);
             // Get percent of 'selected' keywords
             do fgets(cbptemp, 128, casefp); while (cbptemp[0] == '#');
             CasePercent = atoi(cbptemp);
             fl_set_object_label(fd_caseinfo->info_percent, cbptemp);
             // Get default 'low' weighting
             do fgets(cbptemp, 128, casefp); while (cbptemp[0] == '#');
             CaseLow = atof(cbptemp);
             fl_set_input(fd_caseinfo->info_low, cbptemp);
             // Get default 'medium' weighting
             do fgets(cbptemp, 128, casefp); while (cbptemp[0] == '#');
             CaseMed = atof(cbptemp);
             fl_set_input(fd_caseinfo->info_med, cbptemp);
             // Get default 'high' weighting
             do fgets(cbptemp, 128, casefp); while (cbptemp[0] == '#');
             CaseHigh = atof(cbptemp);

             fclose(casefp);

             fl_show_object(fd_cbpinput->cbp_view);
          } else {
             fl_show_messages("Error: Unable to open Criteria file");
          }
       }
       break;

     case 4: // Save Case
       strcpy(CaseID, CaseOrder);
       fl_set_input(fd_caseinfo->info_caseid, CaseID);

       strcpy(CasePrep, UserName);
       fl_set_input(fd_caseinfo->info_prep, CasePrep);

       time(&clock);
       loctime = gmtime(&clock);
       str = asctime(loctime);
       strncpy(CaseBilt, str, 31);
       strsub(CaseBilt, '\n', ' ');
       fl_set_object_label(fd_caseinfo->info_bilt, CaseBilt);

       strncpy(CaseLast, str, 31);
       strsub(CaseLast, '\n', ' ');
       fl_set_object_label(fd_caseinfo->info_last, CaseLast);

       sprintf(cbptemp, "%d", CaseUsed);
       fl_set_object_label(fd_caseinfo->info_used, cbptemp);

       n_enable = 0;
       for (i=0; i<n_keys; i++) {
	  if (indices[i].Enabled != 0) n_enable++;
       }
       CasePercent = (int)(100.0 * (float)n_enable/(float)n_keys);
       sprintf(cbptemp, "%d", CasePercent);
       fl_set_object_label(fd_caseinfo->info_percent, cbptemp);

       sprintf(cbptemp, "%f", CaseLow);
       fl_set_input(fd_caseinfo->info_low, cbptemp);

       sprintf(cbptemp, "%f", CaseMed);
       fl_set_input(fd_caseinfo->info_med, cbptemp);

       sprintf(cbptemp, "%f",CaseHigh);
       fl_set_input(fd_caseinfo->info_high, cbptemp);

       fl_show_form(fd_caseinfo->caseinfo,FL_PLACE_CENTERFREE,FL_FULLBORDER,"Case Info");
       break;

     case 8: // Write Case
       //strcpy(filename, fl_show_fselector("Selection Criteria", "./SelCriteria", "*.ctl", NULL));
       strcpy(cbptemp, fl_get_input(fd_caseinfo->info_caseid));
       strsub(cbptemp, ' ', '_');
       sprintf(filename, "./SelCriteria/%s.ctl", cbptemp);
       if ((casefp = fopen(filename, "w")) != NULL) {
          fprintf(casefp, "%5d\n", n_words);
          fprintf(casefp, "%s\n", filename);
          fprintf(casefp, "%s\n", "#");
          for (j=0; j<n_keys; j++) {
            if (indices[j].Enabled > 0) 
	      fprintf(casefp, "%-15s %5d %5.1f %5d %5d %5d\n", 
	          indices[j].Name,
		  indices[j].Weighting,
		  indices[j].Weight,
                  indices[j].n_constraints,
                  indices[j].n_synonyms,
	          indices[j].Enabled);	 
          }
          fprintf(casefp, "%s\n", "<EOD>");
	  strcpy(CaseDesc, fl_get_input(fd_caseinfo->info_desc));
	  strtrm(CaseDesc);
          fprintf(casefp, "%s\n", CaseDesc);
	  strcpy(CasePrep, fl_get_input(fd_caseinfo->info_prep));
	  strtrm(CasePrep);
          fprintf(casefp, "%s\n", CasePrep);
          fprintf(casefp, "%s\n", CaseBilt);
          fprintf(casefp, "%s\n", CaseLast);
          fprintf(casefp, "%d\n", CaseUsed);
          fprintf(casefp, "%d\n", CasePercent);
          fprintf(casefp, "%f\n", CaseLow);
          fprintf(casefp, "%f\n", CaseMed);
          fprintf(casefp, "%f\n", CaseHigh);
	  fprintf(casefp, "%s\n", "<EOF>");
	  fclose(casefp);
       } else {
          fl_show_messages("Error: Unable to open Criteria save file");
          break;
       }
 
       strcpy(CaseID, fl_get_input(fd_caseinfo->info_caseid));
       fl_set_input(fd_cbpinput->cbp_caseid, CaseID);
       fl_set_input(fd_cbpcoaview->cbpcoa_caseid, CaseID);
       fl_set_input(fd_cbpmapview->cbpmap_caseid, CaseID);

     case 9: // Cancel
       fl_hide_form(fd_caseinfo->caseinfo);
       break;

     default:
       break;
   }
}

void caseselectCB(FL_OBJECT *ob, long data)
{
int             i, item = (int)data; 
char            chbuf[40];
FL_COLOR        CatColor[3] = { FL_YELLOW, FL_RED, FL_BLUE };
//
//   Add the keywords to the browser, bolding if enabled
//
   if (item == 0) strcpy(cbptemp, "JCS-Order-1014.4");
   if (item == 1) strcpy(cbptemp, "JCS-Order-1258.2");
   if (item == 2) strcpy(cbptemp, "JCS-Order-1411.8");
   fl_set_input(fd_cbpcoaview->cbp_case_select, CaseName[item]);

   fl_clear_browser(fd_cbpcoaview->cbp_case_active);
   for (i=0; i<n_keys; i++) {
     if (indices[i].CaseKeys[item] == 'Y') {    // Keyword matches enabled Dictionary key
	 sprintf(cbptemp, "      ", CatColor[indices[i].Category]);
         strcat(cbptemp, indices[i].Name);
         fl_add_browser_line(fd_cbpcoaview->cbp_case_active, cbptemp);
      } else if (indices[i].Enabled < 0) {      // If menu title
	if (indices[i].Enabled < -9) {
	   sprintf(cbptemp, "@C%d@b@N   ", CatColor[indices[i].Category]);
	   sprintf(chbuf, "%s (%d/%d/%d)", indices[i].Name,
		   KeyMatchs[indices[i].Category][indices[i].Class],
		   KeyActive[indices[i].Category][indices[i].Class],
		   KeyCounts[indices[i].Category][indices[i].Class]);
	   strcat(cbptemp, chbuf);
	} else {
	   sprintf(cbptemp, "@C%d@b@N", CatColor[indices[i].Category]);
	   strcat(cbptemp, indices[i].Name);
	}
        fl_add_browser_line(fd_cbpcoaview->cbp_case_active, cbptemp); 
      }
   }
}

void caseinfoCB(FL_OBJECT *ob, long data)
{
int      item = data;

   switch (item) {
     case 0: // OK
       strcpy(CaseID, fl_get_input(fd_caseinfo->info_caseid));
       fl_hide_form(fd_caseinfo->caseinfo);

       fl_set_input(fd_cbpinput->cbp_caseid, CaseID);
       fl_set_input(fd_cbpcoaview->cbpcoa_caseid, CaseID);
       fl_set_input(fd_cbpmapview->cbpmap_caseid, CaseID);
       break;

     default:
       break;
   }
}
/*                                                                       */
/* --------------------------------------------------------------------- */
/*           C O U R S E - O F - A C T I O N    R O U T I N E S          */
/* --------------------------------------------------------------------- */
/*                                                                       */
void coaplanCB(FL_OBJECT *ob, long data)
{
const char      *coafilename;
FILE            *casefp;
int             i, item = data;
int             n_opts, n_tgts, value, tgttype, mobile, Rindex;
float           hard, v1, v2, v3, v4, v5, v6, v7;
char            chtype[32];
char            chwpn[4];
char            chname[32];
char            charena[16], choption[32];
char            chopt[64];
char            filename[64];
Window          winid;


   if (strcmp(getenv("GPDADOMAIN"), "iow") == 0) {
     item = 1;
     fl_set_input(fd_cbprdeinput->cbprde_caseid, CaseID);
   } else {
     item = 2;
   }
 
   switch (item) {
     case 0: // Handle the COA rDE screen
       //
       //   Load Response Options
       //
       fl_clear_browser(fd_cbprdeinput->ro_browser);
       sprintf(cbptemp, "%-32s %-12s %-12s %-4s", "Task Name", "Objective",
	       "Arena", "Weap");
       fl_set_object_label(fd_cbprdeinput->ro_browser, cbptemp);
       sprintf(filename, "%s", "Options-NMD.dat");
       if ((casefp = fopen(filename, "r")) != NULL) {
	  do fgets(cbptemp, 128, casefp); while (cbptemp[0] == '#');
          sscanf(cbptemp, "%d", &n_opts);
	  Rindex = 0;
	  for (i=0; i<n_opts; i++) {
             do fgets(cbptemp, 128, casefp); while (cbptemp[0] == '#');
             sscanf(cbptemp, "%s %s %s %s %f %f %f %f %f %f %f",
                    choption, chwpn, charena, chtype, &v1, &v2, &v3, &v4, &v5, &v6, &v7);
	     strcpy(RespOptions[Rindex].Option, choption);
	     strcpy(RespOptions[Rindex].Weapon, chwpn);
	     strcpy(RespOptions[Rindex].Arena,  charena);
	     strcpy(RespOptions[Rindex].Type,   chtype);
	     RespOptions[Rindex].SVbr   = v1;
	     RespOptions[Rindex].ARbr   = v2;
	     RespOptions[Rindex].DMbr   = v3;
	     RespOptions[Rindex].SVrb   = v4;
	     RespOptions[Rindex].ARrb   = v5;
	     RespOptions[Rindex].DMrb   = v6;
	     RespOptions[Rindex].rDE    = v7;
	     RespOptions[Rindex].Active = FALSE;

	   //sprintf(cbptemp, "%-24s %-12s %-12s %-4s %6.1f %6.1f %6.1f %6.1f %6.1f %6.1f %6.1f",
             sprintf(cbptemp, "%-24s %-12s %-12s %-4s",
		     RespOptions[Rindex].Option, RespOptions[Rindex].Type,
		     RespOptions[Rindex].Arena,  RespOptions[Rindex].Weapon);
	     /*	     RespOptions[Rindex].SVbr,   RespOptions[Rindex].ARbr,
		     RespOptions[Rindex].DMbr,   RespOptions[Rindex].SVrb,
		     RespOptions[Rindex].ARrb,   RespOptions[Rindex].DMrb,
		     RespOptions[Rindex].rDE);
	     */
	     fl_addto_browser(fd_cbprdeinput->ro_browser, cbptemp);

	     Rindex++;
          }
	  RespCount = Rindex;
	  fclose(casefp);
       } else
	  fprintf(stderr, "Unable to open Response Options file: %s\n", filename);
       break;

     case 1: // Handle the COA Intrusion rules screen
       sprintf(cbptemp, "%64s", "IDS Rule");
       fl_set_object_label(fd_cbprdeinput->ro_browser, cbptemp);
       sprintf(filename, "%s.rules", CaseID);
       if ((casefp = fopen(filename, "r")) != NULL) {
	 while (!feof(casefp)) {
	   do fgets(cbptemp, 128, casefp); while (cbptemp[0] == '#');

	 } // while
       } else
	  fprintf(stderr, "Unable to open Rules file: %s\n", filename);
       //
       fl_winposition(COAwinX, COAwinY);
       fl_initial_winsize(COAwinW, COAwinH);
       winid = fl_prepare_form_window(fd_cbprdeinput->cbprdeinput,
				  FL_PLACE_POSITION,FL_NOBORDER, "Planning-Tasks");
       StoreActiveEntry("Planning-Tasks");
       fl_winreparent(winid, COAwinid);
       fl_show_form_window(fd_cbprdeinput->cbprdeinput);
       break;

     case 2: // Handle the COA Planning screen
       fl_clear_browser(fd_cbpplan->cbpplan_constraints);
       fl_addto_browser(fd_cbpplan->cbpplan_constraints, "Inadequate Airlifts");
       fl_addto_browser(fd_cbpplan->cbpplan_constraints, "Inteceptors in short supply");
       fl_addto_browser(fd_cbpplan->cbpplan_constraints, "No Expansion of Defended Areas");
       fl_set_input(fd_cbpplan->action, "Disrupt ability to produce and distribute POL");
       fl_set_input(fd_cbpplan->where, "Via ground LOCs");
       fl_set_input(fd_cbpplan->when, "For at least 30 hours starting at receipt of Execute Order");
       fl_set_input(fd_cbpplan->threat, "Supply POL to TBM units");
       fl_set_input(fd_cbpplan->moe, "= 0 POL produced and = 0 POL distributed");
       fl_set_input(fd_cbpplan->hypothesis, "Disrupt POL supplied to TBM units");
       fl_set_input(fd_cbpplan->objtext, "ability to supply POL to TBM units");
       fl_clear_browser(fd_cbpplan->parents);
       fl_addto_browser(fd_cbpplan->parents, "Disrupt TBM deployment");
       fl_clear_browser(fd_cbpplan->children);
       fl_addto_browser(fd_cbpplan->children, " Disrupt POL refinement");
       fl_addto_browser(fd_cbpplan->children, " Disrupt POL storage");
       fl_addto_browser(fd_cbpplan->children, " Disrupt POL delivery");
       strcpy(cbptemp, "Disrupt ability to produce and distribute POL\n");
       strcat(cbptemp, "measured by = 0 POL produced and = 0 POL distributed.\n");
       strcat(cbptemp, "Interferes with supply POL to TBM units\n");
       strcat(cbptemp, "which interferes with supply POL to TBM units.");
       fl_set_input(fd_cbpplan->mechanism, cbptemp);
       //
       fl_winposition(COAwinX, COAwinY);
       fl_initial_winsize(COAwinW, COAwinH);
       winid = fl_prepare_form_window(fd_cbpplan->cbpplan,
				  FL_PLACE_POSITION,FL_NOBORDER, "Planning-COA");
       StoreActiveEntry("Planning-COA");
       fl_winreparent(winid, COAwinid);
       fl_show_form_window(fd_cbpplan->cbpplan);
       break;

     default:
       break;
   }

   return;
}
 
void coaselectCB(FL_OBJECT *ob, long data)
{
const char      *coafilename;
FILE            *casefp;
int             i, item = data;
/*
   switch (item) {
     case 0: // Process COA Menu  
       coafilename = fl_show_fselector("Course-of-Action", "./JCScoas", "*", NULL);
       if (coafilename != NULL) {
          if ((casefp = fopen(coafilename, "r")) != NULL) {
	     LoadCOAs(casefp);
	     LoadKeys(casefp);
             fclose(casefp); 
             fl_show_object(fd_cbpinput->cbp_view);
             fl_show_object(fd_cbpinput->cbp_view_arrow);
          } else {
             fl_show_messages("Error: Unable to open Course-of-Action file");
          }
       }
       break;

     case 5: // Search COA Cases
       fl_show_object(fd_cbpinput->cbp_view);
       fl_show_object(fd_cbpinput->cbp_view_arrow);
       break;

     default:
       break;
   }
*/
}

void coaviewCB(FL_OBJECT *ob, long data)
{
FILE            *casefp, *targetfp;
int             i, j, k, Rindex, item;
Window          winid;

   if (data < 0)
     item = 0;
   else
     item = fl_get_menu(fd_cbpinput->cbp_view)-1;

   switch (item) {
     case 0: /* handle the COA Output screen */
       fl_set_input(fd_cbpcoaview->cbpcoa_caseid, CaseID);

       fl_winposition(COAwinX, COAwinY);
       fl_initial_winsize(COAwinW, COAwinH);
       winid = fl_prepare_form_window(fd_cbpcoaview->cbpcoaview,
                                     FL_PLACE_POSITION,FL_NOBORDER, "Planning-Match");
       fl_winreparent(winid, COAwinid);
       fl_show_form_window(fd_cbpcoaview->cbpcoaview);
       StoreActiveEntry("Planning-Match");
       break;

     case 1: /* handle the COA Map screen */
       fl_set_input(fd_cbpmapview->cbpmap_caseid, CaseID);

      if (strcmp(getenv("GPDADOMAIN"), "iow") == 0) {
	for (i=0; i<16; i++) {
	  fl_hide_object(fd_cbpmapview->pix_icon[i]);
	  fl_hide_object(fd_cbpmapview->pix_count[i]);
	}
      }

       fl_winposition(COAwinX, COAwinY);
       fl_initial_winsize(COAwinW, COAwinH);
       winid = fl_prepare_form_window(fd_cbpmapview->cbpmapview,
                                     FL_PLACE_POSITION,FL_NOBORDER, "Planning-MapView");
       fl_winreparent(winid, COAwinid);
       fl_show_form_window(fd_cbpmapview->cbpmapview);
       StoreActiveEntry("Planning-MapView");
       break;

     case 2: // Plan View
       coaplanCB(NULL, 1);
       break;

     default:
       fprintf(stderr, "CBP: Invalid window request (%d)\n", item);
       break;
   }
}

void COAexitCB(FL_OBJECT *ob, long data)
{
int             item = data;

   switch (item) {
     case 0: /* handle the COA Output screen */
       fl_hide_form(fd_cbpcoaview->cbpcoaview);
       EraseActiveEntry("Planning-Match");
       break;

     case 1: /* handle the COA Map screen */
       fl_hide_form(fd_cbpmapview->cbpmapview);
       EraseActiveEntry("Planning-MapView");
       break;

     case 2: /* handle the COA rDE screen */
       fl_hide_form(fd_cbprdeinput->cbprdeinput);
       EraseActiveEntry("Planning-Tasks");
       break;

     case 3: /* handle the COA rDE screen */
       fl_hide_form(fd_cbpplan->cbpplan);
       EraseActiveEntry("Planning-COA");
       break;

     case 8: /* handle the COA rDE text screen */
       fl_hide_form(fd_cbprdetext->cbprdetext);
       break;

     default:
       break;
   }
}

void rdetextCB(FL_OBJECT *ob, long data)
{

   fl_load_browser(fd_cbprdetext->cbprdedisclaim, "./IDSrules/snort.text");

   fl_show_form(fd_cbprdetext->cbprdetext, FL_PLACE_CENTER,FL_FULLBORDER,
		"Rules Help");
}

void rdeactionCB(FL_OBJECT *ob, long data)
{
FILE            *casefp;
int             Rindex, i, item = data;
char            filename[64];

   Rindex = fl_get_browser(fd_cbprdeinput->ro_browser) - 1;
 
   switch (item) {
     case 0: // Add
       if (!RespOptions[Rindex].Active) {
         RespOptions[Rindex].Active = TRUE;
         RespActive++;
         sprintf(cbptemp, "@b%-24s %-12s %-12s %-4s %6.1f %6.1f %6.1f %6.1f %6.1f %6.1f %6.1f",
	       RespOptions[Rindex].Option, RespOptions[Rindex].Type,
	       RespOptions[Rindex].Arena,  RespOptions[Rindex].Weapon,
	       RespOptions[Rindex].SVbr,   RespOptions[Rindex].ARbr,
	       RespOptions[Rindex].DMbr,   RespOptions[Rindex].SVrb,
	       RespOptions[Rindex].ARrb,   RespOptions[Rindex].DMrb,
	       RespOptions[Rindex].rDE);
         fl_replace_browser_line(fd_cbprdeinput->ro_browser, Rindex+1, cbptemp);
       }
       break;

     case 1: // Remove
       if (RespOptions[Rindex].Active) {
         RespOptions[Rindex].Active = FALSE;
         RespActive--;
         sprintf(cbptemp, "%-24s %-12s %-12s %-4s %6.1f %6.1f %6.1f %6.1f %6.1f %6.1f %6.1f",
	       RespOptions[Rindex].Option, RespOptions[Rindex].Type,
	       RespOptions[Rindex].Arena,  RespOptions[Rindex].Weapon,
	       RespOptions[Rindex].SVbr,   RespOptions[Rindex].ARbr,
	       RespOptions[Rindex].DMbr,   RespOptions[Rindex].SVrb,
	       RespOptions[Rindex].ARrb,   RespOptions[Rindex].DMrb,
	       RespOptions[Rindex].rDE);
         fl_replace_browser_line(fd_cbprdeinput->ro_browser, Rindex+1, cbptemp);
       }       
       break;

     case 2: // Clear
       fl_clear_browser(fd_cbprdeinput->ro_browser);
       RespActive = 0;
       for (i=0; i<RespCount; i++) RespOptions[i].Active = FALSE;
       break;

     case 3: // Reload
       
       break;

     case 4: // Save
       sprintf(filename, "%s", "ActiveOptions.dat");
       if ((casefp = fopen(filename, "w")) != NULL) {
          fprintf(casefp, "%5d\n", RespActive);
	  for (Rindex=0; Rindex<RespCount; Rindex++) {
	    if (RespOptions[Rindex].Active) {
	      fprintf(casefp, "%-24s %-12s %-12s %-4s %6.1f %6.1f %6.1f %6.1f %6.1f %6.1f %6.1f\n",
		     RespOptions[Rindex].Option, RespOptions[Rindex].Type,
		     RespOptions[Rindex].Arena,  RespOptions[Rindex].Weapon,
		     RespOptions[Rindex].SVbr,   RespOptions[Rindex].ARbr,
		     RespOptions[Rindex].DMbr,   RespOptions[Rindex].SVrb,
		     RespOptions[Rindex].ARrb,   RespOptions[Rindex].DMrb,
		     RespOptions[Rindex].rDE);
	    }
          }
	  fclose(casefp);
       } else
	  fprintf(stderr, "Unable to open Active Options file: %s\n", filename);       
       break;

     default:
       break;
   }
}

void rdeconstraintCB(FL_OBJECT *ob, long data)
{
FILE            *casefp;
int             Rindex, i, item = data;
char            filename[64];

//Rindex = fl_get_browser(fd_cbprdeinput->rde_constraints) - 1;
}

void cbpengageCB(FL_OBJECT *ob, long data)
{
int      pid;
  
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
/*                                                                       */
/* --------------------------------------------------------------------- */
/*           D A M A G E   E X P E C T A N C Y   R O U T I N E S         */
/* --------------------------------------------------------------------- */
/*                                                                       */
void RDEnoneCB(FL_OBJECT *ob, long data)
{
  /* fill-in code for callback */
}

void rdehelpCB(FL_OBJECT *ob, long data)
{

   if (fl_mouse_button() == FL_RIGHTMOUSE) {
     fl_set_form_position(fd_cbpview->cbpview, COAwinX, COAwinY);
     fl_set_form_size(fd_cbpview->cbpview, COAwinW, COAwinH); 
     fl_show_form(fd_cbpview->cbpview, FL_PLACE_POSITION,FL_NOBORDER, "View Text");
     fl_raise_form(fd_cbpview->cbpview);
     sprintf(cbptemp, "JCSorders/%s", fl_get_object_label(fd_cbprdeinput->rde_jcscoa[data]));
     fl_load_browser(fd_cbpview->view_browser, cbptemp);
   } else {
     if ((int)data >= RespCount) return;
     /*
     fl_set_input(fd_cbprdehelp->rdehelp_name,   RespOptions[(int)data].Option);
     fl_set_input(fd_cbprdehelp->rdehelp_weapon, RespOptions[(int)data].Weapon);
     fl_set_input(fd_cbprdehelp->rdehelp_locale, RespOptions[(int)data].Where);
     fl_set_input(fd_cbprdehelp->rdehelp_arena,  RespOptions[(int)data].Arena);
     fl_set_input(fd_cbprdehelp->rdehelp_type,   RespOptions[(int)data].Type);
     fl_set_input(fd_cbprdehelp->rdehelp_class,  RespClass[RespOptions[(int)data].Class]);
     fl_set_input(fd_cbprdehelp->rdehelp_weight, RespWeight[RespOptions[(int)data].Value]);
     */
     fl_show_form(fd_cbprdehelp->cbprdehelp,FL_PLACE_CENTERFREE,FL_FULLBORDER,"Response Info");
   }
}

void rdehelpexitCB(FL_OBJECT *ob, long data)
{
   fl_hide_form(fd_cbprdehelp->cbprdehelp);   
}




