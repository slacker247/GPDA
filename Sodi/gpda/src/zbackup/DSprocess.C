
#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <math.h> 
#include <time.h>
#include <signal.h>
#include <sys/ioctl.h>
#ifdef SC_THREADS
#include "pthread.h"
#endif
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
#include <rpc/xdr.h>
#define SIGCHILD SIGCLD 
/*
 *   Include the SimCmdr stuff
 */
#include "forms.h"
#include "DSforms.h"
#include "dempster.h"
#include "FOGtypes.h"

#define YES      1
#define NO       0
#define POLLRATE 5000
/*
 *   Dempster-Shafer belief network stuff
 */
struct assesstype assesstypes[10];
struct evid       evidences[50];
int           num_types;                   // No. of assessment types
int           curr_type;                   // Current assessment type
int           num_sources;                 // No. of evidence sources
int           curr_source;                 // Current evidence source
int           num_hyp;                     // No. of hypothesis
int           curr_hyp;                    // Current hypothesis
int           num_assess;                  // No. of assessments
int           curr_assess;                 // Current assessment
int           save_assess;
int           num_param;                   // No. of parameters
int           curr_param;                  // Current parameter
int           num_evid;
int           curr_evid;

int           pushed = 1;
char          chtemp[128];
char          dsline[1280];
char          chblank[2];
char          chTstart[32];
int           time_unit;
int           case_id = 1111;
int           evid_item;
int           browser_line = 0;
int           browser_mod  = 0;            // 0=> 'add', 1=> 'modify'

char          chsource[16];
char          chdescript[128];
float         confidence;
float         deltaT;
float         latitude, longitude, altitude;

float         Xtime[50];
float         Ybelief[4][50];
float         Yplause[4][50];

FD_input      *fd_input;
FD_assess     *fd_assess;
FD_network    *fd_network;
FD_assessadd  *fd_assessadd;

FL_OBJECT     *ipm, *npm, *apm, *hpm;
FL_OBJECT     *graph;
FL_OBJECT     *timeunit;

/* --------------------------------------------------------------------- */

void DSinit();
void DSevid();
void DSload(const char *file);
void DSsave(const char *file);
void DSprocess(char *inbuf, char *outbuf, unsigned int *xdrsize);
void DS_Algo(int, float, float);
void DS_AlgoInit();
void DS_AlgoGetResult(int ix, int iy, float results[]);

extern int SC_get_winxpos();
extern int SC_get_winypos();

const char *filter(FL_OBJECT *ob, double value, int prec);

/* --------------------------------------------------------------------- */

void
DSinit()
{
int   i,j;
float xx[70], y1[70], y2[70];
char  chtemp[128];

   fd_input = create_form_input();
   fd_assess = create_form_assess();
   fd_network = create_form_network();
   fd_assessadd = create_form_assessadd();
//
// Put the Gearhead on all forms
//
   /*
   ipm = fd_input->SC_icon;
   fl_show_object(ipm);
   fl_free_pixmap_pixmap(ipm);
   fl_set_pixmap_file(ipm, "SmallHead.xpm");
   timeunit = fd_input->sec_check;
   fl_set_button(timeunit, pushed);

   apm = fd_assess->SC_icon;
   fl_show_object(apm);
   fl_free_pixmap_pixmap(apm);
   fl_set_pixmap_file(apm, "SmallHead.xpm");
   timeunit = fd_assess->ass_sec_check;
   fl_set_button(timeunit, pushed);

   npm = fd_network->SC_icon;
   fl_show_object(npm);
   fl_free_pixmap_pixmap(npm);
   fl_set_pixmap_file(npm, "SmallHead.xpm");

   hpm = fd_assessadd->SC_icon;
   fl_show_object(hpm);
   fl_free_pixmap_pixmap(hpm);
   fl_set_pixmap_file(hpm, "SmallHead.xpm");
   */
//
// Display the CASE id in all forms
//
   sprintf(chtemp, "%d", case_id);
   fl_set_input(fd_input->input_caseid, chtemp);
   fl_set_object_label(fd_assess->case_id, chtemp);
   fl_set_object_label(fd_network->case_id, chtemp);
//
// Define the look of the browsers
//
   fl_set_browser_fontsize(fd_input->input_browser, FL_MEDIUM_SIZE);
   fl_set_browser_fontstyle(fd_input->input_browser, FL_FIXED_STYLE|FL_BOLD_STYLE);
   fl_set_browser_fontstyle(fd_assess->ass_browser, FL_FIXED_STYLE);
//
//----------------------------------------------------------------------------------
//
// Define the initial Assessment types
//
   num_types = 7;
   curr_type = 0;
   strcpy(assesstypes[0].chname, "Foreign Launch");
   strcpy(assesstypes[1].chname, "Weapon Release");
   strcpy(assesstypes[2].chname, "Situational Assessment");
   strcpy(assesstypes[3].chname, "Strike Assessment");
   strcpy(assesstypes[4].chname, "Mission Assessment");
   strcpy(assesstypes[5].chname, "Mission Readiness");
   strcpy(assesstypes[6].chname, "Decision Fusion");
   for (i=0; i<num_types; i++)
       fl_addto_choice(fd_input->asstype_menu,assesstypes[i].chname);
   fl_set_object_label(fd_assess->assess_type, assesstypes[curr_type].chname);
   fl_set_object_label(fd_network->assess_type, assesstypes[curr_type].chname);
//
// Define the initial data Sources (of evidence) for all Assessment types
//
   num_sources = 6;
   curr_source = 0;
   for (i=0; i<num_types; i++) {
       strcpy(assesstypes[i].sources[0].chsource, "SBIRS");
       strcpy(assesstypes[i].sources[1].chsource, "STRAT");
       strcpy(assesstypes[i].sources[2].chsource, "Political");
       strcpy(assesstypes[i].sources[3].chsource, "Intel");
       strcpy(assesstypes[i].sources[4].chsource, "Radar");
       strcpy(assesstypes[i].sources[5].chsource, "Fire");
   }
   for (i=0; i<num_sources; i++) {
       fl_addto_choice(fd_assessadd->add_src_choice, assesstypes[curr_type].sources[i].chsource);
       fl_set_object_label(fd_assess->assess_round[i], assesstypes[curr_type].sources[i].chsource);
       fl_set_object_label(fd_network->src_title[i], assesstypes[curr_type].sources[i].chsource);
       fl_set_slider_bounds(fd_network->netw_src_belief[i+1], 0.0, 1.0);
       fl_set_slider_bounds(fd_network->netw_src_plause[i+1], 0.0, 1.0);
       fl_set_slider_bounds(fd_network->netw_src_unknown[i+1], 0.0, 1.0);
       fl_set_slider_value(fd_network->netw_src_belief[i+1], 0.0);
       fl_set_slider_value(fd_network->netw_src_plause[i+1], 0.0);
       fl_set_slider_value(fd_network->netw_src_unknown[i+1], 0.0);
   }
   fl_set_button(fd_assess->assess_round[0], pushed);
//
// Initialize the default Hypothesis for all Assessment types
//
   num_hyp = 4;
   curr_hyp = 0;
   for (j=0; j<num_types; j++) {
     strcpy(assesstypes[j].hypothesis[0].chhypoth, "Foreign Launch");
     strcpy(assesstypes[j].hypothesis[1].chhypoth, "Anticipated");
     strcpy(assesstypes[j].hypothesis[2].chhypoth, "Friendly Impact");
     strcpy(assesstypes[j].hypothesis[3].chhypoth, "Space Launch");
   }
   for (i=0; i<num_hyp; i++) {
      fl_set_object_label(fd_network->hyp_title[i], assesstypes[curr_type].hypothesis[i].chhypoth);
      fl_set_slider_bounds(fd_network->netw_hyp_belief[i+1], 0.0, 1.0);
      fl_set_slider_bounds(fd_network->netw_hyp_plause[i+1], 0.0, 1.0);
      fl_set_slider_bounds(fd_network->netw_hyp_unknown[i+1], 0.0, 1.0);
      fl_set_slider_value(fd_network->netw_hyp_belief[i+1], 0.0);
      fl_set_slider_value(fd_network->netw_hyp_plause[i+1], 0.0);
      fl_set_slider_value(fd_network->netw_hyp_unknown[i+1], 0.0);
   }
//
// Initialize the default Assessment for all Assessment types
//
   num_assess = 3;
   curr_assess = 0;
   for (j=0; j<num_types; j++) {
     strcpy(assesstypes[j].assessment[0].chassess, "Hostile");      // Hostile or Friendly?
     strcpy(assesstypes[j].assessment[1].chassess, "Deliberate");   // Accidental?
     strcpy(assesstypes[j].assessment[2].chassess, "ICBM");         // Space or Missile?
     assesstypes[j].assessment[0].belief = 90.0;
     assesstypes[j].assessment[0].plause = 95.0;
     assesstypes[j].assessment[0].Bthreshold = 75.0;
     assesstypes[j].assessment[0].Tthreshold = 100.0;
     assesstypes[j].assessment[0].select = 0;
     assesstypes[j].assessment[0].piecolor = FL_RED;
     assesstypes[j].assessment[1].belief = 5.0;
     assesstypes[j].assessment[1].plause = 10.0;
     assesstypes[j].assessment[1].Bthreshold = 75.0;
     assesstypes[j].assessment[1].Tthreshold = 100.0;
     assesstypes[j].assessment[1].select = 1;
     assesstypes[j].assessment[1].piecolor = FL_BLUE;
     assesstypes[j].assessment[2].belief = 1.0;
     assesstypes[j].assessment[2].plause = 1.0;
     assesstypes[j].assessment[2].Bthreshold = 75.0;
     assesstypes[j].assessment[2].Tthreshold = 100.0;
     assesstypes[j].assessment[2].select = 2;
     assesstypes[j].assessment[2].piecolor = FL_YELLOW;
   }
   for (i=0; i<num_assess; i++) {
      fl_set_slider_bounds(fd_network->netw_ass_belief[i+1], 0.0, 1.0);
      fl_set_slider_bounds(fd_network->netw_ass_plause[i+1], 0.0, 1.0);
      fl_set_slider_bounds(fd_network->netw_ass_unknown[i+1], 0.0, 1.0);
      fl_set_slider_value(fd_network->netw_ass_belief[i+1], 0.0);
      fl_set_slider_value(fd_network->netw_ass_plause[i+1], 0.0);
      fl_set_slider_value(fd_network->netw_ass_unknown[i+1], 0.0);
      fl_add_chart_value(fd_assess->belief_pie,
          (double)assesstypes[curr_type].assessment[i].belief, "",
                  assesstypes[curr_type].assessment[i].piecolor);
      fl_add_chart_value(fd_assess->plause_pie,
          (double)assesstypes[curr_type].assessment[i].plause, "",
                  assesstypes[curr_type].assessment[i].piecolor);
   }

   fl_set_object_label(fd_network->netw_ass1_text,assesstypes[curr_type].assessment[0].chassess);
   fl_set_object_label(fd_network->netw_ass2_text,assesstypes[curr_type].assessment[1].chassess);
   fl_set_object_label(fd_network->netw_ass3_text,assesstypes[curr_type].assessment[2].chassess);

   fl_set_object_label(fd_assess->ass_ass1_text, assesstypes[curr_type].assessment[0].chassess);
   fl_set_object_label(fd_assess->ass_ass2_text, assesstypes[curr_type].assessment[1].chassess);
   fl_set_object_label(fd_assess->ass_ass3_text, assesstypes[curr_type].assessment[2].chassess);
//
//   Display an example graph
//
   graph = fd_assess->conf_plot;
   fl_set_xyplot_overlay_type(graph, 1, FL_CIRCLE_XYPLOT);
   fl_set_xyplot_overlay_type(graph, 2, FL_DASHED_XYPLOT);
   fl_set_xyplot_overlay_type(graph, 3, FL_DASHED_XYPLOT);
   fl_set_xyplot_xbounds(graph, 0, 175);
   fl_set_xyplot_ybounds(graph, 0, 1.4);
   fl_set_xyplot_ytics(graph, 14, 0);

   xx[0] = 0.0;
   y1[0] = 0.0;
   fl_set_xyplot_data(graph, xx, y1, 1, "Confidence of Assessment","Time (sec)","%");
   fl_add_xyplot_text(graph, 160, 1.2,
                      assesstypes[curr_type].assessment[curr_assess].chassess,
                      FL_ALIGN_LEFT,
                      assesstypes[curr_type].assessment[curr_assess].piecolor);
   save_assess = curr_assess;

   return;
}

const char *filter(FL_OBJECT *ob, double value, int prec)
{
static char buf[32];

  sprintf(buf, "%d", value);
  return buf;
}

void
DSevid()
{

   DSload("DSinitevid.evid");
}

void
DSload(const char *filename)
{
int   i,j;
float fogfactors[2];
char  chtemp[128];
FILE  *EVIDfp;
//
//   Get the evidence
//
   fl_clear_browser(fd_input->input_browser);

   EVIDfp = fopen(filename, "r+w");
   fscanf(EVIDfp, "%d %s", &num_evid, chTstart);
   for (i=0; i<num_evid; i++) {
       fscanf(EVIDfp, "%s %f %f %f %f %f %s", chsource, &confidence, &deltaT, &latitude,
              &longitude, &altitude, chdescript);

       if (FOGenabled()) {
	 if (FOGget(FOG_LOSS, FOG_EVIDENCE, fogfactors) > 0)
	   confidence = confidence - fogfactors[1]*confidence;
       }

       strcpy(evidences[i].chsource, chsource);
       strncpy(evidences[i].chdescript, chdescript,20);
       evidences[i].latitude   = latitude;
       evidences[i].longitude  = longitude;
       evidences[i].altitude   = altitude;
       evidences[i].deltaT     = deltaT;
       evidences[i].confidence = confidence;

       sprintf(chtemp, "%-20s %20.5f %30.5f", chsource, confidence, deltaT);
       fl_addto_browser(fd_input->input_browser, chtemp);
   }
   fclose(EVIDfp);
   curr_evid = 0;
   fl_set_object_label(fd_input->sdate_text, chTstart);
   fl_set_object_label(fd_assess->sdate_text, chTstart);
   fl_set_object_label(fd_network->sdate_text, chTstart);
}

void
DSsave(const char *filename)
{
int   i,j;
char  chtemp[128];
FILE  *SAVEfp;
//
//   Save the evidence
//
   SAVEfp = fopen(filename, "w+");
   fprintf(SAVEfp, "%5d %s\n", num_evid, chTstart);
   for (i=0; i<num_evid; i++) {
       fprintf(SAVEfp, "%-20s  %f  %f  %f  %f  %f  %20s\n",
              evidences[i].chsource, evidences[i].confidence, evidences[i].deltaT,
              evidences[i].latitude, evidences[i].longitude,  evidences[i].altitude,
              evidences[i].chdescript);
   }
   fclose(SAVEfp);
}

void
DSsaveinit(const char *filename)
{
FILE      *SAVEfp;
time_t    clock;
struct tm *ltime;
char      *str;
int       i,j;

   time(&clock);
   ltime = localtime(&clock);
   str = asctime(ltime);

   SAVEfp = fopen(filename, "w+");
   //fprintf(SAVEfp, "%s                        //File_Creation_DATE\n", str);
   fprintf(SAVEfp, "%5d                       //Case_ID\n", case_id);
   fprintf(SAVEfp, "%20s         //Start_time\n", "1400Z");

   fprintf(SAVEfp, "%5d                       //Assessment_Types\n", num_types);
   for (i=0; i<num_types; i++) {
       fprintf(SAVEfp, "%s\n", assesstypes[i].chname);
       fprintf(SAVEfp, "  %5d                     //Evidence_Sources\n", num_sources);
       for (j=0; j<num_sources; j++)
           fprintf(SAVEfp, "  %s\n", assesstypes[i].sources[j].chsource);

       fprintf(SAVEfp, "  %5d                     //Hypothesis\n", num_hyp);
       for (j=0; j<num_hyp; j++)
           fprintf(SAVEfp, "  %s\n", assesstypes[i].hypothesis[j].chhypoth);

       fprintf(SAVEfp, "  %5d                     //Assessments\n", num_assess);
       for (j=0; j<num_assess; j++)
           fprintf(SAVEfp, "  %s\n", assesstypes[i].assessment[j].chassess,
                   assesstypes[i].assessment[j].belief,
                   assesstypes[i].assessment[j].plause);
   }

}

/* ----------------------- Dempster-Shafer CB's ----------------------- */

/* callbacks for form input */
void input_exitCB(FL_OBJECT *object, long item_no)
{
int item = 0;

   fl_hide_form(fd_input->input);
}

void input_menuCB(FL_OBJECT *object, long item_no)
{
int item = 0;

   curr_type = fl_get_choice(fd_input->asstype_menu)-1;
   printf("Input assess type menu %d\n", curr_type);
   fl_set_object_label(fd_assess->assess_type, assesstypes[curr_type].chname);
   fl_set_object_label(fd_network->assess_type, assesstypes[curr_type].chname); 
}

void input_saveCB(FL_OBJECT *object, long item_no)
{
int        item = 0;
const char *otfilename;

   sprintf(chtemp, "DS%4.4d.evid", case_id);
   otfilename = fl_show_fselector("Save file", "./", "*", chtemp);
   if (otfilename != NULL) {
     DSsave(otfilename);
   }
}

void input_loadCB(FL_OBJECT *object, long item_no)
{
int        item = 0;
const char *infilename;

   sprintf(chtemp, "DS%4d.rule", 0);
   infilename = fl_show_fselector("Load file", "./", "*", chtemp);
   if (infilename != NULL) {
     DSload(infilename);
   }
}

void input_browserCB(FL_OBJECT *object, long item_no)
{
int item = 0;

   browser_line = fl_get_browser(fd_input->input_browser);
   if (browser_line < 0) browser_line = 0;
}

void input_modCB(FL_OBJECT *object, long item_no)
{
int item = 0;

   if (browser_line > 0) {
       fl_show_form(fd_assessadd->assessadd,FL_PLACE_CENTER,FL_FULLBORDER,
                    "Modify Evidence");
       browser_mod = 1;

       confidence = evidences[browser_line-1].confidence;
       sprintf(dsline, "%f", confidence);
       fl_set_input(fd_assessadd->add_confid, dsline);

       deltaT = evidences[browser_line-1].deltaT;
       sprintf(dsline, "%f", deltaT);
       fl_set_input(fd_assessadd->add_time, dsline);

       latitude = evidences[browser_line-1].latitude;
       sprintf(dsline, "%f", latitude);
       fl_set_input(fd_assessadd->add_lat, dsline);

       longitude = evidences[browser_line-1].longitude;
       sprintf(dsline, "%f", longitude);
       fl_set_input(fd_assessadd->add_long, dsline);

       altitude = evidences[browser_line-1].altitude;
       sprintf(dsline, "%f", altitude);
       fl_set_input(fd_assessadd->add_alt, dsline);

       strncpy(chdescript, evidences[browser_line-1].chdescript, 20);
       fl_set_input(fd_assessadd->add_desc, chdescript);
   }
}

void input_addCB(FL_OBJECT *object, long item_no)
{
int item = 0;

   fl_show_form(fd_assessadd->assessadd,FL_PLACE_CENTER,FL_FULLBORDER,
                "Add Evidence");
   browser_mod = 0;
}

void input_delCB(FL_OBJECT *object, long item_no)
{
int item = 0;

   if (browser_line > 0)
       fl_delete_browser_line(fd_input->input_browser, browser_line);
}

void input_execCB(FL_OBJECT *object, long item_no)
{
int item = 0;

}

void input_assessCB(FL_OBJECT *object, long item_no)
{
int item = 0;

   fl_set_form_position(fd_assess->assess, SC_get_winxpos(), SC_get_winypos() );
   fl_set_form_size(fd_assess->assess, 850, 460);
   fl_show_form(fd_assess->assess, FL_PLACE_POSITION,FL_NOBORDER, "Dempster-Shafer Assess");
   fl_raise_form(fd_assess->assess);
}

void input_netwCB(FL_OBJECT *object, long item_no)
{
int item = 0;

   fl_set_form_position(fd_network->network, SC_get_winxpos(), SC_get_winypos() );
   fl_set_form_size(fd_network->network, 850, 460);
   fl_show_form(fd_network->network, FL_PLACE_POSITION,FL_NOBORDER, "Dempster-Shafer Network");
   fl_raise_form(fd_network->network);
}

void input_caseidCB(FL_OBJECT *object, long item_no)
{
int        item = 0;
const char *chinput;

   chinput = fl_get_input(fd_input->input_caseid);    
   sscanf(chinput, "%d", &case_id);
   fl_set_object_label(fd_assess->case_id, chinput);
   fl_set_object_label(fd_network->case_id, chinput);
}

/* callbacks for form assess */
void assess_exitCB(FL_OBJECT *object, long item_no)
{
int item = 0;

   fl_hide_form(fd_assess->assess);
}

void assess_netwCB(FL_OBJECT *object, long item_no)
{
int item = 0;

   fl_hide_form(fd_assess->assess);
   fl_set_form_position(fd_network->network, SC_get_winxpos(), SC_get_winypos() );
   fl_set_form_size(fd_network->network, 850, 460);
   fl_show_form(fd_network->network, FL_PLACE_POSITION,FL_NOBORDER, "Dempster-Shafer Network");
   fl_raise_form(fd_network->network);
}

void assess_selectCB(FL_OBJECT *object, long item_no)
{
int   item = 0, i;
float Yb[50], Yp[50], Xt[50], Xover2[2], Yover2[2], Xover3[2], Yover3[2];
char  chtemp[8];

   curr_assess = item_no;
   //j = curr_evid - 1;

   for (i=0; i<curr_evid; i++) {
      Yb[i] = Ybelief[curr_assess][i];
      Yp[i] = Yplause[curr_assess][i];
      Xt[i] = Xtime[i];
   }

   graph = fd_assess->conf_plot;
   //fl_set_xyplot_overlay_type(graph, 1, FL_CIRCLE_XYPLOT);
   //fl_set_xyplot_overlay_type(graph, 2, FL_DASHED_XYPLOT);
   //fl_set_xyplot_overlay_type(graph, 3, FL_DASHED_XYPLOT);
   //fl_set_xyplot_xbounds(graph, 0, 175);
   //fl_set_xyplot_ybounds(graph, 0, 1.4);
   //fl_set_xyplot_interpolate(graph, 1, 2, 0.1);
   //fl_set_xyplot_fixed_yaxis(graph, "ii", "ii");

   fl_set_xyplot_data(graph, Xt, Yb, curr_evid, "Confidence of Assessment",
                      "Time (min)","%");
   fl_add_xyplot_overlay(graph, 1, Xt, Yp, curr_evid, FL_BLUE);

   Xover2[0] = 0.0;
   Yover2[0] = assesstypes[curr_type].assessment[curr_assess].Bthreshold/100.0;
   Xover2[1] = assesstypes[curr_type].assessment[curr_assess].Tthreshold;
   Yover2[1] = assesstypes[curr_type].assessment[curr_assess].Bthreshold/100.0;
   fl_add_xyplot_overlay(graph, 2, Xover2, Yover2, 2, FL_YELLOW);

   Xover3[0] = assesstypes[curr_type].assessment[curr_assess].Tthreshold;
   Yover3[0] = 0.0;
   Xover3[1] = assesstypes[curr_type].assessment[curr_assess].Tthreshold;
   Yover3[1] = assesstypes[curr_type].assessment[curr_assess].Bthreshold/100.0;
   fl_add_xyplot_overlay(graph, 3, Xover3, Yover3, 2, FL_YELLOW);

   fl_delete_xyplot_text(graph,
                      assesstypes[curr_type].assessment[save_assess].chassess);
   fl_add_xyplot_text(graph, 160, 1.2,
                      assesstypes[curr_type].assessment[curr_assess].chassess,
                      FL_ALIGN_LEFT,
                      assesstypes[curr_type].assessment[curr_assess].piecolor);
   save_assess = curr_assess; 
}

void assess_tunitCB(FL_OBJECT *object, long item_no)
{
int item = 0;

   time_unit = item_no;

}

void assess_issueCB(FL_OBJECT *object, long item_no)
{
int item = 0;


}

void evidenceCB(FL_OBJECT *object, long item_no)
{
int item = 0;

}

/* callbacks for form network */
void netw_exitCB(FL_OBJECT *object, long item_no)
{
int item = 0;

   fl_hide_form(fd_network->network);
}

void netw_assessCB(FL_OBJECT *object, long item_no)
{
int item = 0;

   fl_hide_form(fd_network->network);
   fl_set_form_position(fd_assess->assess, SC_get_winxpos(), SC_get_winypos() );
   fl_set_form_size(fd_assess->assess, 850, 460);
   fl_show_form(fd_assess->assess, FL_PLACE_POSITION,FL_NOBORDER, "Dempster-Shafer Assess");
   fl_raise_form(fd_assess->assess);
}

void netw_beliefCB(FL_OBJECT *object, long item_no)
{
int item = 0;

}

void netw_plauseCB(FL_OBJECT *object, long item_no)
{
int item = 0;

}

void netw_unknownCB(FL_OBJECT *object, long item_no)
{
int item = 0;

}

/* callbacks for form assessadd */
void add_sourceCB(FL_OBJECT *object, long item_no)
{
int item = 0;

   item = fl_get_choice(fd_assessadd->add_src_choice)-1;
   fl_set_object_label(fd_assessadd->add_source,
                       assesstypes[curr_type].sources[item].chsource);
   strcpy(chsource, assesstypes[curr_type].sources[item].chsource); 
}

void add_getvalCB(FL_OBJECT *object, long item_no)
{
int        item = 0;
const char *ifield;

   switch (item_no)
   {
     case 0:
       confidence = atof(fl_get_input(fd_assessadd->add_confid));
       break;
     case 1:
       deltaT = atof(fl_get_input(fd_assessadd->add_time));
       break;
     case 2:
       latitude = atof(fl_get_input(fd_assessadd->add_lat));
       break;
     case 3:
       longitude = atof(fl_get_input(fd_assessadd->add_long));
       break;
     case 4:
       altitude = atof(fl_get_input(fd_assessadd->add_alt));
       break;
     case 5:
       strncpy(chdescript, fl_get_input(fd_assessadd->add_desc), 20);
       break;
     default:
       break;
   }
}

void add_acceptCB(FL_OBJECT *object, long item_no)
{
int item = 0, i;

   i = num_evid;
   fl_hide_form(fd_assessadd->assessadd);
   strcpy(evidences[i].chsource,  chsource);
   strncpy(evidences[i].chdescript, chdescript, 20);
   evidences[i].latitude   = latitude;
   evidences[i].longitude  = longitude;
   evidences[i].altitude   = altitude;
   evidences[i].deltaT     = deltaT;
   evidences[i].confidence = confidence;   
   sprintf(chtemp, "%-20s %20.5f %30.5f",
               evidences[i].chsource,
               evidences[i].confidence,
               evidences[i].deltaT);
   if (!browser_mod) {
       fl_addto_browser(fd_input->input_browser, chtemp);
       num_evid = num_evid + 1;
   } else {
       fl_replace_browser_line(fd_input->input_browser, browser_line, chtemp);
   }
}

void assadd_cancelCB(FL_OBJECT *object, long item_no)
{
int item = 0;

   fl_hide_form(fd_assessadd->assessadd);
}

/* callbacks for common forms */
void populateCB(FL_OBJECT *object, long item_no)
{
int    item = 0, i, j, k;
int    prop_grayed = FALSE;
char   chsource[40];
char   chhypoth[40];
float  confidence, deltaT, results[5];
const char *chtemp;
char   chline[40];

   curr_evid = curr_evid + 1;
   j = curr_evid - 1;

   if (curr_evid > num_evid) {
      printf("Warning, no more evidence to process. Request ignored\n");
      curr_evid = curr_evid - 1;
      return;
   }

   if (curr_evid == num_evid) {
      fl_deactivate_object(fd_assess->propagate_button);
      fl_deactivate_object(fd_network->propagate_button);
      fl_set_object_color(fd_assess->propagate_button, FL_INACTIVE_COL, FL_INACTIVE_COL);
      fl_set_object_color(fd_network->propagate_button, FL_INACTIVE_COL, FL_INACTIVE_COL);
   }
//
//   Get the next piece of evidence from the database
//
   fl_select_browser_line(fd_input->input_browser, curr_evid);
   chtemp = fl_get_browser_line(fd_input->input_browser, curr_evid);
   sscanf(chtemp, "%s %f %f", chsource, &confidence, &deltaT);
   item = -1;
   for (i=0; i<num_sources; i++) {
      if (strcmp(chsource, assesstypes[curr_type].sources[i].chsource) == 0)
          item = i+1;
   }
   //
   //   If evidence available, process it.
   //
   if (item != -1) {
     //printf("Evidence match found. Process %d %s %f %f\n", item, chsource, confidence, deltaT);
     DS_Algo(item, confidence, deltaT);
     //
     //   Process the Assessment displays
     //
     for (i=0; i<num_assess; i++) {
       DS_AlgoGetResult(i+1, 1, results);
       //printf(" %f %f %f %f %d\n", results[1],results[2],results[3],results[4],curr_evid);
       Ybelief[i][j] = results[2];
       Yplause[i][j] = results[3];
     }
     Xtime[j] = deltaT; //results[4];

     sprintf(chline, "%f", deltaT);
     fl_set_object_label(fd_assess->cdate_text, chline);
     fl_set_object_label(fd_network->cdate_text, chline);

     sprintf(chline, "%d", (int)(Ybelief[0][j]*100.0+0.5));
     fl_set_object_label(fd_assess->assess_ass1_belief, chline);
     sprintf(chline, "%d", (int)(Ybelief[1][j]*100.0+0.5));
     fl_set_object_label(fd_assess->assess_ass2_belief, chline);
     sprintf(chline, "%d", (int)(Ybelief[2][j]*100.0+0.5));
     fl_set_object_label(fd_assess->assess_ass3_belief, chline);

     sprintf(chline, "%d", (int)(Yplause[0][j]*100.0+0.5));
     fl_set_object_label(fd_assess->assess_ass1_plause, chline);
     sprintf(chline, "%d", (int)(Yplause[1][j]*100.0+0.5));
     fl_set_object_label(fd_assess->assess_ass2_plause, chline);
     sprintf(chline, "%d", (int)(Yplause[2][j]*100.0+0.5));
     fl_set_object_label(fd_assess->assess_ass3_plause, chline);

     for (i=0; i<num_assess; i++) {
        fl_set_slider_value(fd_network->netw_ass_belief[i+1], (double)(Ybelief[i][j]+0.005));
        fl_set_slider_value(fd_network->netw_ass_plause[i+1], (double)(Yplause[i][j]+0.005));
        fl_replace_chart_value(fd_assess->belief_pie, i+1, (double)Ybelief[i][j], "",
                  assesstypes[curr_type].assessment[i].piecolor);
        fl_replace_chart_value(fd_assess->plause_pie, i+1, (double)Yplause[i][j], "",
                  assesstypes[curr_type].assessment[i].piecolor);
     }
     /*
     sprintf(chline, "Belief %d\nPlause %d", (int)(Ybelief[0][j]*100.0+0.5),
                                             (int)(Yplause[0][j]*100.0+0.5));
     fl_set_object_label(fd_network->ass1_value, chline);
     sprintf(chline, "Belief %d\nPlause %d", (int)(Ybelief[1][j]*100.0+0.5),
                                             (int)(Yplause[1][j]*100.0+0.5));
     fl_set_object_label(fd_network->ass2_value, chline);
     sprintf(chline, "Belief %d\nPlause %d", (int)(Ybelief[2][j]*100.0+0.5),
                                             (int)(Yplause[2][j]*100.0+0.5));
     fl_set_object_label(fd_network->ass3_value, chline);
     */
     assess_selectCB(NULL, (long)curr_assess);

     sprintf(dsline, "%-20s %5.1fE %6.1fN %6.1fKm %7.1fs ",
               evidences[j].chdescript,
               evidences[j].latitude,
               evidences[j].longitude,
               evidences[j].altitude,
               evidences[j].deltaT);
     fl_addto_browser(fd_assess->ass_browser, dsline);
     //
     //   Process Hypothesis displays
     //
     for (i=0; i<num_hyp; i++) {
        k = i+1;
        DS_AlgoGetResult(k, 2, results); 
        fl_set_slider_value(fd_network->netw_hyp_belief[k], (double)(results[2]+0.005));
        fl_set_slider_value(fd_network->netw_hyp_plause[k], (double)(results[3]+0.005));
     }
     //
     //   Process Source displays
     //
     for (i=0; i<num_sources; i++) {
        k = i+1;
        DS_AlgoGetResult(k, 3, results); 
        fl_set_slider_value(fd_network->netw_src_belief[k], (double)(results[2]+0.005));
        fl_set_slider_value(fd_network->netw_src_plause[k], (double)(results[3]+0.005));
     }

   } else {
     printf("Evidence match not found. Request ignored.\n");
     curr_evid = curr_evid - 1;
   }
}

void resetCB(FL_OBJECT *object, long item_no)
{
int item = 0;

   curr_type   = 0;
   curr_source = 0;
   curr_hyp    = 0;
   curr_assess = 0;
   curr_evid   = 0;

   fl_select_browser_line(fd_input->input_browser, curr_evid);
   fl_clear_browser(fd_assess->ass_browser);
   browser_line = 0;
   browser_mod  = 0;            // 0=> 'add', 1=> 'modify'

   fl_activate_object(fd_assess->propagate_button);
   fl_activate_object(fd_network->propagate_button);
   fl_set_object_color(fd_assess->propagate_button, FL_COL1, FL_INACTIVE_COL);
   fl_set_object_color(fd_network->propagate_button, FL_COL1, FL_INACTIVE_COL);

   DS_AlgoInit();
}

void executeCB(FL_OBJECT *object, long item_no)
{
int item = 0;

   for (item=0; item<num_evid; item++)
      populateCB(NULL, item);
}
