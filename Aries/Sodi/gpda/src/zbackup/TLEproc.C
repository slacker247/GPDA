#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <math.h> 
#include <time.h>
#include <signal.h>
#include <sys/ioctl.h>

#include "forms.h"
#include "TLEforms.h"

#include "Globals.h"
#include "alarms.H"

#include "GL/glx.h" 
#include "GL/gltk.h"
#include "glfont.h"

/* --------------------------------------------------------------------- */
 
#ifndef FALSE
#define FALSE   0
#endif
#ifndef TRUE
#define TRUE    1
#endif 

#define DEGRAD  57.29577951308232
#define RADDEG  0.0174532925199432958

#define logf(x) (float)log((double)x)

typedef struct {
int             id;
int             priority;
float           time;
char            timeunit;
char            type[16];
char            action[16];
char            argument[16];
char            description[64];
char            filename[64];
void            *nextevent;
void            *prevevent;
} EVENT;
 
/* --------------------------------------------------------------------- */
 
int             TLEwinX, TLEwinY;
int             TLEwinW, TLEwinH;
Window          TLEwinid;
char            tletemp[1280];                    // Global scratch space for string operations
char            TLElabel[32];                     // Time Line Enforcer window label

EVENT           *first_event = NULL;              // Pointer to first event in queue
EVENT           *last_event = NULL;               // Pointer to last event in queue
EVENT           *curr_event = NULL;               // Pointer to current event being processed
int             n_events = 0;                     // No. of events in queue
int             TLEtimeoutID;                     // ID of 1 minute timer
int             TLEventimeID;                     // ID of next event Time Out
int             AutoAccept = 0;                   // If set, automagically accept all event TOs
int             StopAccept = 0;                   // If set, stop doing Time Line Enforcement
int             AlarmFilter = 1;                  // Minimum priority of alarms for event TO
long            TimeBasis;                        // Seconds part of time since Jan 00 1970
long            Microseconds;                     // uSec part of time since Jan 00 1970
float           MissionTime;                      // Mission Time (secs)
float           Xbias = 0.0;
int             Xscaling = 0;
float           Xinterval = 6.0;                  // Time axis interval (in minutes)
float           Xleft = -120.0;                   // Time at left edge of viewport
float           Xright = 120.0;                   // Time at right edge of viewport
float           Xpan = 0.0;                       // Viewport offset
int             Freplan = TRUE;                   // If TRUE, draw a replan trigger indicator
float           Treplan = 34.0;                   // Time of replan trigger indicator
float           Xreplan, Yreplan;                 // (x,y) of Replan trigger indicator

char            *tleBITMAPDIR;
char            tlexpm[128];

int             n_Pms = 0;                        // No. of Probability of Mission Success pnts
float           PmsTable[100][2];                 // Pms table (Time, Prob)

FD_tlegraph     *fd_tlegraph;                     // Time Time Enforcer top level form
FD_tleevent     *fd_tleevent;                     // TLE event information
FD_tlereplan    *fd_tlereplan;                    // TLE Replan Trigger explanation
 
/* --------------------------------------------------------------------- */

void TLEinit();
void TLEshow(int xpos, int ypos, int width, int height, Window mainwinID);
int  TLEAddEvent(float time, char tunit, int priority, char* type,
		        char* action, char* arg, char* desc, char* fname);
int  TLERemEvent(int eventid);
void TLEIdle(int tid, void *stuff);

void timelapsedCB(int tid, void *stuff);
void timeacceptCB(FL_OBJECT *ob, long data);

void tlereasonCB(FL_OBJECT *object, long item_no);

//callback events for opengl canvas
int exposeCB(FL_OBJECT *ob, Window win, int w, int h, XEvent *xev, void *ud);
int buttonpressCB(FL_OBJECT *ob, Window win, int w, int h, XEvent *xev, void *ud);
//draw to opengl canvas
void tledrawCB();

extern "C" GLXContext fl_get_glcanvas_context(FL_OBJECT * ob);
extern "C" void SC_update_gvt(float newgvt);
extern "C" void SC_update_def(int newdef);
extern "C" void SC_update_rp(int newrp);
extern "C" void SC_update_dea(char *newdea);
extern "C" void SC_update_roe(char *newroe);
extern "C" void SC_update_plan(char *str);
extern "C" void SC_update_node(char *str);
extern "C" void SC_update_hist(char *str);

extern int  EraseActiveEntry(char *text);
extern int  StoreActiveEntry(char *text);
extern int  FinishUp();

/*                                                                       */
/* --------------------------------------------------------------------- */
/*              M A I N   P R O G R A M   S T A R T S   H E R E          */
/* --------------------------------------------------------------------- */
/*                                                                       */
void TLEinit()
{
FILE            *timefp;
int             i;
int             h, m, s;
int             Ah, Am, As;
char            filename[80];
char            chperiod[16];
char            chline[64];

   fd_tlegraph  = create_form_tlegraph();
   fd_tleevent  = create_form_tleevent();
   fd_tlereplan = create_form_tlereplan();

   strcpy(TLElabel, "Time-Line");

   //fl_set_thumbwheel_return(fd_tlegraph->interval, FL_RETURN_CHANGED);
   fl_set_thumbwheel_value(fd_tlegraph->interval, (double)Xinterval);
   fl_set_thumbwheel_bounds(fd_tlegraph->interval, (double)5.0, (double)120.0);
   fl_set_thumbwheel_step(fd_tlegraph->interval, (double)1.0);

   fl_set_browser_fontsize(fd_tlegraph->event_list, 10);
   fl_set_browser_fontstyle(fd_tlegraph->event_list, FL_FIXED_STYLE);

   //fl_deactivate_object(fd_tlegraph->accept_button);
   //fl_set_object_lcol(fd_tlegraph->accept_button, FL_INACTIVE_COL);

   fl_add_canvas_handler(fd_tlegraph->canvas, Expose,      exposeCB, 0);
   fl_add_canvas_handler(fd_tlegraph->canvas, ButtonPress, buttonpressCB, 0);

   first_event = (EVENT *)new(EVENT);
   first_event->prevevent = NULL;
   first_event->nextevent = NULL;
   curr_event  = first_event;

   fl_gettime(&TimeBasis, &Microseconds);
   MissionTime = 0.0;

   sprintf(filename, "timeline.%s", getenv("GPDADOMAIN"));

   if ((timefp = fopen(filename, "r")) != NULL) {
      fscanf(timefp, "%d %d %d:%d:%d\n", &n_events, &AutoAccept, &Ah, &Am, &As);
      //fprintf(stderr, "No. of events should be %d\n", n_events);
      do fgets(tletemp, 128, timefp); while ((tletemp[0] == '#') && (!feof(timefp)) );
      curr_event  = first_event;

      for (i=0; i<n_events; i++) {
	 curr_event->id = i+1;
         sscanf(tletemp, "%f %c %d %s %s %s %s %s\n",
		&curr_event->time,
                &curr_event->timeunit,
		&curr_event->priority,
		curr_event->type,
		curr_event->action,
                curr_event->argument,
		curr_event->description,
                curr_event->filename);
	 if (curr_event->timeunit == 'M') curr_event->time = curr_event->time * 60.0;
	 if (curr_event->timeunit == 'H') curr_event->time = curr_event->time * 60.0*60.0;
	 if (curr_event->timeunit == 'D') curr_event->time = curr_event->time * 60.0*60.0*24.0;
	 strnsub(curr_event->description, '_', ' ', 64);
	 sprintf(chline, "%6d  %-8s  %s", (int)curr_event->time, curr_event->type,
		 curr_event->description);
	 fl_add_browser_line(fd_tlegraph->event_list, chline);

	 last_event = curr_event;

         curr_event = (EVENT *)new(EVENT);
	 curr_event->prevevent = (void *)last_event;
	 curr_event->nextevent = NULL;
	 last_event->nextevent = curr_event;
         do fgets(tletemp, 128, timefp); while ((tletemp[0] == '#') && (!feof(timefp)) );
      }
      fclose(timefp);
      curr_event = first_event;
      while (curr_event != NULL) {           // Look for 1st event with time > Mission Time
	 if (curr_event->time > MissionTime) {
	    fl_select_browser_line(fd_tlegraph->event_list, curr_event->id);
            fl_set_timer(fd_tlegraph->event_timer, (double)(curr_event->time-MissionTime));
            fl_set_object_label(fd_tlegraph->next_event, curr_event->description);
            TLEventimeID = fl_add_timeout((long)(curr_event->time-MissionTime)*1000L,
					  timelapsedCB, NULL);
	    break;
	 }
	 curr_event = (EVENT *)curr_event->nextevent;
      }
      TLEtimeoutID = fl_add_timeout(60000L, TLEIdle, NULL);
   } else {
      fl_show_messages("Error: Unable to open Timeline file");
   }

   fl_clear_choice(fd_tleevent->tle_type);
   fl_addto_choice(fd_tleevent->tle_type, "IGNORE");
   fl_addto_choice(fd_tleevent->tle_type, "UPDATE");
   fl_addto_choice(fd_tleevent->tle_type, "SCHED");
   fl_addto_choice(fd_tleevent->tle_type, "ALARM");
   fl_addto_choice(fd_tleevent->tle_type, "WAKEUP"); 

   fl_get_clock(fd_tlegraph->real_time, &h, &m, &s);
   h = -(h-Ah);  m = -(m-Am);  s = -(s-As);
   int adj = h*3600 + m*60 + s;
   fl_set_clock_adjustment(fd_tlegraph->sim_time, adj);

   return;
}

int TLEclose(FL_FORM *form, void *data)
{
   TLEexitCB(NULL, 0);

   return(0);
}

void TLEshow(int xpos, int ypos, int width, int height, Window mainwinID)
{

   if(!fl_form_is_visible(fd_tlegraph->tlegraph) ) {
      fl_transient();
      fl_winposition(xpos, ypos);
      fl_initial_winsize(width, height);
      TLEwinid = fl_prepare_form_window(fd_tlegraph->tlegraph,
                                     FL_PLACE_POSITION,FL_TRANSIENT, "Timeline");
      fl_winreshape(TLEwinid, xpos, ypos, width, height);
      fl_get_wingeometry(TLEwinid, &TLEwinX, &TLEwinY, &TLEwinW, &TLEwinH); 
      fl_show_form_window(fd_tlegraph->tlegraph);
      fl_set_form_atclose(fd_tlegraph->tlegraph, TLEclose, 0);
      StoreActiveEntry("Timeline");

      if (AutoAccept)
         fl_set_object_lcol(fd_tlegraph->tle_auto, FL_GREEN);
      else
	 fl_set_object_lcol(fd_tlegraph->tle_auto, FL_RED);

      if (StopAccept)
         fl_set_object_lcol(fd_tlegraph->tle_accept, FL_GREEN);
      else
	 fl_set_object_lcol(fd_tlegraph->tle_accept, FL_RED);
   }

   if ((tleBITMAPDIR=getenv("BITMAPDIR")) == NULL) {
        tleBITMAPDIR = "../BitMaps";
   }

   sprintf (tlexpm, "%s/%s.xpm", tleBITMAPDIR, "WxSlime");
   fl_show_object(fd_tlegraph->tle_explain);
   fl_free_pixmap_pixmap(fd_tlegraph->tle_explain);
   fl_set_pixmap_file(fd_tlegraph->tle_explain, tlexpm);

   return;
}

void TLEIdle(int tid, void *stuff)
{
   Xbias = Xbias + 1;
   MissionTime = MissionTime + 60.0;
   tledrawCB();
   TLEtimeoutID = fl_add_timeout(60000L, TLEIdle, NULL);
}

void UpdateQList(int nList)
{
int             i, j;
EVENT           *curr_event, *next_event;

   curr_event  = first_event;

   //j = fl_get_browser(fd_tlegraph->event_list);
   fl_clear_browser(fd_tlegraph->event_list);

   for (i=0; i<nList; i++) {
     curr_event->id = i+1;
     sprintf(tletemp, "%6d  %-8s  %s", (int)curr_event->time, curr_event->type,
	     curr_event->description);
     fl_add_browser_line(fd_tlegraph->event_list, tletemp);
     curr_event = (EVENT *)curr_event->nextevent;
   }

   //fl_select_browser_line(fd_tlegraph->event_list, j);

   return;
}

int TLEAddEvent(float time, char tunit, int priority, const char* type,
                 const char* action, const char* arg, const char* desc, const char* fname)
{
int             putid;
EVENT           *curr_event, *next_event;

   curr_event = (EVENT *)new(EVENT);

   curr_event->id = n_events+1;
   curr_event->time = time;
   curr_event->timeunit = tunit;
   curr_event->priority = priority;
   strcpy(curr_event->type, type);
   strcpy(curr_event->action, action);
   strcpy(curr_event->argument, arg);
   strcpy(curr_event->description, desc);
   strcpy(curr_event->filename, fname);

   if (curr_event->timeunit == 'M') curr_event->time = curr_event->time * 60.0;
   if (curr_event->timeunit == 'H') curr_event->time = curr_event->time * 60.0*60.0;
   if (curr_event->timeunit == 'D') curr_event->time = curr_event->time * 60.0*60.0*24.0;

   putid = 0;
   next_event = first_event;
   while (next_event != NULL) {           // Look for 1st event with time > new event time
     putid++;
     if (time < next_event->time) {
        next_event = (EVENT *)next_event->prevevent;
	break;
     }
     next_event = (EVENT *)next_event->nextevent;
   }

   curr_event->prevevent = (void *)next_event;
   curr_event->nextevent = next_event->nextevent;
   next_event->nextevent = (void *)curr_event;

   n_events++;

   UpdateQList(n_events);

   return (curr_event->id);
}

int TLERemEvent(int eventid)
{
int             irc;
int             putid;
EVENT           *temp_event;          // Points to event to be removed
EVENT           *prev_event;          // Points to previous event
EVENT           *next_event;          // Points to next event

   irc = -1;
   putid = 0;
   temp_event = first_event;

   while (temp_event != NULL) {
     putid++;
     if (temp_event->id == eventid) {
       prev_event = (EVENT *)temp_event->prevevent;
       next_event = (EVENT *)temp_event->nextevent;
       prev_event->nextevent = next_event;
       next_event->prevevent = prev_event;
       delete (temp_event);
       n_events--;
       irc = 0;
       break;
     }
     temp_event = (EVENT *)temp_event->nextevent;
   }

   UpdateQList(n_events);

   return (irc);
}

void TLEAddPms(float time, float pms)
{

   PmsTable[n_Pms][1] = time;
   PmsTable[n_Pms][2] = pms;
   n_Pms++;
}

void timelapsedCB(int tid, void *stuff)
{
int             item = 0;
int             alarm;

//fl_activate_object(fd_tlegraph->accept_button);
//fl_set_object_lcol(fd_tlegraph->accept_button, FL_WHITE);

   if (strcmp(curr_event->type, "IGNORE") == 0) {
     //
     //   IGNORE the event
     //
     timeacceptCB(NULL, 0);
     return;
   }

   if (strcmp(curr_event->type, "UPDATE") == 0) {
     //
     //   UPDATE the appropriate field
     //
     if (strcmp(curr_event->action, "DEFCON") == 0)
       SC_update_def(atoi(curr_event->argument));
     else if (strcmp(curr_event->action, "RP") == 0)
       SC_update_rp(atoi(curr_event->argument));
     else if (strcmp(curr_event->action, "ROE") == 0)
       SC_update_roe(curr_event->argument);
     else if (strcmp(curr_event->action, "DEA") == 0)
       SC_update_dea(curr_event->argument);
     else if (strcmp(curr_event->action, "PLAN") == 0)
       SC_update_plan(curr_event->argument);
     else if (strcmp(curr_event->action, "PPS") == 0)
       TLEAddPms(curr_event->time, atof(curr_event->argument));
     //
     timeacceptCB(NULL, 0);
     return;
   }

   if (strcmp(curr_event->type, "SCHED") == 0) {
     //
     //   SCHEDULE the appropriate application
     //
     strcpy(tletemp, curr_event->filename);
     strsub(tletemp, '_', ' ');
     fl_exe_command(tletemp, 0);
     //
     timeacceptCB(NULL, 0);
     return;
   }

   if (AutoAccept) {
     //
     //   Don't do Alarms
     //
     timeacceptCB(NULL, 0);
     return;
   }
     
   if (curr_event->priority >= AlarmFilter) {        // If high enough priority
     if (strcmp(curr_event->type, "ALARM") == 0) {
       //
       //   Sound operator ALARM
       //
       if (strcmp(curr_event->action, "INTEL") == 0)         alarm = ALARM_INTEL;
       else if (strcmp(curr_event->action, "LAUNCH") == 0)   alarm = ALARM_LAUNCH;
       else if (strcmp(curr_event->action, "TRACK") == 0)    alarm = ALARM_TRACK;
       else if (strcmp(curr_event->action, "POTEVENT") == 0) alarm = ALARM_POTEVENT;
       else if (strcmp(curr_event->action, "JCSWARN") == 0)  alarm = ALARM_JCSWARN;
       else if (strcmp(curr_event->action, "JCSEXEC") == 0)  alarm = ALARM_JCSEXEC;
       else if (strcmp(curr_event->action, "JCSTERM") == 0)  alarm = ALARM_JCSTERM;
       Alarm(alarm, curr_event->description, curr_event->priority, curr_event->filename);
       //
     } else {
       //
       //   Must be a WAKEUP event
       //
       if(fl_form_is_visible(fd_tlegraph->tlegraph) ) {
	 fl_ringbell(50);
       } else {
	 if (curr_event->priority >= AlarmFilter)
	   Alarm(ALARM_TIMER, "Event Timeout", PRIORITY_MED, NULL);
       }
     }
   }

   timeacceptCB(NULL, 0);

   return;
}
/*                                                                       */
/* --------------------------------------------------------------------- */
/*      C A L L B A C K    R O U T I N E S    S T A R T    H E R E       */
/* --------------------------------------------------------------------- */
/*                                                                       */
void TLEexitCB(FL_OBJECT *ob, long data)
{
   fl_hide_form(fd_tlegraph->tlegraph);
   EraseActiveEntry("Timeline");

   FinishUp();

   return;
}

void TLEnoneCB(FL_OBJECT *ob, long data)
{

   return;
}

void tledrawCB()
{
int             i;
int             vpwindW, vpwindH;
int             viewport[4];
int             TLEglInit = FALSE;
float           point[3], scale, xtic, fscale;
char            chlabel[16];
EVENT           *temp_event;


   if(!fl_form_is_visible(fd_tlegraph->tlegraph) ) return;

   glXMakeCurrent(fl_display, fl_get_canvas_id(fd_tlegraph->canvas),
                  fl_get_glcanvas_context(fd_tlegraph->canvas)); 

   fl_get_winsize(fl_get_canvas_id(fd_tlegraph->canvas), &vpwindW, &vpwindH); 
   glViewport(0, 0, vpwindW, vpwindH);

   if (!TLEglInit) {
      glMatrixMode(GL_PROJECTION);
      glLoadIdentity();
      gluOrtho2D(0.0, vpwindW, 0.0, vpwindH);
      glMatrixMode(GL_MODELVIEW);
      glLoadIdentity();
      glfontMake(GL_ALLFONTS);        // Build the fonts
      glfontSet(GL_STROKE);
      TLEglInit = TRUE;
   }

   glClearColor(1.0, 1.0, 1.0, 0.0);
   glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

   Xleft  = Xpan - (Xinterval*4.0);
   Xright = Xpan + (Xinterval*4.0);
   fscale = Xinterval/30.0;
   glMatrixMode(GL_PROJECTION);
   glLoadIdentity();
   gluOrtho2D(Xleft, Xright, -10.0, 100.0);
   glMatrixMode(GL_MODELVIEW);
   glLoadIdentity();
   //
   //   Draw the 'time now' indicator
   //
   glColor3f(0.5, 0.5, 0.5);
   glBegin(GL_TRIANGLES);
     glVertex2f(0.0, 0.0);
     glVertex2f(+3.0*fscale, -8.0);
     glVertex2f(-3.0*fscale, -8.0);
   glEnd();
   glBegin(GL_LINES);
     glVertex2f(0.0, 0.0);
     glVertex2f(0.0, 90.0);
   glEnd();
   //
   //   Draw the x-axis
   //
   glColor3f(0.0, 0.0, 0.0);
   glLineWidth(2.0);
   glBegin(GL_LINES);
     glVertex2f(Xleft, 0.0);
     glVertex2f(Xright, 0.0);
   glEnd();
   glLineWidth(1.0);
   //
   //   Put in the tic marks and labels at 'Xinterval' intervals
   //
   for (i=-20; i<20; i++) {
      glColor3f(0.0, 0.0, 0.0);
      xtic = (float)i * Xinterval;
      glBegin(GL_LINES);
        glVertex2f(xtic, 0.0);
        glVertex2f(xtic, -3.0);
      glEnd();
      glColor3f(0.0, 0.0, 1.0);
      sprintf(chlabel, "%5.1f", (MissionTime/60.0)+xtic);
      glPushMatrix();
        glTranslatef(xtic-5.0*fscale, -7.0, 0.0);
        glScalef(0.2*fscale, 0.25, 0.15);
        glfontPrint(chlabel);
      glPopMatrix();
   }
   sprintf(chlabel, "Time (%s)", fl_get_menu_item_text(fd_tlegraph->unit_menu, 2));
   glPushMatrix();
     glTranslatef(-15.0*fscale, 90.0, 0.0);
     glScalef(0.2*fscale, 0.25, 0.15);
     glfontPrint(chlabel);
   glPopMatrix();
   //
   //   Draw the event labels
   // 
   temp_event = first_event;
   while (temp_event != NULL) {
      switch (temp_event->priority) {
        case PRIORITY_LOW:
          glColor3ub(0, 255, 0);
          break;
        case PRIORITY_MED:
          glColor3ub(205, 149, 10);
          break;
        case PRIORITY_HI:
          glColor3ub(0, 0, 255);
          break;
        case PRIORITY_CRIT:
          glColor3ub(255, 0, 0);
          break;
        default:
          glColor3ub(0, 0, 0);
          break;
      } /* switch */
      glPushMatrix();
        if (Xscaling == 0)
          glTranslatef((temp_event->time/60.0)-Xbias, 10.0, 0.0);
	else
          glTranslatef(logf(temp_event->time/60.0), 10.0, 0.0);
        glScalef(0.2*fscale, 0.25, 0.15);
        glRotatef(90.0, 0.0, 0.0, 1.0);
        glfontPrint(temp_event->description);
      glPopMatrix();
      temp_event = (EVENT *)temp_event->nextevent;
   } /* while */
//
//   Draw the Probibility of Mission Success points
//
   if (n_Pms > 0) {
     glColor3f(0.0, 0.0, 0.0);
     for (i=0; i<n_Pms; i++) { 
      glPushMatrix();
        glTranslatef((PmsTable[i][1]/60.0)-Xbias, PmsTable[i][2], 0.0);
        glScalef(0.2*fscale, 0.25, 0.15);
	sprintf(chlabel, "*%2d%%", (int)PmsTable[i][2]);
        glfontPrint(chlabel);
      glPopMatrix();
     }
   }
//
//   Draw the Replan Trigger indicator
//
   if (Freplan) {
     glColor3f(0.0, 0.0, 0.0);
     glBegin(GL_TRIANGLES);
       glVertex2f(Treplan-Xbias, 0.0);
       glVertex2f((Treplan+(2.0*fscale))-Xbias, 6.0);
       glVertex2f((Treplan-(2.0*fscale))-Xbias, 6.0);
       glEnd();
       glBegin(GL_LINES);
       glVertex2f(Treplan-Xbias, 0.0);
       glVertex2f(Treplan-Xbias, 90.0);
       glEnd();
       glPushMatrix();
       Xreplan = Treplan+(2.0*fscale)-Xbias;
       Yreplan = 85.0;
       glTranslatef(Xreplan, Yreplan, 0.0);
       //printf("Draw trigger at %f %f\n", Xreplan, Yreplan);
       glScalef(0.2*fscale, 0.25, 0.15);
       glfontPrint("Replan");
       glPopMatrix();
   }

   glFlush();
   glXSwapBuffers(fl_display, fl_get_canvas_id(fd_tlegraph->canvas));
}


//<------------------------expose-------------------------------------->

//Run when the opengl screen needs to be refreshed 'cause it is "expose"

int exposeCB(FL_OBJECT *ob, Window win, int w, int h, XEvent *xev, void *ud)
{
   tledrawCB();

   return(0);
}

//<---------------------button press-------------------------------->

//when the user clicks in the opengl canvas window...the program goes here

int buttonpressCB(FL_OBJECT *ob, Window win, int w, int h, XEvent *xev, void *ud)
{
FL_Coord        width, height;
float           xpos, ypos, closest, d;
float           slat, slon, salt, shead;
int             i, xval, yval;

   xval = xev->xbutton.x;
   yval = xev->xbutton.y;

   fl_get_winsize(win, &width, &height);

   xpos = xval; ypos = yval;
   //fprintf(stderr, "User clicked in canvas at %f %f\n", xpos, ypos);
   /*
   if ( ((xpos > Xreplan-10.0) && (xpos < Xreplan+10.0)) &&
	((ypos > Yreplan-10.0) && (ypos > Yreplan+10.0)) )
   */
     tlereasonCB(NULL, 0);

   return(0);
}

void intervalCB(FL_OBJECT *object, long item_no)
{
int             item = 0;

   Xinterval = fl_get_thumbwheel_value(fd_tlegraph->interval);
   sprintf(tletemp, "%d", (int)Xinterval);
   fl_set_object_label(object, tletemp);
   tledrawCB();
}

void timescaleCB(FL_OBJECT *object, long item_no)
{
int             item;

   item = fl_get_menu(fd_tlegraph->scale_menu)-1;

   switch (item) {
     case 0:
       fprintf(stderr, "Setting X scaling to LINEAR\n");
       Xscaling = 0;
       break;

     case 1:
       fprintf(stderr, "Setting X scaling to LOG\n");
       Xscaling = 1;
       break;
   }

   tledrawCB();
}

void timeunitCB(FL_OBJECT *object, long item_no)
{
int             item;

   item = fl_get_menu(fd_tlegraph->unit_menu)-1;
}

void timesourceCB(FL_OBJECT *object, long item_no)
{
int             item;

   item = fl_get_menu(fd_tlegraph->source_menu)-1;

   switch (item) {
     case 0:
       break;

     case 1:
       break;
   }

   tledrawCB();
}

void timescrollCB(FL_OBJECT *object, long item_no)
{
int item = 0;

   Xpan = fl_get_scrollbar_value(fd_tlegraph->time_scroll);
   Xpan = (Xpan - 0.5)*100.0;
   //sprintf(tletemp, "%d", (int)Xpan);
   //fl_set_object_label(object, tletemp);
   tledrawCB();
}

void eventlistCB(FL_OBJECT *object, long item_no)
{
int             i, item = item_no;
double          Tremain;
long            TimeNow, Microsec;
EVENT           *temp_event;

   item = fl_get_browser(fd_tlegraph->event_list);

   fl_select_browser_line(fd_tlegraph->event_list, item); 

   if (fl_mouse_button() == FL_RIGHTMOUSE) {
      temp_event = first_event;
      if (temp_event != NULL) {
	//   Skip 'item' events
	for (i=1; i<item; i++) temp_event = (EVENT *)temp_event->nextevent;

	fl_set_input(fd_tleevent->evt_desc,      temp_event->description);
	sprintf(tletemp, "%f", temp_event->time);
	fl_set_input(fd_tleevent->evt_time,      tletemp);
	//fl_set_input(fd_tleevent->evt_unit,    "Seconds");
	//fl_set_input(fd_tleevent->evt_type,    temp_event->type);
	fl_set_input(fd_tleevent->evt_subtype,   temp_event->action);
	fl_set_input(fd_tleevent->tle_value,     temp_event->argument);
	//fl_set_input(fd_tleevent->evt_period,  "OneTime");
	fl_set_choice(fd_tleevent->tle_priority, temp_event->priority);
	fl_set_input(fd_tleevent->evt_file,      temp_event->filename);
	//
        fl_gettime(&TimeNow, &Microsec);
        Tremain = ((double)TimeBasis + (double)temp_event->time) - (double)TimeNow;
	sprintf(tletemp, "%f", Tremain);
	fl_set_input(fd_tleevent->evt_remain,    tletemp);
	fl_set_input(fd_tleevent->evt_runit,     "Seconds");
	sprintf(tletemp, "%d", temp_event->id);
	fl_set_input(fd_tleevent->evt_id,        tletemp);
      }
      fl_show_form(fd_tleevent->tleevent, FL_PLACE_CENTER,FL_FULLBORDER, "Event Info");
   } 
}

void modlistCB(FL_OBJECT *object, long item_no)
{
int             i, item = item_no;
EVENT           *temp_event;

   switch (item) {
     case 0:
       strcpy(tletemp, fl_get_choice_text(fd_tleevent->evt_tunit));
       TLEAddEvent(atof(fl_get_input(fd_tleevent->evt_time)),
		   tletemp[0],
		   fl_get_choice(fd_tleevent->tle_priority),
		   "COMM",
                   fl_get_input(fd_tleevent->evt_subtype),
		   fl_get_input(fd_tleevent->tle_value),
		   fl_get_input(fd_tleevent->evt_desc),
		   fl_get_input(fd_tleevent->evt_file));
       evtexitCB(NULL, 0);
       break;

     case 1:
       item = fl_get_browser(fd_tlegraph->event_list);
       temp_event = first_event;
       if (temp_event != NULL) {
	 //   Skip 'item' events
	 for (i=1; i<item; i++) temp_event = (EVENT *)temp_event->nextevent;
	 TLERemEvent(temp_event->id);
       }
       evtexitCB(NULL, 0);
       break;
   }
}

void evtexitCB(FL_OBJECT *object, long item_no)
{
int item = 0;

   fl_hide_form(fd_tleevent->tleevent);
}


void timeacceptCB(FL_OBJECT *object, long item_no)
{
int             item = 0;
double          event_time;
long            TimeNow, Microsec;

   fl_set_timer(fd_tlegraph->event_timer, (double)0.0);
   //fl_deactivate_object(fd_tlegraph->accept_button);
   //fl_set_object_lcol(fd_tlegraph->accept_button, FL_INACTIVE_COL);
   fl_deselect_browser_line(fd_tlegraph->event_list, curr_event->id);

   curr_event = (EVENT *)curr_event->nextevent;
   if (curr_event != NULL) {
      fl_gettime(&TimeNow, &Microsec);
      event_time = ((double)TimeBasis + (double)curr_event->time) - (double)TimeNow;
      fl_select_browser_line(fd_tlegraph->event_list, curr_event->id);
      fl_set_timer(fd_tlegraph->event_timer, event_time);
      fl_set_object_label(fd_tlegraph->next_event, curr_event->description);
      TLEventimeID = fl_add_timeout((long)event_time*1000L, timelapsedCB, NULL);
   }

   return;
}

void acceptCB(FL_OBJECT *object, long item_no)
{
int item = 0;

   switch (item) {
     case 0:
       AutoAccept = !AutoAccept;
       if (AutoAccept)
          fl_set_object_lcol(object, FL_GREEN);
       else
	  fl_set_object_lcol(object, FL_RED);
       break;

     case 1:
       StopAccept = !StopAccept;
       if (StopAccept)
          fl_set_object_lcol(object, FL_RED);
       else
	  fl_set_object_lcol(object, FL_GREEN);
       break;

   }
}

void filterCB(FL_OBJECT *object, long item_no)
{
int item = 0;

    AlarmFilter = fl_get_menu(fd_tlegraph->filter_menu)-1;
}

void tlereasonCB(FL_OBJECT *object, long item_no)
{
int             item = 0;
FL_OBJECT       *graph;
Window          winid;
float           xx[20] = { 0.0,  5.0, 16.0, 22.0, 29.0, 35.0, 40.0, 45.0, 50.0 }; 
float           y1[20] = { 0.65, 0.48, 0.33, 0.22, 0.21, 0.20, 0.21, 0.22, 0.33 };
float           x2[2]  = { 0.0, 60.0 };
float           y2[2]  = { 0.20, 0.20 };
float           x3[2]  = { 0.0, 60.0 };
float           y3[2]  = { 0.30, 0.30 };
float           Xr[2]  = { 35.0 };
float           Yr[2]  = {  0.2 };
float           Xb[2]  = { 45.0 };
float           Yb[2]  = {  0.22 };


   sprintf(tletemp, "%s\n%s (%d:%d)\n%s %d:%d\n%s %d min %d sec\n%s",
	   " - Ps = Probability of Mission Success",
	   " - Repairs are more likely to fail near time of plan breakdown", 45, 15,
	   " - Best time to repair is", 34, 55,
	   " - Estimated planning time is", 1, 30,
	   " - Y-axis Planning Effort is Planning Time/Mission Time");
   fl_set_object_label(fd_tlereplan->explain_text, tletemp);

   sprintf(tletemp, "%s %5.1f min\n%s %5.1f min\n%s %5.1f\n%s %5.1f",
           "  Cost to Repair ...........", 1.5,
	   "  Cost for New Plan ........", 6.0,
	   "  Plan Breakdown (Ps).... ..", 0.8,
	   "  Repair Parameter (b).... .", -8.0);
   fl_set_object_label(fd_tlereplan->parms_text, tletemp);

   graph = fd_tlereplan->replan_graph;
   fl_set_xyplot_overlay_type(graph, 1, FL_DOTTED_XYPLOT);
   fl_set_xyplot_overlay_type(graph, 2, FL_DASHED_XYPLOT);
   fl_set_xyplot_overlay_type(graph, 3, FL_DASHED_XYPLOT);
   fl_set_xyplot_overlay_type(graph, 4, FL_SQUARE_XYPLOT);
   fl_set_xyplot_overlay_type(graph, 5, FL_CIRCLE_XYPLOT);
   fl_set_xyplot_xbounds(graph, 0, 60.0);
   fl_set_xyplot_ybounds(graph, 0, 1.1);
   fl_set_xyplot_ytics(graph, 6, 0);
   fl_set_xyplot_interpolate(graph, 0, 3, 0.1);
   fl_set_xyplot_data(graph, xx, y1, 9, " ",
		      "Mission Time (Mins)",
		      "Planning Effort");
   fl_add_xyplot_overlay(graph, 1, x2, y2, 2, FL_BLUE);
   fl_add_xyplot_overlay(graph, 2, x3, y3, 2, FL_RED); 
   fl_add_xyplot_text(graph, 2.0, 0.22, "Optimum Repair Time", FL_ALIGN_RIGHT, FL_BLUE);
   fl_add_xyplot_text(graph, 2.0, 0.32, "Never Repair", FL_ALIGN_RIGHT, FL_RED);

   fl_add_xyplot_overlay(graph, 4, Xr, Yr, 1, FL_BLUE);
   fl_add_xyplot_overlay(graph, 5, Xb, Yb, 1, FL_RED);

   fl_winposition(TLEwinX, TLEwinY);
   fl_initial_winsize(TLEwinW, TLEwinH);
   winid = fl_prepare_form_window(fd_tlereplan->tlereplan,
                                  FL_PLACE_POSITION,FL_NOBORDER, "Replan View");
   fl_winreparent(winid, TLEwinid);
   fl_show_form_window(fd_tlereplan->tlereplan);  
}

void tle_planexitCB(FL_OBJECT *object, long item_no)
{
int item = 0;

   fl_hide_form(fd_tlereplan->tlereplan);    
}
