#include <errno.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

#include "forms.h"
#include "xfl.h"
#include "argv.h"

#include "XPSforms.h"

/*----------------------------------------------------------------------*/

int             XPSwinX, XPSwinY;
int             XPSwinW, XPSwinH;
char            XPSlabel[32];
Window          XPSwinid;

/*----------------------------------------------------------------------*/

int cmp_uid(const void *e1, const void *e2);
int cmp_pid(const void *e1, const void *e2);
int cmp_ppid(const void *e1, const void *e2);
int cmp_c(const void *e1, const void *e2);
int cmp_stime(const void *e1, const void *e2);
int cmp_tty(const void *e1, const void *e2);
int cmp_etime(const void *e1, const void *e2);
int cmp_cmd(const void *e1, const void *e2);
int fmt_uid(char *p, int width, void *v);
int fmt_pid(char *p, int width, void *v);
int fmt_ppid(char *p, int width, void *v);
int fmt_c(char *p, int width, void *v);
int fmt_stime(char *p, int width, void *v);
int fmt_tty(char *p, int width, void *v);
int fmt_etime(char *p, int width, void *v);
int fmt_cmd(char *p, int width, void *v);

/*----------------------------------------------------------------------*/
/* These are the columns to display in the browser.
*/

#define F(fld)	cmp_ ## fld, fmt_ ## fld

static XFL_COLUMN Cols[] =
 {
	{ "UID", 	F(uid),		9, 0 },
	{ "PID",	F(pid),		6, 0 },
	{ "PPID",	F(ppid),	6, 0 },
	{ "C",		F(c),		4, 0 },
	{ "STIME",	F(stime),	9, 0 },
	{ "TTY",	F(tty),		7, 0 },
	{ "TIME",	F(etime),	6, 0 },
	{ "CMD",	F(cmd),		20, 0 }
};

#define NCOLS	(sizeof(Cols) / sizeof(Cols[0]))

static XFL_COLUMN_CONTROL Col_ctrl;

/*----------------------------------------------------------------------*/
/* Indicies into Cols[].
*/

typedef enum
 {
	COLIDX_UID,
	COLIDX_PID,
	COLIDX_PPID,
	COLIDX_C,
	COLIDX_STIME,
	COLIDX_TTY,
	COLIDX_TIME,
	COLIDX_CMD
} COLIDX;

/*----------------------------------------------------------------------*/

typedef struct
 {	
	char uid [8+1];
	pid_t pid;
	pid_t ppid;
	int c;
	char stime [sizeof("hh:mm:ss")];
	char tty [7+1];
	char etime [sizeof("mmm:ss")];
	char cmd [32+1];

	time_t stime_time;
	time_t etime_time;
} PROC;

/*----------------------------------------------------------------------*/

static XFL_SORT_ORDER Sort_order;
static FD_xps   *fd_xps;
static char     Cmd [BUFSIZ];
static double   Refresh_Rate = 15.0;	/* Fifteen seconds */

/*----------------------------------------------------------------------*/

typedef enum
 {
	FILE_CLOSE,
	EDIT_FIND,
	VIEW_REFRESH,
	TOOLS_CUSTOMIZE, TOOLS_OPTIONS,
	HELP_ABOUT,
} MENU_ITEMS;

/*----------------------------------------------------------------------*/

static void create_xps_form(void);
void xps_browser_cb(FL_OBJECT *obj, long arg);
void xps_timer_cb(FL_OBJECT *obj, long arg);
static void menu_cb(FL_OBJECT *obj, long arg);
static void customize(void);
static void options(void);
static void find(void);
static void update(FL_OBJECT *browser);
static void parse_proc(PROC *proc, char *buf);
static time_t parse_stime(char *s);
static time_t parse_etime(char *s);
//extern "C" int build_list(LIST **p_list, const void *data, size_t size);
static void sort_proclist(LIST *list, XFL_COLUMN *key, XFL_SORT_ORDER order);
static void format_proc_line(char *p, PROC *proc);
static void sigint(int sig, void *data);

extern int  EraseActiveEntry(char *text);
extern int  StoreActiveEntry(char *text);
extern int  FinishUp();

/*----------------------------------------------------------------------*/

void XPSinit()
{
char            *p;
int             i;

   p = Cmd;
   p += strlen(strcpy(p, "ps -f -e"));
   p += strlen(strcpy(p, " | awk 'NR > 1'"));

   fl_add_signal_callback(SIGINT, sigint, NULL);
   fl_set_border_width(1);
   create_xps_form();

   return;
}

int XPSclose(FL_FORM *form, void *data)
{
   XPSexitCB(NULL, 0);
   return(0);
}

void XPSshow(int xpos, int ypos, int width, int height, Window mainwinID)
{

   if(!fl_form_is_visible(fd_xps->xps) ) {
      fl_transient();
      fl_winposition(xpos, ypos);
      fl_initial_winsize(width, height);
      XPSwinid = fl_prepare_form_window(fd_xps->xps,
                                     FL_PLACE_POSITION,FL_TRANSIENT, "System-Load");
      fl_winreshape(XPSwinid, xpos, ypos, width, height);
      fl_get_wingeometry(XPSwinid, &XPSwinX, &XPSwinY, &XPSwinW, &XPSwinH);
      XPSwinX = XPSwinX + 4;
      XPSwinY = XPSwinY + 24;
      fl_show_form_window(fd_xps->xps);
      fl_set_form_atclose(fd_xps->xps, XPSclose, 0);
      fl_set_form_title(fd_xps->xps, "System-Load");
      StoreActiveEntry("System-Load");
   }


   fl_trigger_object(fd_xps->xps_timer);

   return;
}

void XPSexitCB(FL_OBJECT *ob, long data)
{
   fl_hide_form(fd_xps->xps);
   EraseActiveEntry("System-Load");

   FinishUp();

   return;
}

void XPSnoneCB(FL_OBJECT *ob, long data)
{
   return;
}

/*----------------------------------------------------------------------*/

static void create_xps_form(void)
{
	XFL_MENU *Main_menu;
	XFL_MENU *File_menu;
	XFL_MENU *Edit_menu;
	XFL_MENU *View_menu;
	XFL_MENU *Tools_menu;
	XFL_MENU *Help_menu;

	fd_xps = create_form_xps();

	Col_ctrl.cols		= Cols;
	Col_ctrl.n		= NCOLS;
	Col_ctrl.sort_key	= NULL;
	Col_ctrl.sort_order	= SORT_ASCENDING;
	Col_ctrl.btype		= FL_NORMAL_BUTTON;
	Col_ctrl.bstyle		= FL_NORMAL_STYLE;
	Col_ctrl.bsize		= FL_NORMAL_SIZE;
	Col_ctrl.bheight	= 20;
	Col_ctrl.tstyle		= FL_FIXED_STYLE;
	Col_ctrl.tsize		= FL_MEDIUM_SIZE;
	Col_ctrl.neighbor	= fd_xps->xps_browser;
	Col_ctrl.trigger	= fd_xps->xps_timer;

	Cols[COLIDX_UID].order		= 1;
	Cols[COLIDX_PID].order		= 2;
	Cols[COLIDX_PPID].order		= 3;
	Cols[COLIDX_C].order		= 4;
	Cols[COLIDX_STIME].order	= 5;
	Cols[COLIDX_TTY].order		= 6;
	Cols[COLIDX_TIME].order		= 7;
	Cols[COLIDX_CMD].order		= 8;

	xfl_add_columns(&Col_ctrl);
}

/*----------------------------------------------------------------------*/

void xps_browser_cb(FL_OBJECT *obj, long arg)
{
}

/*----------------------------------------------------------------------*/

void xps_timer_cb(FL_OBJECT *obj, long arg)
{
	fl_set_timer(obj, 0.0L);

	update(fd_xps->xps_browser);

	fl_set_timer(obj, Refresh_Rate);
}

/*----------------------------------------------------------------------*/

static void menu_cb(FL_OBJECT *obj, long arg)
{
	switch (arg)
	{
	case FILE_CLOSE:
		fl_finish();
		exit(0);

	case EDIT_FIND:
		find();
		break;

	case VIEW_REFRESH:
		fl_trigger_object(fd_xps->xps_timer);
		break;

	case TOOLS_CUSTOMIZE:
		customize();
		break;

	case TOOLS_OPTIONS:
		options();
		break;

	case HELP_ABOUT:
		fl_show_messages("xps\nVersion 1.0\n(c) Copyright 1997 Russel Lane & Associates, Inc.");
		break;
	}
}

/*----------------------------------------------------------------------*/

static void customize(void)
{
	xfl_config_columns(&Col_ctrl);
}

/*----------------------------------------------------------------------*/

static void options(void)
{
	char buf [32];
	const char *p;

	sprintf(buf, "%.0f", Refresh_Rate);
	p = fl_show_input("Enter the refresh rate (in seconds):", buf);

	if (p != NULL)
	{
		char *e;
		double d = strtod(p, &e);

		if (e != p && d > 0.0)
			Refresh_Rate = d;
	}
}

/*----------------------------------------------------------------------*/

static void find(void)
{
}

/*----------------------------------------------------------------------*/

static void update(FL_OBJECT *browser)
{
	LIST *proclist = NULL;
	FILE *pp;
	char buf [BUFSIZ];
	int topline;
	int i;

	/*fprintf(stderr, "Running [%s]\n", Cmd);*/

	if ((pp = popen(Cmd, "r")) == NULL)
	{
		perror("popen");
		exit(1);
	}

	while (fgets(buf, sizeof(buf), pp) != NULL)
	{
		PROC proc;

		/*fprintf(stderr, "Read %s", buf);*/

		parse_proc(&proc, buf);

		if (build_list(&proclist, &proc, sizeof(proc)) == ERROR)
		{
			fprintf(stderr, "Can't build list\n");
			exit(1);
		}
	}

	/*fprintf(stderr, "Closing pipe\n");*/

	pclose(pp);

	sort_proclist(proclist, Col_ctrl.sort_key, Col_ctrl.sort_order);

	fl_freeze_form(browser->form);
	topline = fl_get_browser_topline(browser);
	fl_clear_browser(browser);

	for (i = 0 ; i < proclist->argc ; i++)
	{
		format_proc_line(buf, proclist->argv[i]);
		fl_add_browser_line(browser, buf);
	}

	fl_set_browser_topline(browser, topline);
	fl_unfreeze_form(browser->form);

	destroy_list(&proclist);
}

/*----------------------------------------------------------------------*/

static void parse_proc(PROC *proc, char *buf)
{
	sscanf(buf, "%8s %d %d %d%*c%8c %7s %6s %32[^\n]",
		proc->uid, &proc->pid, &proc->ppid, &proc->c,
		proc->stime, proc->tty, proc->etime, proc->cmd);

	proc->stime[8] = '\0';

	/* Make sorting life easier */
	proc->stime_time = parse_stime(proc->stime);
	proc->etime_time = parse_etime(proc->etime);
}

/*----------------------------------------------------------------------*/

static time_t parse_stime(char *s)
{
	time_t t = time(NULL);
	struct tm *tp = localtime(&t);

	if (sscanf(s, "%d:%d:%d", &tp->tm_hour, &tp->tm_min, &tp->tm_sec) != 3)
	{
		static char *months[] =
		{
			"Jan", "Feb", "Mar", "Apr", "May", "Jun",
			"Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
		};

		char mon[3+1];
		int i;

		if (sscanf(s, "%3s %d", mon, &tp->tm_mday) != 2)
		{
			return(0);
		}

		for (i = 0 ; i < 12 ; i++)
			if (strcmp(mon, months[i]) == 0)
				break;
		if (i >= 12)
			return(0);

		if (i > tp->tm_mon)
			tp->tm_year--;

		tp->tm_mon = i;
		tp->tm_hour = tp->tm_min = tp->tm_sec = 0;
	}

	t = mktime(tp);
	return(t);
}

/*----------------------------------------------------------------------*/

static time_t parse_etime(char *s)
{
	int mm, ss;

	if (sscanf(s, "%d:%d", &mm, &ss) == 2)
	{
		time_t t = (mm * 60) + ss;
		return(t);
	}

	return(0);
}

/*----------------------------------------------------------------------*/

static void sort_proclist(LIST *list, XFL_COLUMN *key, XFL_SORT_ORDER order)
{
	if (list && list->argc > 1 && key)
	{
		Sort_order = order;
		qsort(list->argv, list->argc, sizeof(list->argv[0]), key->cmp);
	}
}

/*----------------------------------------------------------------------*/

static void format_proc_line(char *p, PROC *proc)
{
	int i;

	*p++ = '@'; *p++ = 'f'; *p++ = '@'; *p++ = 'm';

	for (i = 0 ; i < Col_ctrl.n_in_view ; i++)
	{
		XFL_COLUMN *c = Col_ctrl.ordered_cols[i];

		if (i > 0)
		{
			*p++ = ' ';
		}

		p += (*c->fmt)(p, c->nchars, proc);
	}
}

/*----------------------------------------------------------------------*/

static void sigint(int sig, void *data)
{
	fprintf(stderr, "sigint\n");
	fl_finish();
	exit(0);
}

/*----------------------------------------------------------------------*/
/* Compare routines							*/
/*----------------------------------------------------------------------*/

int cmp_uid(const void *e1, const void *e2)
{
	const PROC *s1 = *((PROC *const *)e1);
	const PROC *s2 = *((PROC *const *)e2);
	int x = strcmp(s1->uid, s2->uid);
	return(Sort_order == SORT_ASCENDING ? x : -x);
}

/*----------------------------------------------------------------------*/

int cmp_pid(const void *e1, const void *e2)
{
	const PROC *s1 = *((PROC *const *)e1);
	const PROC *s2 = *((PROC *const *)e2);
	int x = s1->pid - s2->pid;
	return(Sort_order == SORT_ASCENDING ? x : -x);
}

/*----------------------------------------------------------------------*/

int cmp_ppid(const void *e1, const void *e2)
{
	const PROC *s1 = *((PROC *const *)e1);
	const PROC *s2 = *((PROC *const *)e2);
	int x = s1->ppid - s2->ppid;
	return(Sort_order == SORT_ASCENDING ? x : -x);
}

/*----------------------------------------------------------------------*/

int cmp_c(const void *e1, const void *e2)
{
	const PROC *s1 = *((PROC *const *)e1);
	const PROC *s2 = *((PROC *const *)e2);
	int x = s1->c - s2->c;
	return(Sort_order == SORT_ASCENDING ? x : -x);
}

/*----------------------------------------------------------------------*/

int cmp_stime(const void *e1, const void *e2)
{
	const PROC *s1 = *((PROC *const *)e1);
	const PROC *s2 = *((PROC *const *)e2);
	int x = s1->stime_time - s2->stime_time;
	return(Sort_order == SORT_ASCENDING ? x : -x);
}

/*----------------------------------------------------------------------*/

int cmp_tty(const void *e1, const void *e2)
{
	const PROC *s1 = *((PROC *const *)e1);
	const PROC *s2 = *((PROC *const *)e2);
	int x = strcmp(s1->tty, s2->tty);
	return(Sort_order == SORT_ASCENDING ? x : -x);
}

/*----------------------------------------------------------------------*/

int cmp_etime(const void *e1, const void *e2)
{
	const PROC *s1 = *((PROC *const *)e1);
	const PROC *s2 = *((PROC *const *)e2);
	int x = s1->etime_time - s2->etime_time;
	return(Sort_order == SORT_ASCENDING ? x : -x);
}

/*----------------------------------------------------------------------*/

int cmp_cmd(const void *e1, const void *e2)
{
	const PROC *s1 = *((PROC *const *)e1);
	const PROC *s2 = *((PROC *const *)e2);
	int x = strcmp(s1->cmd, s2->cmd);
	return(Sort_order == SORT_ASCENDING ? x : -x);
}

/*----------------------------------------------------------------------*/
/* Formatting routines							*/
/*----------------------------------------------------------------------*/

/*----------------------------------------------------------------------*/
/* Equiv. to: return(sprintf(p, "%*s", width, s));
*/

#define	PADSTRING(p, width, s)	do {\
	int len;\
	for (len = strlen(s) ; len < width ; len++)\
		*p++ = ' ';\
	while ((*p++ = *s++) != '\0')\
		continue;\
	*p = '\0';\
	return(len);\
} while (0)

/*----------------------------------------------------------------------*/
/* Equiv. to: return(sprintf(p, "%*s", width, s));
*/

#define	LEFTJUST(p, width, s)	do {\
	int len = 0;\
	while ((*p++ = *s++) != '\0')\
		len++;\
	for ( ; len < width ; len++)\
		*p++ = ' ';\
	*p = '\0';\
	return(len);\
} while (0)

/*----------------------------------------------------------------------*/
/* Equiv. to: return(sprintf(p, "%*d", width, n));
*/

#define	PADNUMBER(p, width, n)	do {\
	if (width == 0)\
	{\
		char buf [32];\
		char *s = buf + 31;\
		*s-- = '\0';\
		do {\
			*s-- = (n % 10) + '0';\
			n /= 10;\
		} while (n != 0);\
		strcpy(p, s+1);\
		return(strlen(p));\
	}\
	else\
	{\
		char *s = p + width;\
		*s-- = '\0';\
		while (s >= p)\
		{\
			*s-- = (n % 10) + '0';\
			n /= 10;\
			if (n == 0)\
				break;\
		}\
		while (s >= p)\
			*s-- = ' ';\
		return(width);\
	}\
} while (0)

/*----------------------------------------------------------------------*/

int fmt_uid(char *p, int width, void *v)
{
	char *s = ((PROC*)v)->uid;
	PADSTRING(p, width, s);
}

/*----------------------------------------------------------------------*/

int fmt_pid(char *p, int width, void *v)
{
	int n = ((PROC*)v)->pid;
	PADNUMBER(p, width, n);
}

/*----------------------------------------------------------------------*/

int fmt_ppid(char *p, int width, void *v)
{
	int n = ((PROC*)v)->ppid;
	PADNUMBER(p, width, n);
}

/*----------------------------------------------------------------------*/

int fmt_c(char *p, int width, void *v)
{
	int n = ((PROC*)v)->c;
	PADNUMBER(p, width, n);
}

/*----------------------------------------------------------------------*/

int fmt_stime(char *p, int width, void *v)
{
	char *s = ((PROC*)v)->stime;
	PADSTRING(p, width, s);
}

/*----------------------------------------------------------------------*/

int fmt_tty(char *p, int width, void *v)
{
	char *s = ((PROC*)v)->tty;
	PADSTRING(p, width, s);
}

/*----------------------------------------------------------------------*/

int fmt_etime(char *p, int width, void *v)
{
	char *s = ((PROC*)v)->etime;
	PADSTRING(p, width, s);
}

/*----------------------------------------------------------------------*/

int fmt_cmd(char *p, int width, void *v)
{
	char *s = ((PROC*)v)->cmd;
	LEFTJUST(p, width, s);
}
