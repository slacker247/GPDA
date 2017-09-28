/***************************************************************************
   01/11/93: Added and modified things from jbl's new stuff; Tung
   01/21/93: added timedraw routine and related stuff; Tung
   01/22/93: jbl's p_processEvent did this:
                in default case when there is no special Request,
                1. mouse down: pick (& thus draw), then either
                2. mouse move: pick (& draw) again, or
                3. mouse up:   pick (& draw) again;
             that is two window draws while only one (want to pick) or
             none (want to drag) pick is desired. I removed the 2nd and 
             the 3rd.  Also, when in shiftmode, p_processEvent is not even
             called. Tung
 
**************************************************************************** */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h> 
#include <X11/keysym.h>
#include <X11/StringDefs.h>
#include <Xm/Frame.h>
#include <Xm/RowColumn.h>
#include <GL/GLwDrawA.h>
#include "GR_Interface.H"
#include "GR_Window.H"
#include "GR_DispObj.H"

//#define ONYX
#define GLX_OVERLAY     1
#define GLX_BUFSIZE     4096
#define GLX_ZSIZE       1
#define GLX_NOCONFIG    1
#define GLX_NORMAL      1
#define GLX_DOUBLE      2
#define GLX_RGB         3
#define OVERDRAW	0		/* For mswapbuffers */

static GR_matrix Identity =
{
   1.0, 0.0, 0.0, 0.0,
   0.0, 1.0, 0.0, 0.0,
   0.0, 0.0, 1.0, 0.0,
   0.0, 0.0, 0.0, 1.0
};

extern FILE         *INFOfp;
//static GLXContext   p_context;
static Widget       glx_area, glx_frame;
static Display*     glx_display;
int		    GR_MouseX = 0;
int	 	    GR_MouseY = 0;
extern Boolean      RECORDING;
extern void recordCB (Widget, XtPointer client_data, XtPointer);


int GLXwinset(Display* dpy, Window win, GLXContext context)
{
   return (int)glXMakeCurrent(dpy, win, context);
}

void 
installColormap(Widget toplevel, Widget glw)
{
   Window windows[2];
   
   windows[0] = XtWindow(glw);
   windows[1] = XtWindow(toplevel);
   XSetWMColormapWindows(XtDisplay(toplevel), XtWindow(toplevel), windows, 2);
}
/*
void
installColormapWithOverlay(Widget toplevel, Widget glw)
{
   Window windows[5];
   Window overlay, popup, underlay;
   Arg args[5];
   register int i=0;
   
   i=0;
   XtSetArg(args[i], GlxNoverlayWindow, &overlay); i++;
   XtSetArg(args[i], GlxNpopupWindow, &popup); i++;
   XtSetArg(args[i], GlxNunderlayWindow, &underlay); i++;
   XtGetValues(glw, args, i);
   i = 0;
   if (overlay)
   {
      windows[i] = overlay;
      i++;
   }
   if (popup)
   {
      windows[i] = popup;
      i++;
   }
   if (underlay)
   {
      windows[i] = underlay;
      i++;
   }
   windows[i] = XtWindow(glw); i++;
   windows[i] = XtWindow(toplevel); i++;
   XSetWMColormapWindows(XtDisplay(toplevel), XtWindow(toplevel), windows, i);
}
*/
unsigned long
WidgetBackgroundToGlRgb(Widget widget)
{
   Arg args[10];
   int n;
   Pixel xbg;          /* x background pixel */
   Colormap xcolormap;
   XColor xcolor;
   unsigned long glbg; /* gl bacground color */
   
   n = 0;
   XtSetArg(args[n], XtNbackground, &xbg); n++;
   XtGetValues(widget, args, n);
   
   n = 0;
   XtSetArg(args[n], XtNcolormap, &xcolormap); n++;
   XtGetValues(XtParent(widget), args, n);
   
   xcolor.flags = DoRed | DoGreen | DoBlue;
   xcolor.pixel = xbg;
   XQueryColor (XtDisplay(widget), xcolormap, &xcolor);
   
   glbg = (xcolor.red >> 8) + ((xcolor.green >> 8) << 8) +
     ((xcolor.blue >> 8) << 16);
   return (glbg);
}


GR_Window::GR_Window ()
{
   p_drawfunction = 0;
   p_displist_list = 0;
   p_background_color = GR_get_color (0, 0, 0);
   
   p_nextRequest = -1;
   
   p_event.x = 0;
   p_event.y = 0;
   p_event.lastx = 0;
   p_event.lasty = 0;
   p_event.downx = 0;
   p_event.downy = 0;
   p_event.button = 0;
   p_event.moving = 0;
   p_event.down = 0;
   p_event.buttonstate.left = 0;
   p_event.buttonstate.middle = 0;
   p_event.buttonstate.right = 0;
   //p_lightsource = 0;
   
   p_glconfig [0].buffer = GLX_NORMAL;
   p_glconfig [0].mode = GLX_DOUBLE;
   p_glconfig [0].arg = TRUE;

   p_glconfig [1].buffer = GLX_NORMAL;
   p_glconfig [1].mode = GLX_RGB;
   p_glconfig [1].arg = TRUE;

   p_glconfig [2].buffer = GLX_OVERLAY;
   p_glconfig [2].mode = GLX_BUFSIZE;
   p_glconfig [2].arg = 4;

   p_glconfig [3].buffer = GLX_OVERLAY;
   p_glconfig [3].mode = GLX_DOUBLE;
   p_glconfig [3].arg = TRUE;

   p_glconfig [4].buffer = GLX_NORMAL;
   p_glconfig [4].mode = GLX_ZSIZE;
   p_glconfig [4].arg = GLX_NOCONFIG;

   p_glconfig [5].buffer = 0;
   p_glconfig [5].mode = 0;
   p_glconfig [5].arg = 0;

   p_lastDrawTime = 0;
   p_requestDraw = 0;
   p_timedraw_period = 500;
   timeout_id = 0;

   p_twist = 0;
   set_viewmode (GR_ORTHO2);
   p_freeview = FALSE;

   p_awake = TRUE;
}

GR_Window::~GR_Window()
{
}

void
GR_Window::settop(Widget win, XtAppContext context, Widget frame)
{
	GR_toplevel = win;
	GR_appcontext = context;
	glx_frame = frame;
}

long
GR_Window::lastDrawTime ()
{
        if (p_requestDraw)
                return p_lastDrawTime;
        else
                return time (0);
}


void
GR_Window::addDispList (GR_DispList* displist, char *name)
{
   GR_DispListElem		*ptr;

   ptr = p_displist_list;
   
   // if a name is not given, then that displist is placed always
   // as the first element in the list. This is so that if only 
   // one display list is used, then a name doens't have to be 
   // given and for multiple display lists, a search does not have
   // to be done for the display list without a name. All other
   // display lists are added to the bottom of the list to preserve
   // the order in which they were installed. Duplication of a name
   // in the list will cause the stored displist pointer to be 
   // overwritten.
   
   if (ptr == 0) // if the list doesn't exist
   {
      // create a new list element
      ptr = new GR_DispListElem;
      ptr->displist = displist;
      if (name)
      {
	 // if a name was given, then copy it to the new element
	 ptr->name = new char [strlen (name) + 1];
	 strcpy (ptr->name, name);
      }
      else
	// otherwise, set it to zero
	ptr->name = 0;
      ptr->next = 0;
      p_displist_list = ptr;
      return;
   }
   else if (!name)
   {
      if (ptr->name)
      {
	 ptr = new GR_DispListElem;
	 ptr->displist = displist;
	 ptr->name = 0;
	 ptr->next = p_displist_list;
	 p_displist_list = ptr;
      }
      else
	ptr->displist = displist;
      
      return;
   }
   
   while (ptr)
   {
      if (ptr->name)   // added to prevent disaster...
      {
	 if (strcmp (ptr->name, name) == 0)
	 {
	    ptr->displist = displist;
	    return;
	 }
      }
      
      if (ptr->next)
	ptr = ptr->next;
      else
	break;
   }
   
   ptr->next = new GR_DispListElem;
   ptr = ptr->next;
   ptr->displist = displist;
   ptr->name = new char [strlen (name) + 1];
   strcpy (ptr->name, name);
   ptr->next = 0;
}

Widget
GR_Window::v_createWidget (char *name, Widget parent)
{
   Widget	frame;
   Display*	dpy;
   int		attribs[] = {GLX_RGBA, GLX_DOUBLEBUFFER,
                             GLX_RED_SIZE,1, GLX_GREEN_SIZE,1, GLX_BLUE_SIZE,1, 
                             GLX_DEPTH_SIZE, 12, None};
   XVisualInfo* visual;
   GLXContext   glxcontext;
   Bool         direct;
   XFontStruct  *font;
   Font		fid;
   const char   *client;
   int		major, minor;

//   printf ("... enter v_createWidget (%s, %s)...\n", name, XtName(parent));

   dpy = XtDisplay(parent);
   p_display = dpy;
   if (!(visual = glXChooseVisual(dpy, DefaultScreen(dpy), attribs)))
       XtAppError(GR_appcontext, " No double buffered RGBA visual");
   p_widget = XtVaCreateManagedWidget(name,glwDrawingAreaWidgetClass,parent,
                XtNwidth,            620,
                XtNheight,           400,
		XmNbottomAttachment, XmATTACH_FORM,
                XmNtopAttachment,    XmATTACH_FORM,
                XmNleftAttachment,   XmATTACH_FORM,
                XmNrightAttachment,  XmATTACH_FORM,
                GLwNvisualInfo,      visual,
                NULL);
   if (INFOfp != NULL)
       fprintf(INFOfp,"A DrawWidget in frame %s has just been created\n", name);

   XtAddCallback (p_widget, GLwNexposeCallback, (XtCallbackProc)p_exposeCB, (XtPointer) this);
   XtAddCallback (p_widget, GLwNginitCallback,  (XtCallbackProc)p_initCB,   (XtPointer) this);
   XtAddCallback (p_widget, GLwNresizeCallback, (XtCallbackProc)p_resizeCB, (XtPointer) this);
   XtAddCallback (p_widget, GLwNinputCallback,  (XtCallbackProc)p_inputCB,  (XtPointer) this);

   XtRealizeWidget(GR_toplevel);
   p_context = glXCreateContext(dpy, visual, 0, GL_TRUE);
   direct = glXIsDirect(dpy, p_context);
   GLwDrawingAreaMakeCurrent(p_widget, p_context);
   glEnable(GL_DEPTH_TEST);
   glEnable(GL_CULL_FACE);
   glEnable(GL_NORMALIZE);
   glShadeModel(GL_SMOOTH);
   glClearColor(0.0, 0.0, 0.0, 0.0);
   glDepthRange(0,1);
   glClearDepth(1.0);
   glMatrixMode(GL_PROJECTION);
   glOrtho(0.0, 620.0, 0.0, 400.0, -1.0, 1.0);
   glMatrixMode(GL_MODELVIEW);
   glLoadIdentity();

   font = XLoadQueryFont(dpy, 
          "-adobe-courier-bold-o-normal--14-140-75-75-m-90-iso8859-1");
   fid = font->fid;
   glXUseXFont(fid, 32, 96, 2000+32);

   if (INFOfp != NULL) {
       client = (char *)(glGetString(GL_VENDOR));
       fprintf(INFOfp,"OpenGL Vendor is %s\n", (char *)client);
       client = (char *)(glGetString(GL_VERSION));
       fprintf(INFOfp,"OpenGL Version is %s\n", (char *)client);
       client = (char *) (glGetString(GL_RENDERER));
       fprintf(INFOfp,"Renderer name is %s\n", (char *)client);
       client = (char *)(glGetString(GL_EXTENSIONS));
       fprintf(INFOfp,"GL Extensions are %s\n", (char *)client);

       if (glXQueryVersion(dpy, &major, &minor))
           fprintf(INFOfp,"Client GLX Version is %d.%d\n", major,minor);
       client = glXGetClientString(dpy, GLX_VENDOR);
       fprintf(INFOfp,"Client Vendor is %s\n", (char *)client);
       client = glXGetClientString(dpy, GLX_VERSION);
       fprintf(INFOfp,"Client Version is %s\n", (char *)client);
       client = glXGetClientString(dpy, GLX_EXTENSIONS);
       fprintf(INFOfp,"Client GLX Extensions are %s\n", (char *)client);
     }

   return p_widget;
}

void
GR_Window::set_viewmode (long viewmode)
{
   p_viewmode = viewmode;
   switch (p_viewmode)
   {
    case GR_ORTHO:
    case GR_ORTHO2:
      p_set_default_ortho ();
      break;
    case GR_PERSPECTIVE:
      p_set_default_perspective ();
      break;
    default:
      p_viewmode = GR_ORTHO2;
      p_set_default_ortho ();
      break;
   }
}

void
GR_Window::p_set_default_ortho ()
{
   p_viewparams.orthoView.left = 0.0;
   p_viewparams.orthoView.right = 1.0;
   p_viewparams.orthoView.bottom = 0.0;
   p_viewparams.orthoView.top = 1.0;
   p_viewparams.orthoView.near = -1.0;
   p_viewparams.orthoView.far = 1.0;
}

void
GR_Window::p_set_default_perspective ()
{
   p_viewparams.perspectiveView.fov = 600;
   p_viewparams.perspectiveView.aspect = 1.0;
   p_viewparams.perspectiveView.near = 0.1;
   p_viewparams.perspectiveView.far = 100.0;
}

void
GR_Window::field_of_view (float value)
{
   if (p_viewmode != GR_PERSPECTIVE)
     return;

   p_viewparams.perspectiveView.fov = (long) (value);
}

void
GR_Window::aspect (float value)
{
   if (p_viewmode != GR_PERSPECTIVE)
     return;
   
   p_viewparams.perspectiveView.aspect = value;
}

void
GR_Window::near (float value)
{
   switch (p_viewmode)
   {
    case GR_ORTHO:
	case GR_ORTHO2:
      p_viewparams.orthoView.near = value;
      break;
      
    case GR_PERSPECTIVE:
      p_viewparams.perspectiveView.near = value;
      break;
   }
}

void
GR_Window::far (float value)
{
   switch (p_viewmode)
   {
    case GR_ORTHO:
	case GR_ORTHO2:
      p_viewparams.orthoView.far = value;
      break;
      
    case GR_PERSPECTIVE:
      p_viewparams.perspectiveView.far = value;
      break;
   }
}

void
GR_Window::left (float value)
{
   if (p_viewmode != GR_ORTHO && p_viewmode != GR_ORTHO2)
     return;
   
   p_viewparams.orthoView.left = value;
}

void
GR_Window::right (float value)
{
   if (p_viewmode != GR_ORTHO && p_viewmode != GR_ORTHO2)
     return;
   
   p_viewparams.orthoView.right = value;
}

void
GR_Window::bottom (float value)
{
   if (p_viewmode != GR_ORTHO && p_viewmode != GR_ORTHO2)
     return;
   
   p_viewparams.orthoView.bottom = value;
}

void
GR_Window::top (float value)
{
   if (p_viewmode != GR_ORTHO && p_viewmode != GR_ORTHO2)
     return;
   
   p_viewparams.orthoView.top = value;
}


float
GR_Window::field_of_view ()
{
   if (p_viewmode != GR_PERSPECTIVE)
     return 0.0;
   
   return p_viewparams.perspectiveView.fov;
}

float
GR_Window::aspect ()
{
   if (p_viewmode != GR_PERSPECTIVE)
     return 0.0;
   
   return p_viewparams.perspectiveView.aspect;
}

float
GR_Window::near ()
{
   switch (p_viewmode)
   {
    case GR_ORTHO:
	case GR_ORTHO2:
      return p_viewparams.orthoView.near;
    case GR_PERSPECTIVE:
      return p_viewparams.perspectiveView.near;
   }
}

float
GR_Window::far ()
{
   switch (p_viewmode)
   {
    case GR_ORTHO:
	case GR_ORTHO2:
      return p_viewparams.orthoView.far;
      
    case GR_PERSPECTIVE:
      return p_viewparams.perspectiveView.far;
   }
}

float
GR_Window::left ()
{
   if (p_viewmode != GR_ORTHO && p_viewmode != GR_ORTHO2)
     return 0.0;
   
   return p_viewparams.orthoView.left;
}

float
GR_Window::right ()
{
   if (p_viewmode != GR_ORTHO && p_viewmode != GR_ORTHO2)
     return 0.0;
   
   return p_viewparams.orthoView.right;
}

float
GR_Window::bottom ()
{
   if (p_viewmode != GR_ORTHO && p_viewmode != GR_ORTHO2)
     return 0.0;
   
   return p_viewparams.orthoView.bottom;
}

float
GR_Window::top ()
{
   if (p_viewmode != GR_ORTHO && p_viewmode != GR_ORTHO2)
     return 0.0;
   
   return p_viewparams.orthoView.top;
}

void
GR_Window::twist (float value)
{
   p_twist = (long) (value);
//   p_twist = (long) (10.0 * value);
}

void
GR_Window::view_position (GR_point value)
{
   p_viewpos = value;
}

void
GR_Window::view_position (float x, float y, float z)
{
   p_viewpos.x = x;
   p_viewpos.y = y;
   p_viewpos.z = z;
//   view_position (GR_Point (x, y, z));
}

void
GR_Window::look_vector (GR_point vector)
{
   p_lookvec = vector;
}

void
GR_Window::look_vector (float x, float y, float z)
{
   p_lookvec.x = x;
   p_lookvec.y = y;
   p_lookvec.z = z;
//   look_vector (GR_Point (x, y, z));
}

long
GR_Window::get_viewmode ()
{
   return p_viewmode;
}

void
p_initCB (Widget w, caddr_t client_data, caddr_t callptr)
{
   Window	overlayWindow;
   GR_Window    *window;
   GLwDrawingAreaCallbackStruct *call_data;
   
// installColormapWithOverlay (GR_toplevel, w);
   installColormap (GR_toplevel, w);
   call_data = (GLwDrawingAreaCallbackStruct *) callptr;
   window = (GR_Window *) client_data;
   
// XtVaGetValues (w, GlxNoverlayWindow, &overlayWindow, NULL);   

   GR_zbuffer(TRUE);
   GR_mmode(MVIEWING);
//   GLXwinset(XtDisplay(w), overlayWindow, window->p_context);
   GR_mapcolor (1, 255, 0, 0);
   GR_mapcolor (2, 0, 255, 0);
   
   // my old stuff:   
   //background = WidgetBackgroundToGlRgb(w);
// GR_glcompat(GLC_ZRANGEMAP, 0);
}


void
p_inputCB (Widget, caddr_t client_data, caddr_t call_data_ptr)
{
   char buffer[1];
   GLwDrawingAreaCallbackStruct *call_data;
   KeySym keysym;
   GR_Window		*window;

   int charcount;

   // my stuff:
   static short dx, dy;
   Boolean shiftmode = FALSE;
   char    message[80];
 
   window = (GR_Window *) client_data;
   
   call_data = (GLwDrawingAreaCallbackStruct *) call_data_ptr;

   window->p_event.lastx = window->p_event.x;
   window->p_event.lasty = window->p_event.y;
   window->p_event.x = call_data->event->xbutton.x;
   window->p_event.y = call_data->event->xbutton.y;
   GR_MouseX = window->p_event.x;
   GR_MouseY = window->p_event.y;
   
   switch(call_data->event->type)
   {
    case KeyRelease:
      buffer [0] = 0;
      charcount =
	XLookupString((XKeyEvent *) call_data->event,buffer,1,&keysym,NULL);
      if (charcount == 1 && keysym == (KeySym)XK_Escape)
	exit(0);
      printf("Key release = %s\n", buffer[0]);
      break;

    case ButtonPress:
      window->p_event.down = 1;
      if (!window->p_event.moving)
      {
	 window->p_event.downx = window->p_event.x;
	 window->p_event.downy = window->p_event.y;
      }
     
      // my old stuff:
      dx = window->p_event.x;
      dy = window->p_event.y;

      switch(call_data->event->xbutton.button)
      {
       case Button1:
	 if (call_data->event->xbutton.state & ShiftMask)
	 {
	    // printf("  Shift_Button1 pressed.\n");
	    shiftmode = TRUE;
 	    // action reserved;
	 }
	 else
	 {
	    // printf("  Button1 pressed.\n");
	    window->p_event.button = GR_LEFTMOUSE;
	    window->p_event.buttonstate.left = 1;
	 }
	 break;
       case Button2:
	 window->p_event.button = GR_MIDDLEMOUSE;
	 window->p_event.buttonstate.middle = 1;
	 break;
       case Button3:
	 window->p_event.button = GR_RIGHTMOUSE;
	 window->p_event.buttonstate.right = 1;
	 break;
      }
      break;

    case ButtonRelease:
      window->p_event.down = 0;
      switch(call_data->event->xbutton.button)
      {
       case Button1:
	 shiftmode = FALSE;
	 window->p_event.button = GR_LEFTMOUSE;
	 window->p_event.buttonstate.left = 0;
	 break;
       case Button2:
	 window->p_event.button = GR_MIDDLEMOUSE;
	 window->p_event.buttonstate.middle = 0;
	 break;
       case Button3:
	 window->p_event.button = GR_RIGHTMOUSE;
	 window->p_event.buttonstate.right = 0;
	 break;
      }
      break;

    case MotionNotify:
      window->p_event.moving = 1;
      if ((call_data->event->xmotion.state & Button1Mask) &&
          (call_data->event->xmotion.state & ShiftMask))
      {
	 shiftmode = TRUE; 
	 GR_rotate(call_data->event->xbutton.x - dx, 'y');
	 GR_rotate(call_data->event->xbutton.y - dy, 'x');
	 window->set_freeview (TRUE); 
         window->draw();
         window->set_freeview (FALSE);
      }
      break;
   }

   // new stuff, modified by Tung:
   if (!shiftmode)
     window->p_processEvent ();

   // comment out the old stuff:
   /*
   GLXwinset(XtDisplay(w), call_data->window, window->p_context);
   if ((window->p_event.down == 1) && !shiftmode)
   {
      printf ("  doing picking....\n\n");
      GR_bgnpick ();
      window->draw ();
      GR_endpick ();
      
      object = (GR_DispObj *) GR_top_hit ();
      if (object && window->p_pick_event_object)
      {
	 if (!window->p_event.down)
	   return;
	 altobj = window->p_pick_event_object;
	 window->p_pick_event_object = 0;
	 altobj->pick_event (window->p_event, object, window);
      }

      else if (object)
	object->process_pick (window->p_event, window);
   }
   */
   
}

void
GR_Window::p_processEvent ()
{
   GR_DispObj      *object=NULL, *requestObj;
   long requestType;
   
   if (p_nextRequest < 0)
   {
      requestType = GR_EMPTY;
      requestObj = 0;
   }
   else
   {
      requestType = p_requestStack [p_nextRequest].type;
      requestObj = p_requestStack [p_nextRequest].requestObj;
   }
   GLXwinset(XtDisplay (p_widget), XtWindow (p_widget), p_context);
   if (p_event.moving)
   {
      if (p_event.buttonstate.left == 0  // has the last button been released?
	  && p_event.buttonstate.middle == 0
	  && p_event.buttonstate.right == 0)
      {
	 // if done dragging, process the result
	 
	 switch (requestType)
	 {
	  case GR_GET_RECT_EVENT:
	  case GR_GET_RECT:
	    p_drawOverlayRect (0, 0);
	    break;
	  case GR_GET_RECT_EVENT_KA:
	  case GR_GET_RECT_KA:
	    p_drawOverlayRect (0, 1);
	    break;
	  case GR_GET_PICK_EVENT:
	  case GR_GET_OBJ:
	  case GR_EMPTY:
	    /*
            GR_bgnpick ();
	    draw ();
	    GR_endpick ();
	    object = (GR_DispObj *) GR_top_hit ();
	    */
            break;
          default:
            break;
	 }
	 
	 switch (requestType)
	 {
	  case GR_GET_RECT_EVENT:
	    if (requestObj)
	      requestObj->rectEvent
		(p_event.downx, p_event.downy, p_event.x, p_event.y);
	    else
	      rectEvent (p_event.downx, p_event.downy, p_event.x, p_event.y);
	    p_nextRequest--;
	    break;
	  case GR_GET_RECT_EVENT_KA:
	    if (requestObj)
	      requestObj->rectEvent
		(p_event.downx, p_event.downy, p_event.x, p_event.y);
	    else
	      rectEvent (p_event.downx, p_event.downy, p_event.x, p_event.y);
	    p_nextRequest--;
	    break;
	  case GR_GET_RECT:
	  case GR_GET_RECT_KA:
	    p_nextRequest--;
	    break;
	  case GR_GET_DRAG_EVENT:
	    requestObj->pickEvent (p_event, this);
	    p_nextRequest--;
	    break;
	  case GR_GET_PICK_EVENT:
	    requestObj->pickEvent (p_event, object, this);
	    p_nextRequest--;
	    break;
	  case GR_GET_OBJ:
	    p_pickedDispObj = object;
	    p_nextRequest--;
	    break;
	  case GR_EMPTY:
	    /*
            if (object)
	      object->pickEvent (p_event, this);
	    */
            break;
          default:
            break;
	 }
	 p_event.moving = 0;
      }
      else // still dragging
      {
	 switch (requestType)
	 {
	  case    GR_GET_RECT_EVENT:
	  case    GR_GET_RECT:
	    p_drawOverlayRect (1, 0);
	    break;
	  case GR_GET_RECT_EVENT_KA:
	  case GR_GET_RECT_KA:
	    p_drawOverlayRect (1, 1);
	    break;
	  case GR_GET_DRAG_EVENT:
	    requestObj->dragEvent (p_event, this);
	    break;
	 }
      }
   }
   else
   {
      switch (requestType)
      {
       case GR_GET_RECT:
       case GR_GET_RECT_EVENT:
       case GR_GET_RECT_KA:
       case GR_GET_RECT_EVENT_KA:
	 break;
       case GR_EMPTY:
         if (p_event.down)
         {
            //printf ("---> DRE now doing non-moving picking....\n");
	    GR_bgnpick ();
	    draw ();
	    GR_endpick ();
	    object = (GR_DispObj *) GR_top_hit ();
	    if (object)
	      object->pickEvent (p_event, this);
         }
         else
         {
             // printf ("---> skip picking....\n");
         }
	 break;
       case GR_GET_OBJ:
       case GR_GET_PICK_EVENT:
	 if (!p_event.down)
	 {
	    GR_bgnpick ();
	    draw ();
	    GR_endpick ();
	    
	    printf("Get picked object\n");
	    p_pickedDispObj = (GR_DispObj *) GR_top_hit ();
	    if (requestType == GR_GET_PICK_EVENT)
	      requestObj->pickEvent (p_event, p_pickedDispObj, this);
	    p_nextRequest--;
	 }
	 break;
       default:
         break;
      }
   }
}


void
p_exposeCB (Widget, caddr_t clientdata, caddr_t cdata)
{
   GR_DispListElem      *ptr;
   GR_Window               *window;
   GLwDrawingAreaCallbackStruct *call_data;

//   printf (" p_exposeCB is called \n");

   window = (GR_Window *) clientdata;
   call_data = (GLwDrawingAreaCallbackStruct *) cdata;
   window->p_width = call_data->width;
   window->p_height = call_data->height;
//   glScalef(10.0, 10.0, 10.0);
   window->draw ();
}

void
GR_Window::draw (GR_DispObj *object)
{
   long status;
   
   if (!object)
     return;
   
   status = GLXwinset (XtDisplay (p_widget), XtWindow (p_widget), p_context);
   if (status < 0)
   {
      fprintf (stderr, "Bad return from GLXwinset\n");
      exit (-1);
   }
   
   this->setViewTrans ();
   GR_frontbuffer (TRUE);
   object->draw ();
   GR_frontbuffer (FALSE);
}

void
GR_Window::draw ()
{
   GR_DispListElem	*ptr;
   long status;
   int  listcount;

   p_requestDraw = 0;
   p_lastDrawTime = time (0);

   status = GLXwinset (XtDisplay (p_widget), XtWindow (p_widget), p_context);
   if (status < 0)
   {
      fprintf (stderr, "Bad return from GLXwinset\n");
      exit (-1);
   }

   // my old stuff, still fine:
   if (p_viewmode == GR_PERSPECTIVE)
   {
     if (p_height == 0)
     {
        long wxsize, wysize;
        GR_getsize(&wxsize, &wysize);
        if (wysize == 0)
        {
	 printf(" Warning: can't get ysize of the window.\n");
	 aspect(1.0);
        }
        else
        {
	 //printf("Warning: use aspect: %f.\n", (float)wxsize/(float)wysize);
	 aspect ((float)wxsize/(float)wysize);
        }
     }
     else
       aspect ((float)p_width/(float)p_height);
   }

//   printf("Expose win size = %d  %d\n",p_width,p_height);
   GR_czclear(p_background_color, getgdesc(GD_ZMAX));

   if (!GR_pickmode())
      this->drawBackground ();

   this->setViewTrans ();
   ptr = p_displist_list;
   listcount = 0;
   while (ptr) {
      if (ptr->displist)  // condition added on 12/7/92 by Y. Tung
         ptr->displist->drawobjs();
      ptr = ptr->next;
      listcount = listcount+1;
   }
   //printf("GR_Window: drawing %d display lists\n", listcount);

   // note that sequence of drawing is diff from jbl's:
   if ( p_drawfunction && !GR_pickmode() )
      (*p_drawfunction)();

   if ( !GR_pickmode() ) {
      GR_swapbuffers();
      GLwDrawingAreaSwapBuffers(p_widget);
      glFinish();
      if (RECORDING) recordCB(NULL, (XtPointer)2, NULL);
   }

   if (p_freeview == TRUE)
      p_freeview = FALSE;
}

void
GR_Window::setViewTrans ()
{
GLint     viewport[4];
double    pick_x=10.0, pick_y=10.0;

   //printf("SetView with mode = %d\n", p_viewmode);

   glMatrixMode(GL_PROJECTION);
   glLoadIdentity();

   if (GR_pickmode()) {
       glGetIntegerv (GL_VIEWPORT, viewport);
       gluPickMatrix ((GLdouble)GR_MouseX, (GLdouble)viewport[3]-(GLdouble)GR_MouseY, 
                      pick_x, pick_y, viewport);
       //printf("WinDraw: Mouse at %d %d.\n", GR_MouseX, GR_MouseY);
   }

   if (p_freeview) printf("SetViewTrans: freeview is TRUE\n");
   switch (p_viewmode) {
      case GR_ORTHO:
         GR_ortho (p_viewparams.orthoView.left,
		   p_viewparams.orthoView.right,
		   p_viewparams.orthoView.bottom,
		   p_viewparams.orthoView.top,
		   p_viewparams.orthoView.near,
		   p_viewparams.orthoView.far);
         break;
      case GR_ORTHO2:
         GR_ortho2 (p_viewparams.orthoView.left,
		    p_viewparams.orthoView.right,
		    p_viewparams.orthoView.bottom,
		    p_viewparams.orthoView.top);
         break;
      case GR_PERSPECTIVE:
	//if (!p_freeview) glLoadIdentity();  
         GR_perspective (p_viewparams.perspectiveView.fov,
		         p_viewparams.perspectiveView.aspect,
		         p_viewparams.perspectiveView.near,
		         p_viewparams.perspectiveView.far);
         break;
   }

   glMatrixMode(GL_MODELVIEW);
   glLoadIdentity();

   if (!p_freeview)
      GR_lookat (p_viewpos, p_lookvec, p_twist);
}
   
void
GR_Window::convert_screen2 (short x, short y, float& xwind, float& ywind)
{
	float left, right, bottom, top;

	if (p_viewmode == GR_PERSPECTIVE)
		return;

	left = p_viewparams.orthoView.left; 
	right = p_viewparams.orthoView.right; 
	bottom = p_viewparams.orthoView.bottom; 
	top = p_viewparams.orthoView.top;

	xwind = // left edge + percentage across * total width
		left
		+ (float)x / (float)p_width
		* (right - left);

	ywind = // bottom edge + percentage up * total height
		bottom
		+ (1.0 - (float)y / (float)p_height)
		* (top - bottom);
}


void
p_resizeCB (Widget w, caddr_t client_data, caddr_t call)
{
	GR_Window		*window;
	GLwDrawingAreaCallbackStruct *callData;

	window = (GR_Window *) client_data;
	
	callData = (GLwDrawingAreaCallbackStruct *) call;
	printf("Resize to %d  %d\n", callData->width, callData->height);
	GLXwinset (XtDisplay (w), XtWindow (w), window->p_context);
	glXWaitX();
	glViewport(0, 0, callData->width, callData->height);
//	GR_reshapeviewport ();
}



/* new stuff ... */

void
GR_Window::mouse_getRectangle (short &x1, short &y1, short &x2, short &y2,
			       long aspectflag)
{
   XEvent xevent;
   long	  request_id;
   
   if (aspectflag)
     request_id = p_pushRequest (GR_GET_RECT_KA, 0);
   else
     request_id = p_pushRequest (GR_GET_RECT, 0);
   
   printf ("In mouse_getRectangle...\n");
   while (1)
   {
      XtAppNextEvent (GR_appcontext, &xevent);
      XtDispatchEvent (&xevent);
      switch(xevent.type)
      {
         case ButtonPress:
          x1 = p_event.downx;
          y1 = p_event.downy;
          break;
         case ButtonRelease:
          x2 = p_event.x;
          y2 = p_event.y;
          return;
          break;
      }
//      if (p_doneRequest (request_id))
//	break;
   }

   x1 = 100; //p_event.downx;
   y1 = 100; //p_event.downy;
   x2 = 200; //p_event.x;
   y2 = 175; //p_event.y;

   printf ("Done with mouse_getRectangle\n");
}

GR_DispObj*
GR_Window::mouse_getDispObj ()
{
   XEvent xevent;
   long request_id;
   
   request_id = p_pushRequest (GR_GET_OBJ, 0);
   
   //printf ("In mouse_getDispObj...\n");
   while (1)
   {
      XtAppNextEvent (GR_appcontext, &xevent);
      XtDispatchEvent (&xevent);
      if (p_doneRequest (request_id))
	break;
   }
   //printf ("Done with mouse_getDispObj\n");
   return p_pickedDispObj;
}

void
GR_Window::p_drawOverlayRect (long onoff, long keepaspect)
{
   float aspect;
   Window overlayWindow;

// XtVaGetValues(p_widget, GlxNoverlayWindow, &overlayWindow, NULL);
//   if (GLXwinset (XtDisplay (p_widget), overlayWindow, p_context) < 0)
//     printf ("GLXwinset failed\n");
   
   if (keepaspect)
   {
      aspect = (float) p_height / (float) p_width;
      
      if (aspect > 1.0)
      {
	 if (p_event.x < p_event.downx)
	   p_event.x = (short) (p_event.downx -
				fabs ((p_event.y - p_event.downy)) / aspect);
	 else
	   p_event.x = (short) (p_event.downx +
				fabs ((p_event.y - p_event.downy)) / aspect);
      }
      else
      {
	 if (p_event.y < p_event.downy)
	   p_event.y = (short) (p_event.downy -
				aspect * fabs (p_event.x - p_event.downx));
	 else
	   p_event.y = (short) (p_event.downy +
				aspect * fabs (p_event.x - p_event.downx));
      }
   }

   // add to prevent permanent change of the linewidth..., by Tung 01/22/93:
   GR_pushattributes();
   GR_zbuffer (FALSE);
   GR_color (0,0,0);
   GR_clear ();
   if (onoff)
   {
      GR_ortho2 (0.0, (float)p_width - 1.0, 0.0, (float)p_height - 1.0);
      GR_color (255,255,255);
      GR_linewidth (2);
      GR_rect ( (float) p_event.downx, (float) p_height - p_event.downy,
	       (float) p_event.x, (float) p_height - p_event.y);
   }
   GR_mswapbuffers (OVERDRAW);
   GR_zbuffer (TRUE);
   GR_popattributes();
}

long
GR_Window::p_pushRequest (long type, GR_DispObj* object)
{
   if (p_nextRequest == GR_WIN_MAX_REQ - 1)
   {
      fprintf (stderr, "ERROR: GR_Window::p_pushRequest -- stack overflow\n");
      exit (-1);
   }
   p_nextRequest++;
   p_requestStack [p_nextRequest].type = type;
   p_requestStack [p_nextRequest].requestObj = object;
   return p_nextRequest;
}

long
GR_Window::p_doneRequest (long request_id)
{
   if (p_nextRequest < request_id)
     return 1;
   else
     return 0;
}


void
GR_Window::cancel_getRectangle ()
{
   if (p_nextRequest < 0)
     return;
   
   if (p_requestStack [p_nextRequest].type == GR_GET_RECT_EVENT
       || p_requestStack [p_nextRequest].type == GR_GET_RECT_EVENT_KA
       || p_requestStack [p_nextRequest].type == GR_GET_RECT
       || p_requestStack [p_nextRequest].type == GR_GET_RECT_KA)
   {
      p_nextRequest--;
      p_event.downx = 0;
      p_event.downy = 0;
      p_event.x = 0;
      p_event.y = 0;
   }
}

GR_DispList*
GR_Window::getDispList (char *name)
{
   GR_DispListElem *ptr;
   
   ptr = p_displist_list;
   
   if (name == 0 && ptr)
   {
      if (ptr->name == 0)
	return ptr->displist;
      else
	return 0;
   }
   
   while (ptr)
	{
	   if (strcmp (name, ptr->name) == 0)
	     return ptr->displist;
	   ptr = ptr->next;
	}
   return 0;
}


void
GR_Window::cancel_mouse_get ()
{
   p_nextRequest = -1;
   p_pickedDispObj = 0;
}

void
GR_Window::setOverlay ()
{
   Window overlayWindow;
   Window GlxNoverlayWindow; 
/*
   XtVaGetValues(p_widget, 
		 GlxNoverlayWindow, &overlayWindow,
		 NULL);
   if (GLXwinset (XtDisplay (p_widget), overlayWindow, p_context) < 0)
     printf ("GR_Window::setOverlay: GLXwinset failed\n");
*/
}




/* ===== added 12/7/92 ===== */

void 
GR_Window::remDispList (GR_DispList*, char* name)
{
  GR_DispListElem *ptr = p_displist_list;

  for (; ptr!=NULL; ptr=ptr->next)
  {
    if (ptr->name && strcmp (ptr->name,name)==0)
    {
       printf (" display list %s will be removed.\n",name);
       ptr->displist=NULL;
       return;
    }
  }
}

void
GR_Window::rem_all_namedDispLists ()
{
  GR_DispListElem *ptr = p_displist_list;

  if (ptr)
  {
     if (ptr->name)
        ptr = NULL;
     else
        ptr->next=NULL;
  }
}


/* ===== added 1/21/93 ===== */

void
p_timedraw (XtPointer w, XtIntervalId *)
{
  GR_Window *win;
  win = (GR_Window*)w;

  if (win->get_requestDraw() > 0)
  {
     //printf ("...FYI: clearing up %d request_draw()'s.\n", 
     //         win->get_requestDraw());
     win->set_requestDraw(0);
     win->draw ();
     win->timeout_id = XtAppAddTimeOut (GR_appcontext, 
                         win->get_timedraw_period(), 
                         p_timedraw,
                         (XtPointer)w);
  }
  else if (win->get_requestDraw() == 0)
  {
     win->timeout_id = XtAppAddTimeOut (GR_appcontext, 
                         win->get_timedraw_period(), 
                         p_timedraw, 
                         (XtPointer)w);
  }
  else
  {
    printf ("Warning: %s's window->p_timedraw routine terminates.\n", 
             XtName(win->widget()));
    win->timeout_id = 0;
  }
}    



void
GR_Window::request_draw (long period)
{
  if (period < 0) // remove timeout routine:
  {
     set_requestDraw(-1);
     if (timeout_id != 0)
       XtRemoveTimeOut (timeout_id);
     timeout_id = 0;
  }
  else            // set up timeout routine:
  {
     if (timeout_id==0)  // timeout routine not exist:
     {
       set_timedraw_period(period);
       timeout_id = XtAppAddTimeOut (GR_appcontext, period,
                                     p_timedraw, (XtPointer)this);
     }
     else if (get_timedraw_period()!=period) // modify timeout routine:
     {
       XtRemoveTimeOut (timeout_id);
       set_timedraw_period(period);
       timeout_id = XtAppAddTimeOut (GR_appcontext, period,
                                     p_timedraw, (XtPointer)this);
     }
     p_requestDraw++;
  }
}


