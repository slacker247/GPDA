#include "GR_Interface.H"
#include <X11/Xirisw/GlxMDraw.h>
#include <X11/keysym.h>
#include <X11/StringDefs.h>
#include <stdio.h>
#include <Xm/Frame.h>

GR_MouseEvent GR_glxWidget::p_event =
{
	0, // x
	0, // y
	0, // lastx
	0, // lasty
	0, // button
	0, // moving
	0, // down
	{ 0, 0, 0 } // buttonstate
};

GR_glxWidget::GR_glxWidget ()
{
	p_drawfunction = 0;
	p_config.doublebuffer = 0;
	p_config.rgbmode = 0;
	p_background_color = GR_get_color (0, 0, 0);
}

Widget
GR_glxWidget::v_createWidget (char *name, GR_Widget *parent)
{
	Arg args [10];
	int n;
	char string [32];
	Widget w, frame;

	p_glconfig [0].buffer = GLX_NORMAL;
	p_glconfig [0].mode = GLX_DOUBLE;
	if (p_config.doublebuffer)
		p_glconfig [0].arg = TRUE;
	else
		p_glconfig [0].arg = FALSE;

	p_glconfig [1].buffer = GLX_NORMAL;
	p_glconfig [1].mode = GLX_RGB;
	if (p_config.rgbmode)
		p_glconfig [1].arg = TRUE;
	else
		p_glconfig [1].arg = FALSE;

	p_glconfig [2].buffer = 0;
	p_glconfig [2].mode = 0;
	p_glconfig [2].arg = 0;

	frame = XtVaCreateManagedWidget ("frame", xmFrameWidgetClass, parent->widget(), 0);
	sprintf (string, "%s_glwidget", name);
	w = XtVaCreateManagedWidget (string, glxMDrawWidgetClass,
				frame, 
				GlxNglxConfig, p_glconfig,
				NULL);

	XtAddCallback (w, GlxNexposeCallback, p_exposeCB, (XtPointer) this);
	XtAddCallback (w, GlxNginitCallback, p_initCB, (XtPointer) this);
	XtAddCallback (w, GlxNresizeCallback, p_resizeCB, (XtPointer) this);
	XtAddCallback (w, GlxNinputCallback, p_inputCB, (XtPointer) this);

	return frame;
}

void
p_initCB (Widget, caddr_t, caddr_t)
{
}

void
p_inputCB (Widget w, caddr_t client_data, caddr_t call_data_ptr)
{
	char buffer[1];
	GlxDrawCallbackStruct *call_data;
	KeySym keysym;
	GR_glxWidget		*window;
	GR_DispObj	*object, *altobj, *keyobj;
	int charcount;

	window = (GR_glxWidget *) client_data;

	call_data = (GlxDrawCallbackStruct *) call_data_ptr;
	GLXwinset(XtDisplay(w), call_data->window);

	window->p_event.lastx = window->p_event.x;
	window->p_event.lasty = window->p_event.y;
	window->p_event.x = call_data->event->xbutton.x;
	window->p_event.y = call_data->event->xbutton.y;

	switch(call_data->event->type)
	{
    case KeyRelease:
			/* It is necessary to convert the keycode to a keysym before
			 * it is possible to check if it is an escape
		  */
			buffer [0] = 0;
			charcount = XLookupString((XKeyEvent *) call_data->event,buffer,1,&keysym,NULL);
			if (charcount == 1 && keysym == (KeySym)XK_Escape)
				exit(0);
			else if (buffer [0] && window->p_key_event_object) 
			{
				keyobj = window->p_key_event_object;
				window->p_key_event_object = 0;
				keyobj->key_event (buffer [0], window);
			}
			break;
    case ButtonPress:
			window->p_event.down = 1;
			switch(call_data->event->xbutton.button)
			{
				case Button1:
					window->p_event.button = GR_LEFTMOUSE;
					window->p_event.buttonstate.left = 1;
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
			printf ("dragging...\n");
			window->p_event.moving = 1;
			break;
	}

	if (window->p_event.moving)
	{
		if (window->p_event.buttonstate.left == 0 
				&& window->p_event.buttonstate.middle == 0
				&& window->p_event.buttonstate.right == 0)
		{
			if (window->p_drag_event_object)
				window->p_drag_event_object->process_pick (window->p_event, window);
			window->p_event.moving = 0;
			window->p_drag_event_object = 0;
		}
		else if (window->p_drag_event_object)
			window->p_drag_event_object->drag_event (window->p_event, window);

		return;
	}

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

void
p_exposeCB (Widget, caddr_t clientdata, caddr_t)
{
	GR_glxWidget		*window;

	window = (GR_glxWidget *) clientdata;

	window->draw ();
}

void
GR_glxWidget::draw ()
{
	GR_DispListElem	*ptr;

	printf ("GR_glxWidget::draw ()\n");
	GLXwinset (XtDisplay (this->widget()), XtglxWidget (this->widget()));
	GR_color (p_background_color);
	GR_clear ();
	GR_lookat (p_viewpos, p_lookvec, p_twist);
	switch (p_viewmode)
	{
		case GR_ORTHO:
			GR_ortho (
				p_viewparams.orthoView.left, 
				p_viewparams.orthoView.right, 
				p_viewparams.orthoView.bottom, 
				p_viewparams.orthoView.top, 
				p_viewparams.orthoView.near, 
				p_viewparams.orthoView.far
			);
			break;
		case GR_ORTHO2:
			GR_ortho2 (
				p_viewparams.orthoView.left, 
				p_viewparams.orthoView.right, 
				p_viewparams.orthoView.bottom, 
				p_viewparams.orthoView.top 
			);
			break;
		case GR_PERSPECTIVE:
			GR_perspective (
				p_viewparams.perspectiveView.fov,
				p_viewparams.perspectiveView.aspect,
				p_viewparams.perspectiveView.near,
				p_viewparams.perspectiveView.far
			);
			break;
	}

	if ( p_drawfunction && !GR_pickmode() )
		(*p_drawfunction)();

	ptr = p_displist_list;
	while (ptr)
	{
		ptr->displist->drawobjs();
		ptr = ptr->next;
	}

	if ( !GR_pickmode() )
		GR_swapbuffers();
}

void
GR_glxWidget::convert_screen2 (short x, short y, float& xwind, float& ywind)
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
p_resizeCB (Widget w, caddr_t client_data, caddr_t data)
{
	GlxDrawCallbackStruct *call_data;
	GR_glxWidget		*window;

	window = (GR_glxWidget *) client_data;
	
	call_data = (GlxDrawCallbackStruct *) data;
	window->p_width = call_data->width;
	window->p_height = call_data->height;

	GLXwinset (XtDisplay (w), XtWindow (w));
	GR_reshapeviewport ();
}
