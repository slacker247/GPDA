#include "GR_Widget.H"
#include <stdio.h>
#include <stdlib.h>
#include <X11/StringDefs.h>

#include <unistd.h>

GR_Widget::GR_Widget ()
{
	//p_widget_flags.dynamic = 0;
	p_widget_flags.created = 0;
	p_widget_flags.realized = 0;
	p_widget_flags.Xdelete = 0;
}

GR_Widget::~GR_Widget ()
{
}

void*
GR_Widget::operator new (size_t sz)
{
	GR_Widget	*wptr;
	wptr = ::new GR_Widget[sz]; 
	wptr->p_widget_flags.dynamic = 1;
	return wptr;
}

Widget
GR_Widget::v_createWidget (char *, Widget)
{
   fprintf (stderr, "ERROR: GR_Widget::v_createWidget called directly\007\n");
   sleep (1);
   fprintf (stderr,"\007\n"); 
   sleep (1);
   fprintf (stderr,"\007\n"); 
   return NULL;
}

void
GR_Widget::operator delete (void *ptr, size_t)
{
	GR_Widget *wptr;

	wptr = (GR_Widget *) ptr;
	if (wptr->p_widget_flags.dynamic)
	{
		if (wptr->p_widget_flags.Xdelete)
			::delete wptr;
		else
			XtDestroyWidget (wptr->widget ());
	}
	//else
		//printf ("Attempt to delete a nondynamic GR_Widget\n");
}

void
GR_Widget::createWidget (char* name, GR_Widget *parent)
{
	if (parent)
		p_parent = parent->widget ();
	else
		p_parent = 0;

	createWidget (name, p_parent);
}

void
GR_Widget::createWidget (char* name, GR_Widget &parent)
{
	createWidget (name, &parent);
}

void
p_destroyWidget (Widget, XtPointer client_data, XtPointer)
{
	GR_Widget *wptr;

	wptr = (GR_Widget *) client_data;
	wptr->p_widget_flags.Xdelete = 1;

	delete wptr;
}

void
GR_Widget::createWidget (char *name, Widget w)
{
	p_this = this->v_createWidget (name, w);
  
        if (p_this)
        {
           //printf (" Widget %s is created via v_createWidget.\n", 
           //          XtName(p_this));
	   p_widget_flags.created = 1;
	   XtAddCallback (p_this, XmNdestroyCallback, p_destroyWidget, (XtPointer)this);
        }
        else
           printf ("Warning: %s->v_createWidget is failed?.\007\n",
                   name); 
}
