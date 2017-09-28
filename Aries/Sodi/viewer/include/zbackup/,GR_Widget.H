#ifndef GR_WIDGET
#define GR_WIDGET

#include <Xm/Xm.h>

struct GR_WidgetFlags
{
	unsigned dynamic : 1;
	unsigned created : 1;
	unsigned realized : 1;
};

class GR_Widget
{
	private:
		Widget		p_this;
		Widget		p_parent;
		friend void p_destroyWidget (Widget, XtPointer, XtPointer);
	protected:
		GR_WidgetFlags		p_widget_flags;
		virtual Widget		v_createWidget (char *name, GR_Widget *parent = 0);
	public:
		GR_Widget ();
		virtual ~GR_Widget ();
		Widget widget ();
		Widget parent ();
		void realize ();
		void createWidget (char *name, GR_Widget *parent = 0);
		void createWidget (char *name, GR_Widget &parent);
		void createWidget (char *name, Widget parent);
		void* operator new (size_t);
		void operator delete (void*, size_t);
};

inline Widget
GR_Widget::widget ()
{
	return p_this;
}

inline Widget
GR_Widget::parent ()
{
	return p_parent;
}

inline void
GR_Widget::realize ()
{
	if (p_widget_flags.created)
		XtRealizeWidget (p_this);
}
#endif
