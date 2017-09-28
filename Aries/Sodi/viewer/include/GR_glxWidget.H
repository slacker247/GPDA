#ifndef GR_GLXWIDGET
#define GR_GLXWIDGET

#define INLINE inline

#include "GR_Widget.H"

typedef void (*GR_glxWidgetDrawFunction)();
typedef struct {
	int buffer;
	int mode;
	int arg;
} GLXconfig;


class GR_glxWidget : public GR_Widget
{
	private:
		static GR_MouseEvent		p_event;
		GLXconfig		p_glconfig [3];
		long p_background_color;

		void (*p_drawfunction)();

		friend void p_initCB (Widget, caddr_t, caddr_t);
		friend void p_exposeCB (Widget, caddr_t, caddr_t);
		friend void p_resizeCB (Widget, caddr_t, caddr_t);
		friend void p_inputCB (Widget, caddr_t, caddr_t);
		Widget v_createWidget (char *name, GR_Widget *);
	public:
		GR_glxWidget ();
		virtual void exposeEvent (){}
		virtual void resizeEvent (){}
		virtual void inputEvent (GR_MouseEvent *){}

		void color (short, short, short);
		void set_doublebuffer ();
		void set_rgbmode ();
		GR_MouseEvent*	mouse_event();
};

INLINE GR_MouseEvent*
GR_glxWidget::mouse_event ()
{
	return &p_event;
}

INLINE void
GR_glxWidget::set_drawfunction (GR_glxWidgetDrawFunction drawfunction)
{
	p_drawfunction = drawfunction;
}

INLINE void
GR_glxWidget::set_doublebuffer ()
{
	p_config.doublebuffer = 1;
}

INLINE void
GR_glxWidget::set_rgbmode ()
{
	p_config.rgbmode = 1;
}

INLINE void
GR_glxWidget::color (short red, short grn, short blu)
{
	p_background_color = GR_get_color (red, grn, blu);
}
#endif
