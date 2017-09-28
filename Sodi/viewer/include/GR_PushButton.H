#ifndef GR_PUSHBUTTON
#define GR_PUSHBUTTON

#define INLINE inline

#include "GR_Widget.H"
class GR_PushButton;
typedef long (*GR_PushButtonFunc) (GR_PushButton*, void* contextobj, void* data);

class GR_PushButton : public GR_Widget
{
	private:
		void			*p_object;
		void			*p_data;
		GR_PushButtonFunc		p_function;
		Widget v_createWidget (char *, GR_Widget*);
		friend void p_activateCB (Widget, caddr_t, caddr_t);
	protected:
		long exec_function ();
	public:
		GR_PushButton ();

		void button_width (float factor) {}

		void button_height (float factor) {}

		void set_function (GR_PushButtonFunc function, void *object = 0);

		//void set_menu			(C_PopUp *);

		void	set_data (void *data);

		void set_enable (long tf){}

		void do_button ();
};


INLINE void
GR_PushButton::set_function (GR_PushButtonFunc function, void *object)
{
	p_function = function;
	p_object = object;
}

/*
INLINE void
GR_PushButton::set_menu (C_PopUp *menu)
{
	p_menu = menu;
}
*/

INLINE long
GR_PushButton::exec_function ()
{
	/*
	if (p_menu)
		p_menu->domenu ();
		*/

	if (p_function)
		return (*p_function)(this, p_object, p_data);
	else
		return 1;
}

INLINE void
GR_PushButton::set_data (void *data)
{
	p_data = data;
}

#endif
