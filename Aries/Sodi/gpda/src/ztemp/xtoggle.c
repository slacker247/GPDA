#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "forms.h"

#include "xmisc.h"
#include "xmenus.h"
#include "xtoggle.h"
#include "check.xbm"
#include "blank.xbm"

/*----------------------------------------------------------------------*/

typedef enum { TF_TOGGLE = 1, TF_CLOSE } XFL_TOGGLE_MODE;

/*----------------------------------------------------------------------*/

static void toggle_form2(XFL_TOGGLE_FORM *tf, XFL_TOGGLE_MODE mode);

/*----------------------------------------------------------------------*/

void xfl_toggle_form(XFL_TOGGLE_FORM *tf)
{
	toggle_form2(tf, TF_TOGGLE);
}

/*----------------------------------------------------------------------*/

void xfl_close_toggle_form(XFL_TOGGLE_FORM *tf)
{
	toggle_form2(tf, TF_CLOSE);
}

/*----------------------------------------------------------------------*/

static void toggle_form2(XFL_TOGGLE_FORM *tf, XFL_TOGGLE_MODE mode)
{
	if (mode == TF_CLOSE || fl_form_is_visible(tf->form))
	{
		if (fl_form_is_visible(tf->form))
			fl_hide_form(tf->form);

		if (tf->button)
			fl_set_object_label(tf->button, tf->show_button_label);

		if (tf->menu_item)
		{
			SUBMENU_CLASS_DATA *cd = tf->menu_item->obj->c_vdata;
			cd->item->val = 0;
			fl_set_bitmap_data(cd->left,
				blank_xbm_width, blank_xbm_height,
				blank_xbm_bits);
		}
	}
	else
	{
		fl_show_form(tf->form, FL_PLACE_FREE, FL_TRANSIENT, tf->title);

		if (tf->button)
			fl_set_object_label(tf->button, tf->hide_button_label);

		if (tf->menu_item)
		{
			SUBMENU_CLASS_DATA *cd = tf->menu_item->obj->c_vdata;
			cd->item->val = 1;
			fl_set_bitmap_data(cd->left,
				check_xbm_width, check_xbm_height,
				check_xbm_bits);
		}
	}
}
