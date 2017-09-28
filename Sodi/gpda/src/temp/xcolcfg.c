#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <time.h>

#include "forms.h"

#include "xmisc.h"
#include "ixcol.h"
#include "xcol.h"

/*----------------------------------------------------------------------*/

static FL_FORM *Config_form;
static XFL_COLUMN *Source_col;
static FL_OBJECT *Source_button;
static FL_OBJECT *Config_remove_button;

/*----------------------------------------------------------------------*/

static FL_FORM *create_config_form(XFL_COLUMN_CONTROL *ctrl);
static void config_remove_button_cb(FL_OBJECT *obj, long arg);
static void config_save_button_cb(FL_OBJECT *obj, long arg);
static void config_close_button_cb(FL_OBJECT *obj, long arg);
static void hijack_column_buttons(XFL_COLUMN_CONTROL *ctrl);
static void restore_column_buttons(XFL_COLUMN_CONTROL *ctrl);
static void pool_button_cb(FL_OBJECT *obj, long arg);
static void column_button_cb(FL_OBJECT *obj, long arg);
static void rebuild_columns(XFL_COLUMN_CONTROL *ctrl, int order);
static void remove_all_columns(XFL_COLUMN_CONTROL *ctrl);
static void update_config_buttons(XFL_COLUMN_CONTROL *ctrl);
static void change_cursor(void);
static void restore_cursor(void);

/*----------------------------------------------------------------------*/

void xfl_config_columns(XFL_COLUMN_CONTROL *ctrl)
{
	if (Config_form == NULL)
	{
		Config_form = create_config_form(ctrl);
	}

	Source_col = NULL;
	update_config_buttons(ctrl);
	hijack_column_buttons(ctrl);

	if (!fl_form_is_visible(Config_form))
	{
		fl_show_form(Config_form, FL_PLACE_CENTERFREE,
			FL_TRANSIENT, "config");
	}
}

/*----------------------------------------------------------------------*/

#define XBIAS 20
#define YBIAS 20

#define XINCR (max_w + 10)
#define YINCR (ctrl->bheight + 5)

static FL_FORM *create_config_form(XFL_COLUMN_CONTROL *ctrl)
{
	FL_FORM *form;
	FL_OBJECT *obj;
	XFL_COLUMN *c;
	FL_Coord form_w = 700;
	FL_Coord form_h = 200;
	FL_Coord x = 0;
	FL_Coord y = 0;
	FL_Coord w;
	FL_Coord max_w = 0;
	int i;

	form = fl_bgn_form(FL_NO_BOX, form_w, form_h);
	obj = fl_add_box(FL_FRAME_BOX, 0, 0, form_w, form_h, "");

	/* Lay out the buttons from top to bottom, left to right */

	fl_bgn_group();

	for (c = ctrl->cols, i = 0 ; i < ctrl->n ; c++, i++)
	{
		w = c->sys->width;

		if (w > max_w)
		{
			max_w = w;
		}

		obj = fl_add_button(FL_RADIO_BUTTON, x + XBIAS, y + YBIAS,
			w, ctrl->bheight, c->label);

		fl_set_object_boxtype(obj, FL_UP_BOX);
		fl_set_object_bw(obj, -1);
		fl_set_object_callback(obj, pool_button_cb, i);
		fl_set_object_resize(obj, FL_RESIZE_NONE);
		obj->u_vdata = ctrl;

		c->sys->config_button = obj;

		y += YINCR;

		if (y + YINCR >= form_h)
		{
			/* Move to top of next column */
			x += XINCR;
			y = 0;
			max_w = 0;
		}
	}

	fl_end_group();

	obj = fl_add_button(FL_NORMAL_BUTTON, 600, 150, 70, 20, "Remove");
	Config_remove_button = obj;
	fl_set_object_lsize(obj, FL_NORMAL_SIZE);
	fl_set_object_callback(obj, config_remove_button_cb, 0);
	xfl_deactivate_button(obj);

	obj = fl_add_button(FL_NORMAL_BUTTON, 600, 125, 70, 20, "Save");
	fl_set_object_lsize(obj, FL_NORMAL_SIZE);
	fl_set_object_callback(obj, config_save_button_cb, 0);

	obj = fl_add_button(FL_NORMAL_BUTTON, 600, 175, 70, 20, "Close");
	fl_set_object_lsize(obj, FL_NORMAL_SIZE);
	fl_set_object_callback(obj, config_close_button_cb, 0);

	fl_end_form();

	return(form);
}

/*----------------------------------------------------------------------*/

static void config_remove_button_cb(FL_OBJECT *obj, long arg)
{
	XFL_COLUMN_CONTROL *ctrl = Source_button->u_vdata;
	XFL_COLUMN *c = ctrl->ordered_cols[ctrl->n - 1];

	rebuild_columns(ctrl, c->order + 1);
}

/*----------------------------------------------------------------------*/

static void config_save_button_cb(FL_OBJECT *obj, long arg)
{
}

/*----------------------------------------------------------------------*/

static void config_close_button_cb(FL_OBJECT *obj, long arg)
{
	XFL_COLUMN_CONTROL *ctrl = NULL;
	FL_OBJECT *o;

	if (Source_col)
		fl_set_button(Source_button, 0);

	/* Search the form for a radio button; every radio button's
	** u_vdata points to the control structure.
	*/

	for (o = obj->form->first ; o ; o = o->next)
	{
		if (o->type == FL_RADIO_BUTTON)
		{
			ctrl = o->u_vdata;
			break;
		}
	}

	if (ctrl == NULL)
	{
		fprintf(stderr, "Can't find ctrl\n");
		abort();
	}

	fl_hide_form(obj->form);
	restore_column_buttons(ctrl);
}

/*----------------------------------------------------------------------*/

static void hijack_column_buttons(XFL_COLUMN_CONTROL *ctrl)
{
	int i;

	for (i = 0 ; i < ctrl->n_in_view ; i++)
	{
		XFL_COLUMN *c = ctrl->ordered_cols[i];
		FL_OBJECT *obj = c->sys->column_button;

		obj->c_vdata = obj->object_callback;

		fl_set_object_callback(obj, column_button_cb, i);

		obj->type = FL_RADIO_BUTTON;
	}
}

/*----------------------------------------------------------------------*/

static void restore_column_buttons(XFL_COLUMN_CONTROL *ctrl)
{
	int i;

	for (i = 0 ; i < ctrl->n_in_view ; i++)
	{
		XFL_COLUMN *c = ctrl->ordered_cols[i];
		FL_OBJECT *obj = c->sys->column_button;
		FL_CALLBACKPTR cb = obj->c_vdata;

		fl_set_object_callback(obj, cb, i);

		obj->type = ctrl->btype;
	}
}

/*----------------------------------------------------------------------*/
/* Called when a button in the "pool" is pressed.
*/

static void pool_button_cb(FL_OBJECT *obj, long arg)
{
	XFL_COLUMN_CONTROL *ctrl = obj->u_vdata;

	/* Selecting a pool button as the source */

	if (Source_col)
		fl_set_button(Source_button, 0);
	Source_col = &ctrl->cols[arg];
	change_cursor();
	Source_button = obj;
	xfl_deactivate_button(Config_remove_button);
}

/*----------------------------------------------------------------------*/
/* Called when an active column's button is pressed.
*/

static void column_button_cb(FL_OBJECT *obj, long arg)
{
	XFL_COLUMN_CONTROL *ctrl = obj->u_vdata;
	XFL_COLUMN *dst_col;
	int order;

	if (Source_button == NULL)
	{
		/* Selecting a column button as the source */

		Source_button = obj;
		Source_col = ctrl->ordered_cols[arg];
		change_cursor();
		fl_set_button(Source_button, 1);
		xfl_activate_button(Config_remove_button);
		return;
	}
	else if (Source_button == obj)
	{
		/* Selected the same column as both source and destination */

		fl_set_button(Source_button, 0);
		xfl_deactivate_button(Config_remove_button);
		Source_button = NULL;
		Source_col = NULL;
		restore_cursor();
		return;
	}

	dst_col = ctrl->ordered_cols[arg];

	/* Remember where the column is to go */

	order = dst_col->order;

	/* Bump all columns from the dest on down */

	for ( ; arg < ctrl->n ; arg++)
	{
		ctrl->ordered_cols[arg]->order++;
	}

	rebuild_columns(ctrl, order);
}

/*----------------------------------------------------------------------*/

static void rebuild_columns(XFL_COLUMN_CONTROL *ctrl, int order)
{
	Source_col->order = order;
	remove_all_columns(ctrl);
	xfl_add_columns(ctrl);
	hijack_column_buttons(ctrl);
	fl_trigger_object(ctrl->trigger);
	fl_set_button(Source_button, 0);
	xfl_deactivate_button(Config_remove_button);
	Source_button = NULL;
	Source_col = NULL;
	update_config_buttons(ctrl);
}

/*----------------------------------------------------------------------*/

static void remove_all_columns(XFL_COLUMN_CONTROL *ctrl)
{
	int i;

	fl_addto_form(ctrl->neighbor->form);

	for (i = 0 ; i < ctrl->n_in_view ; i++)
	{
		XFL_COLUMN *c = ctrl->ordered_cols[i];

		if (c->sys->column_button != NULL)
		{
			fl_delete_object(c->sys->column_button);
			c->sys->column_button = NULL;
		}
	}

	fl_end_form();
}

/*----------------------------------------------------------------------*/

static void update_config_buttons(XFL_COLUMN_CONTROL *ctrl)
{
	XFL_COLUMN *c;
	int i;

	for (c = ctrl->cols, i = 0 ; i < ctrl->n ; c++, i++)
	{
		if (c->sys->column_button == NULL)
		{
			/* If the column is not in display,
			** activate the button in the pool.
			*/

			xfl_activate_button(c->sys->config_button);
		}
		else
		{
			/* If the column is in display,
			** deactivate the button in the pool.
			*/

			xfl_deactivate_button(c->sys->config_button);
		}
	}
}

/*----------------------------------------------------------------------*/

static void change_cursor(void)
{
	fl_set_cursor(Config_form->window, XC_sb_left_arrow);
}

/*----------------------------------------------------------------------*/

static void restore_cursor(void)
{
	fl_set_cursor(Config_form->window, XC_X_cursor);
}
