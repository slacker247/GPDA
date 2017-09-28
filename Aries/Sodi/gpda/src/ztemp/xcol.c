#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "forms.h"

#include "xmisc.h"
#include "ixcol.h"
#include "xcol.h"

/*----------------------------------------------------------------------*/

#define SLIDER_WIDTH 21

/*----------------------------------------------------------------------*/

static void column_cb(FL_OBJECT *obj, long arg);
static FL_Coord get_fl_width(int bstyle, int bsize, char *label, int tstyle, int tsize, int nchars);
static int compare_column(const void *e1, const void *e2);

/*----------------------------------------------------------------------*/

int xfl_add_columns(XFL_COLUMN_CONTROL *ctrl)
{
	static char func[] = "xfl_add_columns";

	XFL_COLUMN *c;
	FL_OBJECT *obj;
	int i;
	FL_Coord button_x, button_y, button_w;
	FL_Coord frame_x, frame_y, frame_w, frame_h;
	FL_Coord max_width;
	int frame_bw = 8;

	if (ctrl->ordered_cols == NULL)
	{
		/* This is the first time xfl_add_columns
		** has been called with this 'ctrl'.
		*/

		/* Get space for the list of pointers pointers to
		** columns that we can sort in left-to-right 'order'.
		*/

		if ((ctrl->ordered_cols =
			calloc(ctrl->n, sizeof(XFL_COLUMN*))) == NULL)
		{
			fprintf(stderr, "%s: Can't alloc ordered_cols\n", func);
			return(ERROR);
		}

		/* Get space for private data on each column */

		if ((ctrl->sys =
			calloc(ctrl->n, sizeof(XFL_SYSCOL))) == NULL)
		{
			fprintf(stderr, "%s: Can't alloc private data\n", func);
			return(ERROR);
		}

		for (c = ctrl->cols, i = 0 ; i < ctrl->n ; c++, i++)
		{
			/* Populate the ordered_cols list */

			ctrl->ordered_cols[i] = c;

			/* Point to the private data for this column */

			c->sys = &ctrl->sys[i];

			/* Also see how much space each column needs */

			c->sys->width = get_fl_width(
					ctrl->bstyle, ctrl->bsize, c->label,
					ctrl->tstyle, ctrl->tsize, c->nchars);

			/* If not set by the user,
			** move up and out of the way.
			*/

			if (c->order == 0)
			{
				c->order = ctrl->n + i;
			}
		}
	}

	/* Place the column's buttons on top of the neighboring object,
	** which is probably a browser.  The width of this object determines
	** the number of columns that can appear.
	*/

	fl_addto_form(ctrl->neighbor->form);

	/* Where do the buttons go? */

	fl_set_object_position(ctrl->neighbor, 7, ctrl->neighbor->y);

	/* left align with neighbor (almost) */
	button_x = ctrl->neighbor->x + 1;

	/* on top on neighbor */
	button_y = ctrl->neighbor->y - ctrl->bheight;

	/* Where does the frame go? */

	frame_x = button_x - frame_bw + 2;
	frame_y = button_y - frame_bw + 2;
	frame_w = ctrl->neighbor->w + frame_bw + 2;
	frame_h = ctrl->neighbor->h + ctrl->bheight + frame_bw + 3;

  	obj = fl_add_frame(FL_EMBOSSED_FRAME,
		frame_x, frame_y, frame_w, frame_h, "");

	fl_set_object_bw(obj, frame_bw);

	/* Sort the columns in the desired sequence */

	if (ctrl->n > 1)
		qsort(ctrl->ordered_cols, ctrl->n,
			sizeof(ctrl->ordered_cols[0]), compare_column);

	/* Determine the last column to fit completely in display */

	button_w = 0;
	max_width = (ctrl->neighbor->w - button_x) +
			(frame_bw - 2) - SLIDER_WIDTH;

	for (i = 0 ; i < ctrl->n ; i++)
	{
		c = ctrl->ordered_cols[i];

		button_w += c->sys->width;

		if (button_w > max_width)
			break;
	}

	if (i == 0)
	{
		fprintf(stderr, "No buttons in view\n");
		abort();
	}

	ctrl->n_in_view = i;

	/* Create the buttons */

	for (i = 0 ; i < ctrl->n_in_view ; i++)
	{
		c = ctrl->ordered_cols[i];

		if (i == ctrl->n_in_view - 1)
		{
			/* This is the last column; take all remaining space */

			button_w = max_width + SLIDER_WIDTH;
		}
		else
		{
			button_w = c->sys->width;
		}

		obj = fl_add_button(ctrl->btype, button_x, button_y,
			button_w, ctrl->bheight, c->label);

		fl_set_object_boxtype(obj, FL_UP_BOX);
		fl_set_object_bw(obj, -1);
		fl_set_object_callback(obj, column_cb, i);
		fl_set_object_resize(obj, FL_RESIZE_X);
		obj->u_vdata = ctrl;

		c->sys->column_button = obj;

		button_x += button_w;
		max_width -= button_w;
	}

	fl_end_form();
	return(OK);
}

/*----------------------------------------------------------------------*/

void xfl_order_column(XFL_COLUMN_CONTROL *ctrl, XFL_COLUMN *col, int order)
{
	int i;
	XFL_COLUMN *c;

	for (c = ctrl->cols, i = 0 ; i < ctrl->n ; c++, i++)
	{
		if (c->order >= order)
		{
			c->order++;
		}
	}

	col->order = order;
}

/*----------------------------------------------------------------------*/
/* Called whenever a column's button is pressed, this adjusts the
** sort key and sort order before triggering a user object.
** The user object is expected to re-sort and re-display its data.
**
** The "args" parameter is the index into ordered_cols[]
** of the selected column.
*/

static void column_cb(FL_OBJECT *obj, long arg)
{
	XFL_COLUMN_CONTROL *ctrl;
	XFL_COLUMN *key;

	ctrl = obj->u_vdata;
	key = ctrl->ordered_cols[arg];

	if (ctrl->sort_key != key)
	{
		ctrl->sort_key = key;
		ctrl->sort_order = SORT_ASCENDING;
	}
	else if (ctrl->sort_order == SORT_ASCENDING)
	{
		ctrl->sort_order = SORT_DESCENDING;
	}
	else
	{
		ctrl->sort_order = SORT_ASCENDING;
	}

	fl_trigger_object(ctrl->trigger);
}

/*----------------------------------------------------------------------*/

static FL_Coord get_fl_width(
int bstyle,
int bsize,
char *label,
int tstyle,
int tsize,
int nchars)
{
	static char string[] =	"----+----1----+----2----+----3----+----4"
				"----+----5----+----6----+----7----+----8";

	FL_Coord text_w =
		fl_get_string_width(tstyle, tsize, string, nchars + 1);

	FL_Coord label_w =
		fl_get_string_width(bstyle, bsize, label, strlen(label));

	return MAX(text_w, label_w);
}

/*----------------------------------------------------------------------*/

static int compare_column(const void *e1, const void *e2)
{
	const XFL_COLUMN *c1 = *((XFL_COLUMN *const *)e1);
	const XFL_COLUMN *c2 = *((XFL_COLUMN *const *)e2);

	return(c1->order - c2->order);
}
