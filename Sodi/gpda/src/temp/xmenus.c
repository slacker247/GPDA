#include <ctype.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "forms.h"

#include "xmisc.h"
#include "xmenus.h"
#include "rightri.xbm"

/*----------------------------------------------------------------------*/
/* The minimum amount of space that must appear between
** a label and a shortcut's label.
*/
#define MINIMUM_GUTTER 24

/* The width of the left.
*/
#define BITMAP_WIDTH	16
#define MAIN_MENU_PADDING 10
#define SUBMENU_PADDING 10

/*----------------------------------------------------------------------*/

#define FL_MOD_MASK (FL_ALT_MASK | FL_CONTROL_MASK | FL_SHIFT_MASK)

#define PREV(item)	((item)->prev ? (item)->prev : (item)->parent->last)
#define NEXT(item)	((item)->next ? (item)->next : (item)->parent->first)

/*----------------------------------------------------------------------*/

char XFL_separator[] = "@DnLine";

/*----------------------------------------------------------------------*/

static XFL_MENU_CTRL *Mctrl = NULL;
static int XFL_menu_fontsize = FL_NORMAL_SIZE;
static int XFL_menu_fontstyle = FL_NORMAL_STYLE;

/*----------------------------------------------------------------------*/

static FL_OBJECT *xfl_add_pulldown_menu( XFL_MENU *menu, FL_Coord x, FL_Coord y, FL_Coord h);
static FL_Coord xfl_menu_item_width(int type, XFL_MENU *m);
static FL_OBJECT *xfl_add_main_menu_button( int type, FL_Coord x, FL_Coord y, FL_Coord w, FL_Coord h, const char *label);
static FL_OBJECT *xfl_create_main_menu_button( int type, FL_Coord x, FL_Coord y, FL_Coord w, FL_Coord h, const char *label);
static FL_OBJECT *xfl_add_submenu_button( int type, FL_Coord x, FL_Coord y, FL_Coord w, FL_Coord h, const char *label, const char *shortcut_label);
static FL_OBJECT *xfl_create_submenu_button( int type, FL_Coord x, FL_Coord y, FL_Coord w, FL_Coord h, const char *label, const char *shortcut_label);
static int xfl_parse_label(XFL_MENU *item, int type);
static KeySym xfl_keysym_toupper(KeySym keysym);
static int xfl_main_menu_event( FL_OBJECT *o, int event, FL_Coord mx, FL_Coord my, int key, void *xev);
static int xfl_submenu_event( FL_OBJECT *o, int event, FL_Coord mx, FL_Coord my, int key, void *xev);
static int xfl_raw_key_press(FL_FORM *form, void *xevent);
static void xfl_sync(XFL_MENU *tree, XFL_MENU *menu, XFL_MENU *item);
static void xfl_trigger_menu_item(XFL_MENU *item);
static void check_mouse_hover(void);
static void xfl_mouse_on_the_loose(void);
static void xfl_mouse_in_the_house(void);
static int xfl_mousetrap(FL_FORM *form, void *xevent);
static void xfl_submenu_expose_timer_cb(FL_OBJECT *o, long arg);
static void xfl_show_submenu(XFL_MENU *menu);
static void xfl_hide_submenu(XFL_MENU *menu);
static void xfl_select_menu_item(XFL_MENU *mi);
static void xfl_deselect_menu_item(XFL_MENU *mi);
static XFL_MENU *xfl_hide_tree(XFL_MENU *active, XFL_MENU *new);
static FL_Coord xfl_string_width(const char *s);

/*----------------------------------------------------------------------*/
/* Public routines							*/
/*----------------------------------------------------------------------*/

/*----------------------------------------------------------------------*/
/* Start the creation of a menu.  Place the first (leftmost) item
** of the main menu on the current form at the given coordinates.
** Use 'h' as the height for all menu items.
** Returns a pointer to the main menu.
*/

XFL_MENU *xfl_bgn_menu(FL_Coord x, FL_Coord y, FL_Coord h)
{
	static char func[] = "xfl_bgn_menu";

	XFL_MENU_CTRL *mc;
	char *p;

	/* Check environment */

	if ((p = getenv("XFL_MENU_FONTSIZE")) != NULL)
	{
		XFL_menu_fontsize = atoi(p);
	}

	if ((p = getenv("XFL_MENU_FONTSTYLE")) != NULL)
	{
		XFL_menu_fontstyle = atoi(p);
	}

	/* Get a control structure */

	if ((mc = calloc(1, sizeof(*mc))) == NULL)
	{
		fprintf(stderr, "%s: Can't alloc main menu\n", func);
		return(NULL);
	}

	/* Remember the form it'll be on */

	mc->form = fl_current_form;

	/* Create the main menu.  This menu does not appear anywhere,
	** however, its items do.
	*/

	mc->menu = xfl_add_menu(NULL, NULL);

	/* No active menu or item yet */

	mc->active_tree = NULL;
	mc->active_menu = NULL;
	mc->active_item = NULL;
	mc->pushed = 0;

	/* Tie the main menu back to the control */

	mc->menu->mc = mc;

	/* Save these parameters;
	** they won't be used until xfl_end_form().
	*/

	mc->x = x;
	mc->y = y;
	mc->h = h;

	return(mc->menu);
}

/*----------------------------------------------------------------------*/

XFL_MENU *xfl_add_menu(XFL_MENU *parent, const char *label)
{
	XFL_MENU *menu = xfl_add_menu_item(parent, label, NULL, 0, 0, NULL);
	return(menu);
}

/*----------------------------------------------------------------------*/
/* Add an item to a menu.
**	parent	Add item to this menu.
**	label	Text string.
**	cb	Callback function.
**	arg	Parameter to pass to callback function.
*/

XFL_MENU *xfl_add_menu_item(
XFL_MENU *parent,
const char *label,
FL_CALLBACKPTR cb,
long arg,
KeySym shortcut,
const char *status)
{
	static char func[] = "xfl_add_menu_item";

	XFL_MENU *item;

	/* Get space for the item */

	if ((item = calloc(1, sizeof(*item))) == NULL)
	{
		fprintf(stderr, "%s: Can't alloc menu item\n", func);
		return(NULL);
	}

	/* Unless we're the main menu, point our predecessors to us */

	if (parent)
	{
		item->mc = parent->mc;

		if (parent->first == NULL)
		{
			/* We are first item on the menu */

			parent->first = item;
		}
		else
		{
			/* We are subsequent items on the menu */

			parent->last->next = item;
			item->prev = parent->last;
		}

		parent->last = item;
	}

	/* Fill it in */

	item->parent = parent;
	item->label = label;
	item->cb = cb;
	item->arg = arg;
	item->status = status;
	if (shortcut)
		xfl_set_menu_item_shortcut(item, shortcut);

	return(item);
}

/*----------------------------------------------------------------------*/

XFL_MENU *xfl_add_separator(XFL_MENU *parent)
{
	XFL_MENU *item = xfl_add_menu_item(parent, XFL_SEPARATOR, NULL, 0, 0, NULL);
	return(item);
}

/*----------------------------------------------------------------------*/

int xfl_set_menu_item_shortcut(XFL_MENU *item, KeySym keysym)
{
	static char func[] = "xfl_set_menu_item_shortcut";

	char buf [BUFSIZ], *s = buf;

	if (item->mc->n_shortcuts == XFL_MAX_SHORTCUTS)
	{
		fprintf(stderr, "%s: Too many shortcuts: (%d max)\n",
			func, XFL_MAX_SHORTCUTS);
		return(ERROR);
	}

	item->shortcut = xfl_keysym_toupper(keysym);
	item->mc->shortcut[item->mc->n_shortcuts].keysym = item->shortcut;
	item->mc->shortcut[item->mc->n_shortcuts].item = item;
	item->mc->n_shortcuts++;

	/* Build the shortcut's label */

	s = buf;
	if (item->shortcut & FL_ALT_MASK)
	{
		strcpy(s, "Alt+");
		s += 4;
	}
	if (item->shortcut & FL_CONTROL_MASK)
	{
		strcpy(s, "Ctrl+");
		s += 5;
	}
	if (item->shortcut & FL_SHIFT_MASK)
	{
		strcpy(s, "Shift+");
		s += 6;
	}

	strcpy(s, XKeysymToString(item->shortcut & ~FL_MOD_MASK));

	if ((item->shortcut_label = strdup(buf)) == NULL)
	{
		fprintf(stderr, "%s: no space for shortcut_label\n", func);
		return(ERROR);
	}

	return(OK);
}

/*----------------------------------------------------------------------*/

int xfl_set_menu_item_status(XFL_MENU *item, const char *status)
{
	item->status = status;
	return(OK);
}

/*----------------------------------------------------------------------*/

void xfl_end_menu(XFL_MENU *main_menu)
{
	XFL_MENU *menu;
	FL_Coord x, y, w, h;

	Mctrl = main_menu->mc;
	x = Mctrl->x;
	y = Mctrl->y;
	h = Mctrl->h;

	/* Add buttons for the horizontal menu */

	for (menu = main_menu->first ; menu ; menu = menu->next)
	{
		if (XFL_IS_SEPARATOR(menu))
			continue;

		xfl_parse_label(menu, FL_MAIN_MENU_BUTTON);

		w = xfl_menu_item_width(0, menu);

		menu->obj= xfl_add_main_menu_button(0, x, y, w, h, menu->label);

		/* Link the object back to the menu */

		menu->obj->c_vdata = menu;

		/* If this is a submenu... */

		if (XFL_HAS_SUBMENU(menu))
		{
			menu->subframe = xfl_add_pulldown_menu(
				menu->first,
				/* Start the pulldown menu a little to
				** the right of the left edge of the button.
				*/
				x + 2,
				/* Start the pulldown menu a little below
				** the bottom edge of the button.
				*/
				y + h + 2,
				h);
		}

		x += w;
	}

	/* Add the status frame */

	Mctrl->status = fl_add_box(FL_FRAME_BOX,
		0, Mctrl->form->h - 25, Mctrl->form->w, 25, "");

	fl_set_object_align(Mctrl->status, FL_ALIGN_LEFT | FL_ALIGN_INSIDE);
	fl_set_object_lsize(Mctrl->status, XFL_menu_fontsize);
	fl_set_object_lstyle(Mctrl->status, XFL_menu_fontstyle);
	fl_hide_object(Mctrl->status);

	/* We can't use fl_set_object_shortcut because Xform's shortcuts
	** only trigger the object if the object is visible and menu items
	** are hidden most of time.  Therefore, we need to see all keystrokes
	** at all times we are in the form.
	*/

	Mctrl->raw_key_press = fl_register_raw_callback(
		main_menu->mc->form,
		KeyPressMask | KeyReleaseMask,
		xfl_raw_key_press);

	/* */

	Mctrl->timer = fl_add_timer(FL_HIDDEN_TIMER, 0, 0, 0, 0, NULL);
	fl_set_object_callback(Mctrl->timer, xfl_submenu_expose_timer_cb, 0);
}

/*----------------------------------------------------------------------*/

int xfl_set_menu_fontsize(int size)
{
	int old = XFL_menu_fontsize;
	XFL_menu_fontsize = size;
	return old;
}

/*----------------------------------------------------------------------*/

int xfl_set_menu_fontstyle(int style)
{
	int old = XFL_menu_fontstyle;
	XFL_menu_fontstyle = style;
	return old;
}

/*----------------------------------------------------------------------*/
/* Private routines							*/
/*----------------------------------------------------------------------*/

static FL_OBJECT *xfl_add_pulldown_menu(
XFL_MENU *menu,
FL_Coord x,
FL_Coord y,
FL_Coord h)
{
	FL_OBJECT *subframe;
	XFL_MENU *m;
	FL_Coord width, height;
	FL_Coord w, y2;

	/* Determine the width and height of the pulldown */

	width = height = 0;

	for (m = menu ; m ; m = m->next)
	{
		if (XFL_IS_SEPARATOR(m))
		{
			height += h / 2;
			continue;
		}

		xfl_parse_label(m, FL_SUBMENU_BUTTON);

		w = xfl_menu_item_width(XFL_HAS_SUBMENU(m)
			? FL_SUBMENU_BRANCH : FL_SUBMENU_LEAF,
			m);

		if (w > width)
			width = w;

		height += h;
	}

	/* Surround the pulldown with a frame */

	subframe = fl_add_box(FL_EMBOSSED_BOX,
			x - 1, y - 1, width + 2, height + 2, "");
	fl_hide_object(subframe);

	/* Add buttons for the pulldown */

	for (y2 = y, m = menu ; m ; m = m->next)
	{
		SUBMENU_CLASS_DATA *cd;

		if (XFL_IS_SEPARATOR(m))
		{
			m->obj = fl_add_text(FL_NORMAL_TEXT,
				x, y2, width, h / 2, XFL_SEPARATOR);

			fl_hide_object(m->obj);
			y2 += h / 2;
			continue;
		}

		m->obj = xfl_add_submenu_button(
			XFL_HAS_SUBMENU(m)? FL_SUBMENU_BRANCH : FL_SUBMENU_LEAF,
			x, y2, width, h, m->label, m->shortcut_label);

		if (m->cb)
			fl_set_object_callback(m->obj, m->cb, m->arg);

		/* Link the object back to the menu */

		if ((cd = m->obj->c_vdata) != NULL)
			cd->item = m;

		/* If this item is another menu... */

		if (XFL_HAS_SUBMENU(m))
		{
			m->subframe = xfl_add_pulldown_menu(
				m->first,
				/* Start the pulldown menu a little to
				** the left of the right edge of the frame.
				*/
				x + width - 2,
				/* Start the pulldown menu a little
				** above the top edge of the button.
				*/
				y2 - 2,
				h);
		}

		y2 += h;
	}

	return(subframe);
}

/*----------------------------------------------------------------------*/

static FL_Coord xfl_menu_item_width(int type, XFL_MENU *m)
{
	FL_Coord w;

	if (XFL_IS_TOP_ITEM(m))
	{
		w = xfl_string_width(m->label) + MAIN_MENU_PADDING;
	}
	else
	{
		/* Left */

		w = BITMAP_WIDTH;

		/* Center */

		w += xfl_string_width(m->label);
		w += SUBMENU_PADDING;

		/* Right (optional) */

		if (type == FL_SUBMENU_BRANCH)
		{
			w += BITMAP_WIDTH;
		}
		else if (m->shortcut_label)
		{
			w += xfl_string_width(m->shortcut_label);
			w += MINIMUM_GUTTER;
		}
	}

	return(w);
}

/*----------------------------------------------------------------------*/

static FL_OBJECT *xfl_add_main_menu_button(
int type,
FL_Coord x,
FL_Coord y,
FL_Coord w,
FL_Coord h,
const char *label)
{
	FL_OBJECT *o = xfl_create_main_menu_button(type, x, y, w, h, label);
	fl_add_object(fl_current_form, o);
	return o;
}

/*----------------------------------------------------------------------*/

static FL_OBJECT *xfl_create_main_menu_button(
int type,
FL_Coord x,
FL_Coord y,
FL_Coord w,
FL_Coord h,
const char *label)
{
	FL_OBJECT *o = fl_make_object(FL_MAIN_MENU_BUTTON,
			type, x, y, w, h, label, xfl_main_menu_event);

	fl_set_object_boxtype(o, FL_FLAT_BOX);
	fl_set_object_align(o, FL_ALIGN_CENTER | FL_ALIGN_INSIDE);
	fl_set_object_lsize(o, XFL_menu_fontsize);
	fl_set_object_lstyle(o, XFL_menu_fontstyle);
	fl_set_object_bw(o, -1);

	return(o);
}

/*----------------------------------------------------------------------*/

static FL_OBJECT *xfl_add_submenu_button(
int type,
FL_Coord x,
FL_Coord y,
FL_Coord w,
FL_Coord h,
const char *label,
const char *shortcut_label)
{
	FL_OBJECT *o;
	SUBMENU_CLASS_DATA *cd;

	o = xfl_create_submenu_button(type, x, y, w, h, label, shortcut_label);

	if ((cd = o->c_vdata) != NULL)
	{
		if (cd->left != NULL)
		{
			fl_add_object(fl_current_form, cd->left);
			fl_hide_object(cd->left);
		}

		if (cd->center != NULL)
		{
			fl_add_object(fl_current_form, cd->center);
			fl_hide_object(cd->center);
		}

		if (cd->right != NULL)
		{
			fl_add_object(fl_current_form, cd->right);
			fl_hide_object(cd->right);
		}
	}

	fl_add_object(fl_current_form, o);
	fl_hide_object(o);

	return(o);
}

/*----------------------------------------------------------------------*/

static FL_OBJECT *xfl_create_submenu_button(
int type,
FL_Coord x,
FL_Coord y,
FL_Coord w,
FL_Coord h,
const char *label,
const char *shortcut_label)
{
	static char func[] = "xfl_create_submenu_button";

	SUBMENU_CLASS_DATA *cd;
	FL_Coord leftw, centerw, rightw, gutter;
	FL_OBJECT *obj;

	/* Get space for the class data */

	if ((cd = calloc(1, sizeof(*cd))) == NULL)
	{
		fprintf(stderr, "%s: No space\n", func);
		return(NULL);
	}

	leftw = BITMAP_WIDTH;
	w -= leftw;

	if (type == FL_SUBMENU_BRANCH)
	{
		/* This item is the parent of yet another menu */

		rightw = BITMAP_WIDTH;
		centerw = w - rightw;
	}
	else if (shortcut_label == NULL)
	{
		/* If there's no shortcut, the label consumes
		** all remaining space.
		*/

		centerw = w;
	}
	else
	{
		/* If there is a shortcut, we need the optional right.
		** It is a separate object from the label so we can left
		** align the label and right-align the shortcut.
		**
		** Evenly divide the gutter between the two objects.
		*/

		centerw = xfl_string_width(label);
		rightw = xfl_string_width(shortcut_label);
		gutter = w - (centerw + rightw);
		gutter /= 2;

		centerw += gutter;
		rightw = w - centerw;
	}

	/* The bitmap goes on the left */

	cd->left = fl_create_bitmap(FL_NORMAL_BITMAP, x, y, leftw, h, "");
	fl_set_object_boxtype(cd->left, FL_FLAT_BOX);

	/* The label goes in the center */

	cd->center = fl_create_text(FL_NORMAL_TEXT,
		x + leftw, y, centerw, h, label);

	fl_set_object_boxtype(cd->center, FL_FLAT_BOX);
	fl_set_object_align(cd->center, FL_ALIGN_LEFT | FL_ALIGN_INSIDE);
	fl_set_object_lsize(cd->center, XFL_menu_fontsize);
	fl_set_object_lstyle(cd->center, XFL_menu_fontstyle);

	if (type == FL_SUBMENU_BRANCH)
	{
		/* The right-pointing triangle goes on the right */

		cd->right = fl_create_bitmap(FL_NORMAL_BITMAP,
			x + leftw + centerw, y, rightw, h, "");
		fl_set_object_boxtype(cd->right, FL_FLAT_BOX);

		fl_set_bitmap_data(cd->right,
			rightri_width, rightri_height, rightri_bits);
	}
	else if (shortcut_label)
	{
		/* The shortcut goes on the right */

		cd->right = fl_create_text(FL_NORMAL_TEXT,
			x + leftw + centerw, y, rightw, h, shortcut_label);

		fl_set_object_boxtype(cd->right, FL_FLAT_BOX);
		fl_set_object_align(cd->right, FL_ALIGN_RIGHT|FL_ALIGN_INSIDE);
		fl_set_object_lsize(cd->right, XFL_menu_fontsize);
		fl_set_object_lstyle(cd->right, XFL_menu_fontstyle);
	}

	/* Wrap them all under a hidden button */

	obj = fl_make_object(FL_BUTTON, FL_HIDDEN_BUTTON,
		x, y, w + leftw, h, label, xfl_submenu_event);

	obj->c_vdata = cd;

	return(obj);
}

/*----------------------------------------------------------------------*/
/* If the label contains a caret ('^'), the character following the caret
** is underlined in the label, and that character, x, is used to construct
** either the item's shortcut or hotkey, as follows:
**
** If the item is a main menu item, Alt-x becomes the item's shortcut.
**
** If the item is a submenu item, x becomes the item's hotkey.
**
** Shortcuts can be invoked at any time in any state.
** Hotkeys are only recognized when the item is in display on the active_menu.
*/

static int xfl_parse_label(XFL_MENU *item, int type)
{
	static char func[] = "xfl_parse_label";

	char buf [BUFSIZ], *s = buf;
	const char *p;

	if (item->label == XFL_SEPARATOR || strchr(item->label, '^') == NULL)
	{
		return(OK);
	}

	for (p = item->label ; *p ; p++)
	{
		if (*p == '^')
		{
			p++;

			if (*p == '\0')
			{
				/* Leave trailing '^' unmolested */
				p--;
			}
			else if (*p != '^')	/* '^' quotes '^' */
			{
				if (type == FL_MAIN_MENU_BUTTON)
				{
					KeySym k = FL_ALT_MASK | toupper(*p);
					xfl_set_menu_item_shortcut(item, k);
				}
				else
				{
					item->hotkey = toupper(*p);
				}

				*s++ = *p;

				/* Underline the preceding character */
				*s++ = 0x08;
				continue;
			}
		}

		*s++ = *p;
	}

	*s = '\0';

	if ((item->label = strdup(buf)) == NULL)
	{
		fprintf(stderr, "%s: no space for label\n", func);
		return(ERROR);
	}

	return(OK);
}

/*----------------------------------------------------------------------*/

static KeySym xfl_keysym_toupper(KeySym keysym)
{
	KeySym mods = keysym & FL_MOD_MASK;
	KeySym key = keysym & ~FL_MOD_MASK;

	if (isalpha(key))
	{
		/* Don't allow shift+alpha */
		if (mods == FL_SHIFT_MASK)
			mods &= ~FL_SHIFT_MASK;

		if (islower(key))
			key = toupper(key);
	}

	keysym = mods | key;

	return(keysym);
}

/*----------------------------------------------------------------------*/
/* Main Menu Item Event Handler						*/
/*----------------------------------------------------------------------*/

static int xfl_main_menu_event(
FL_OBJECT *o,
int event,
FL_Coord mx,
FL_Coord my,
int key,
void *xev)
{
#if TESTMAIN
	xfl_trace_event("main_menu", o, event, mx, my, key, xev);
#endif

	switch (event)
	{
	case FL_DRAW:
		fl_drw_box(o->boxtype, o->x, o->y, o->w, o->h, o->col1, o->bw);
		fl_draw_object_label(o);
		break;

	case FL_ENTER:
		xfl_mouse_in_the_house();
		if (Mctrl->pushed)
			xfl_sync(o->c_vdata, o->c_vdata, NULL);
		else
			xfl_sync(o->c_vdata, NULL, NULL);
		break;

	case FL_LEAVE:
		xfl_mouse_on_the_loose();
		break;

	case FL_PUSH:
		Mctrl->pushed ^= 1;
		if (Mctrl->pushed)
			xfl_sync(o->c_vdata, o->c_vdata, NULL);
		else
			xfl_sync(o->c_vdata, NULL, NULL);
		break;
	}

	return 0;
}

/*----------------------------------------------------------------------*/
/* Submenu Item Event Handler						*/
/*----------------------------------------------------------------------*/

static int xfl_submenu_event(
FL_OBJECT *o,
int event,
FL_Coord mx,
FL_Coord my,
int key,
void *xev)
{
	SUBMENU_CLASS_DATA *cd;

#if TESTMAIN
	xfl_trace_event("submenu", o, event, mx, my, key, xev);
#endif

	switch (event)
	{
	case FL_DRAW:
		cd = o->c_vdata;

		if ((o = cd->left) != NULL)
		{
			fl_drw_box(o->boxtype, o->x, o->y, o->w, o->h,
				o->col1, o->bw);
			fl_draw_object_label(o);
		}

		if ((o = cd->center) != NULL)
		{
			fl_drw_box(o->boxtype, o->x, o->y, o->w, o->h,
				o->col1, o->bw);
			fl_draw_object_label(o);
		}

		if ((o = cd->right) != NULL)
		{
			fl_drw_box(o->boxtype, o->x, o->y, o->w, o->h,
				o->col1, o->bw);
			fl_draw_object_label(o);
		}

		break;

	case FL_ENTER:
		xfl_mouse_in_the_house();
		cd = o->c_vdata;
		xfl_sync(Mctrl->active_tree, cd->item->parent, cd->item);
		break;

	case FL_LEAVE:
		xfl_mouse_on_the_loose();
		xfl_sync(Mctrl->active_tree, Mctrl->active_menu, NULL);
		break;

	case FL_PUSH:
		cd = o->c_vdata;
		xfl_trigger_menu_item(cd->item);
		break;
	}

	return 0;
}

/*----------------------------------------------------------------------*/
/* Keystroke Handler							*/
/*----------------------------------------------------------------------*/

static int xfl_raw_key_press(FL_FORM *form, void *xevent)
{
	static int alt_pressed = NO;

	XKeyEvent *key;
	KeySym keysym;
	XFL_MENU *tree, *menu, *item;
	int i;

	/* Give the application first crack at the event */

	if (Mctrl->raw_key_press)
	{
		int ret = (*Mctrl->raw_key_press)(form, xevent);

		if (ret != 0)
			return(ret);
	}

	/* Address the KeyEvent structure */

	key = &((XEvent*)xevent)->xkey;

	if (key->type == KeyRelease)
	{
		if (alt_pressed)
		{
			keysym = XKeycodeToKeysym(
				key->display, key->keycode, 0);

			if (keysym == XK_Alt_L || keysym == XK_Alt_R)
			{
				fprintf(stderr, "Alt key released\n");
				alt_pressed = NO;
			}
		}

		return 0;
	}

	/* key->type == KeyPress */

	/* Convert the keycode to a keysym */

	keysym = XKeycodeToKeysym(key->display, key->keycode, 0);

	/* All menu processing is case-insensitive */

	keysym = xfl_keysym_toupper(keysym);

#if TESTMAIN
	fprintf(stderr, "keycode %#x state %#x keysym %#lx %s\n",
		key->keycode, key->state, keysym, XKeysymToString(keysym));
#endif

	/* Fold in state bits */

	if (key->state & ShiftMask)	keysym |= FL_SHIFT_MASK;
	if (key->state & ControlMask)	keysym |= FL_CONTROL_MASK;
	if (key->state & Mod1Mask)	keysym |= FL_ALT_MASK;

	/* Look for shortcuts */

	if (!Mctrl->active_menu && !alt_pressed)
	{
		for (i = 0 ; i < Mctrl->n_shortcuts ; i++)
		{
			if (Mctrl->shortcut[i].keysym == keysym)
			{
				if (key->state == Mod1Mask &&
				    XFL_IS_TOP_ITEM(Mctrl->shortcut[i].item))
				{
					alt_pressed = YES;
				}

				xfl_trigger_menu_item(Mctrl->shortcut[i].item);
				return FL_PREEMPT;
			}
		}

		/* If it's not menu shortcut and no menu has been
		** activated, we don't care about keystrokes.
		*/
		return 0;
	}

	if (alt_pressed)
	{
		/* Allow this:
		**	Press Alt
		**	Press f
		**	Release f
		**	Press c
		**	Release c
		**	Release Alt
		*/

		keysym &= ~FL_ALT_MASK;
	}

	switch (keysym)
	{
	case XK_Alt_L:
	case XK_Alt_R:
		xfl_sync(NULL, NULL, NULL);
		return FL_PREEMPT;

	case XK_Right:
		if (Mctrl->active_item && XFL_HAS_SUBMENU(Mctrl->active_item))
		{
			tree = Mctrl->active_tree;
			menu = Mctrl->active_item;
			item = menu->first;
		}
		else
		{
			tree = NEXT(Mctrl->active_tree);
			menu = tree;
			item = menu->first;
		}

		xfl_sync(tree, menu, item);
		return FL_PREEMPT;

	case XK_Left:
		if (!Mctrl->active_item || XFL_IS_TOP_ITEM(Mctrl->active_menu))
		{
			tree = PREV(Mctrl->active_tree);
			menu = tree;
			item = menu->first;
		}
		else
		{
			tree = Mctrl->active_tree;
			item = Mctrl->active_menu;
			menu = item->parent;
		}

		xfl_sync(tree, menu, item);
		return FL_PREEMPT;

	case XK_Up:
		if ((item = Mctrl->active_item) == NULL)
		{
			item = Mctrl->active_menu->last;
		}
		else
		{
			do {
				item = PREV(item);
			} while (XFL_IS_SEPARATOR(item));
		}

		xfl_sync(Mctrl->active_tree, Mctrl->active_menu, item);
		return FL_PREEMPT;

	case XK_Down:
		if ((item = Mctrl->active_item) == NULL)
		{
			item = Mctrl->active_menu->first;
		}
		else
		{
			do {
				item = NEXT(item);
			} while (XFL_IS_SEPARATOR(item));
		}

		xfl_sync(Mctrl->active_tree, Mctrl->active_menu, item);
		return FL_PREEMPT;

	case XK_Return:
	case XK_KP_Enter:
		if (Mctrl->active_item)
		{
			xfl_trigger_menu_item(Mctrl->active_item);
		}
		return FL_PREEMPT;

	case XK_Escape:
		if (XFL_IS_TOP_ITEM(Mctrl->active_menu))
		{
			xfl_sync(NULL, NULL, NULL);
			check_mouse_hover();
			return FL_PREEMPT;
		}

		if (Mctrl->active_item)
		{
			tree = Mctrl->active_tree;
			item = Mctrl->active_menu;
			menu = item->parent;
		}
		else
		{
			/* Weird, but imitates Windows */
			tree = Mctrl->active_tree;
			menu = Mctrl->active_menu;
			item = menu->first;
		}

		xfl_sync(tree, menu, item);
		return FL_PREEMPT;

	default:
		if (isalpha(keysym))
		{
			if (	Mctrl->active_item &&
				XFL_HAS_SUBMENU(Mctrl->active_item))
			{
				/* If the active item has a submenu
				** (which is exposed), search the submenu.
				*/

				menu = Mctrl->active_item;
			}
			else
			{
				/* Otherwise search the active_menu */

				menu = Mctrl->active_menu;
			}

			for (item = menu->first ; item ; item = item->next)
			{
				if (XFL_IS_SEPARATOR(item))
					continue;

				if (keysym == item->hotkey)
				{
					xfl_trigger_menu_item(item);
					return FL_PREEMPT;
				}
			}
		}
	}

	return 0;
}

/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

static void xfl_sync(XFL_MENU *tree, XFL_MENU *menu, XFL_MENU *item)
{
	if (!tree && !menu && !item)
	{
		/* If we're deactivating the entire menu,
		** we no longer need to trap button presses
		** (if we were trapping them).
		*/

		xfl_mouse_in_the_house();

		/* And a main menu item has not been pushed */

		Mctrl->pushed = NO;

		/* And we don't need to see the status window anymore */

		fl_hide_object(Mctrl->status);
	}

	/* Tear down (from bottom up) */

	if (Mctrl->active_item && (!item || item != Mctrl->active_item))
	{
		xfl_deselect_menu_item(Mctrl->active_item);

		if (XFL_HAS_SUBMENU(Mctrl->active_item))
		{
			Mctrl->active_menu =
				xfl_hide_tree(Mctrl->active_item, menu);
		}

		Mctrl->active_item = NULL;
	}

	if (Mctrl->active_menu && (!menu || menu != Mctrl->active_menu))
	{
		Mctrl->active_menu = xfl_hide_tree(Mctrl->active_menu, menu);
	}

	if (Mctrl->active_tree && (!tree || tree != Mctrl->active_tree))
	{
		fl_set_object_boxtype(Mctrl->active_tree->obj, FL_FLAT_BOX);
		Mctrl->active_tree = NULL;
	}

	/* Build (from top down) */

	if (tree)
	{
		fl_show_object(Mctrl->status);

		if (Mctrl->pushed)
			fl_set_object_boxtype(tree->obj, FL_DOWN_BOX);
		else
			fl_set_object_boxtype(tree->obj, FL_UP_BOX);

		Mctrl->active_tree = tree;

		if (menu)
		{
			if (menu != Mctrl->active_menu)
			{
				xfl_show_submenu(menu);
				Mctrl->active_menu = menu;
			}

			if (item && item != Mctrl->active_item)
			{
				xfl_select_menu_item(item);

				if (XFL_HAS_SUBMENU(item))
				{
					fl_set_timer(Mctrl->timer,
						(double) 0.25);
				}

				Mctrl->active_item = item;
			}
		}
	}
}

/*----------------------------------------------------------------------*/

static void xfl_trigger_menu_item(XFL_MENU *item)
{
	if (XFL_HAS_SUBMENU(item))
	{
		if (!Mctrl->active_tree)
		{
			xfl_sync(item, item, item->first);
			Mctrl->pushed = YES;
		}
		else
		{
			xfl_sync(Mctrl->active_tree, item, item->first);
		}
	}
	else
	{
		xfl_sync(NULL, NULL, NULL);
		fl_call_object_callback(item->obj);
		check_mouse_hover();
	}
}

/*----------------------------------------------------------------------*/

static void check_mouse_hover(void)
{
	XFL_MENU *menu;

	/* If the mouse if hovering above a main menu item,
	** give a litle feedback.
	*/

	for (menu = Mctrl->menu->first ; menu ; menu = menu->next)
	{
		if (menu->obj->belowmouse)
		{
			xfl_sync(menu, NULL, NULL);
			break;
		}
	}
}

/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

/*----------------------------------------------------------------------*/
/* Called for all FL_LEAVE events, this traps all button presses so
** if the user presses a button when the mouse is not within the menu,
** we can deactivate the menu.
*/

static void xfl_mouse_on_the_loose(void)
{
	fl_set_timer(Mctrl->timer, (double) 0.0);

	/* Chain into the form's raw callback */

	Mctrl->raw_button_press = fl_register_raw_callback(
		Mctrl->form,
		ButtonPressMask | ButtonReleaseMask,
		xfl_mousetrap);
}

/*----------------------------------------------------------------------*/
/* Called for all FL_ENTER events, this stops trapping button presses.
*/

static void xfl_mouse_in_the_house(void)
{
	if (Mctrl->form->push_callback == xfl_mousetrap)
	{
		/* Restore the form's raw callback */

		fl_register_raw_callback(
			Mctrl->form,
			ButtonPressMask | ButtonReleaseMask,
			Mctrl->raw_button_press);
	}
}

/*----------------------------------------------------------------------*/
/* Caught a button press when the mouse was on the loose.
*/

static int xfl_mousetrap(FL_FORM *form, void *xevent)
{
	/* Give the application first crack at the event */

	if (Mctrl->raw_button_press)
	{
		int ret = (*Mctrl->raw_button_press)(form, xevent);

		if (ret != 0)
			return(ret);
	}

	if (((XEvent*)xevent)->type == ButtonPress)
	{
		xfl_sync(NULL, NULL, NULL);
	}

	return 0;
}

/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

static void xfl_submenu_expose_timer_cb(FL_OBJECT *o, long arg)
{
	xfl_show_submenu(Mctrl->active_item);
}

/*----------------------------------------------------------------------*/
/* Menu show/hide routines						*/
/*----------------------------------------------------------------------*/

static void xfl_show_submenu(XFL_MENU *menu)
{
	XFL_MENU *mi;

	if (menu->subframe)
		fl_show_object(menu->subframe);

	for (mi = menu->first ; mi ; mi = mi->next)
	{
		SUBMENU_CLASS_DATA *cd = mi->obj->c_vdata;

		fl_show_object(mi->obj);

		if (XFL_IS_SEPARATOR(mi))
			continue;

		if (cd->left)
			fl_show_object(cd->left);

		if (cd->center)
			fl_show_object(cd->center);

		if (cd->right)
			fl_show_object(cd->right);
	}
}

/*----------------------------------------------------------------------*/

static void xfl_hide_submenu(XFL_MENU *menu)
{
	XFL_MENU *mi;

	if (menu->subframe)
		fl_hide_object(menu->subframe);

	for (mi = menu->first ; mi ; mi = mi->next)
	{
		SUBMENU_CLASS_DATA *cd = mi->obj->c_vdata;

		fl_hide_object(mi->obj);

		if (XFL_IS_SEPARATOR(mi))
			continue;

		if (cd->left)
			fl_hide_object(cd->left);

		if (cd->center)
			fl_hide_object(cd->center);

		if (cd->right)
			fl_hide_object(cd->right);
	}
}

/*----------------------------------------------------------------------*/
/* Menu item select/deselect routines					*/
/*----------------------------------------------------------------------*/

static void xfl_select_menu_item(XFL_MENU *mi)
{
	SUBMENU_CLASS_DATA *cd = mi->obj->c_vdata;

	if (cd->left)
	{
		cd->left->lcol = FL_WHITE;
#ifdef TRICOLOR
		cd->left->col1 = FL_RED;
#else
		cd->left->col1 = FL_BLUE;
#endif
		fl_show_object(cd->left);
	}

	if (cd->center)
	{
		cd->center->lcol = FL_WHITE;
#ifdef TRICOLOR
		cd->center->col1 = FL_GREEN;
#else
		cd->center->col1 = FL_BLUE;
#endif
		fl_show_object(cd->center);
	}

	if (cd->right)
	{
		cd->right->lcol = FL_WHITE;
		cd->right->col1 = FL_BLUE;
		fl_show_object(cd->right);
	}

	if (mi->status)
	{
		fl_set_object_label(Mctrl->status, mi->status);
	}
}

/*----------------------------------------------------------------------*/

static void xfl_deselect_menu_item(XFL_MENU *mi)
{
	SUBMENU_CLASS_DATA *cd = mi->obj->c_vdata;

	if (cd->left)
	{
		cd->left->lcol = FL_BLACK;
		cd->left->col1 = FL_COL1;
		fl_show_object(cd->left);
	}

	if (cd->center)
	{
		cd->center->lcol = FL_BLACK;
		cd->center->col1 = FL_COL1;
		fl_show_object(cd->center);
	}

	if (cd->right)
	{
		cd->right->lcol = FL_BLACK;
		cd->right->col1 = FL_COL1;
		fl_show_object(cd->right);
	}

	fl_set_object_label(Mctrl->status, "");
}

/*----------------------------------------------------------------------*/
/* Tears down the menu from 'active' to its root.
** 'new' is the menu that will become the next active_menu.
** It is passed to this routine so it can optimize a little
** and not tear down the entire tree if 'new' is a branch
** on the same tree.
*/

static XFL_MENU *xfl_hide_tree(XFL_MENU *active, XFL_MENU *new)
{
	if (new && new->parent == active)
	{
		return(active);
	}

	for ( ; active != new ; active = active->parent)
	{
		xfl_hide_submenu(active);

		if (XFL_IS_TOP_ITEM(active))
			return(NULL);
	}

	return(active);
}

/*----------------------------------------------------------------------*/

static FL_Coord xfl_string_width(const char *s)
{
	return fl_get_string_width( XFL_menu_fontstyle, XFL_menu_fontsize,
		s, strlen(s));
}

#ifdef TESTMAIN
/*----------------------------------------------------------------------*/
/* Test Driver								*/
/*----------------------------------------------------------------------*/

#include "check.xbm"
#include "blank.xbm"

/*----------------------------------------------------------------------*/

typedef enum
{
	MAIN_MENU,
	FILE_MENU, FILE_CLOSE,
	EDIT_MENU, EDIT_FIND,
	COLOR_MENU, COLOR_REVERSE,
	PALETTE_MENU, PALETTE_RED, PALETTE_GREEN, PALETTE_BLUE,
	VIEW_MENU, VIEW_DETAILS, VIEW_REFRESH, VIEW_OPTIONS,
	HELP_MENU, HELP_TOPICS, HELP_ABOUT
} MENU_ITEMS;

/*----------------------------------------------------------------------*/

XFL_MENU *Main_menu;
XFL_MENU *File_menu;
XFL_MENU *Edit_menu;
XFL_MENU *Color_menu;
XFL_MENU *Palette_menu;
XFL_MENU *View_menu;
XFL_MENU *Help_menu;

/*----------------------------------------------------------------------*/

void build_menus(FL_Coord x, FL_Coord y, FL_Coord h);
void menu_cb(FL_OBJECT *obj, long arg);

/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

int main(int argc, char *argv[])
{
	FL_FORM *form;
	FL_OBJECT *obj;

	fl_initialize(&argc, argv, 0, 0, 0);

	form = fl_bgn_form(FL_FRAME_BOX, 790, 360);
	/* Draw a horizontal line beneath the main menu */
	fl_add_box(FL_FRAME_BOX, 0, 0, form->w, 25, "");

	build_menus(10, 3, 18);

	fl_end_form();

	fl_show_form(form, FL_PLACE_FREE, FL_FULLBORDER, "menu");

	obj = fl_do_forms();

	fprintf(stderr, "objclass %d, type %d, label %s\n",
		obj->objclass, obj->type,
		obj->label ? obj->label : "(none)");

	fl_finish();
	return(0);
}

/*----------------------------------------------------------------------*/

void build_menus(FL_Coord x, FL_Coord y, FL_Coord h)
{
	/* Main menu */

	Main_menu = xfl_bgn_menu(x, y, h);

	/* File menu */

	File_menu = xfl_add_menu(Main_menu, "^File");

	xfl_add_menu_item(File_menu, "^Close", menu_cb, FILE_CLOSE, 0,
		"Closes the window.");

	/* Edit menu */

	Edit_menu = xfl_add_menu(Main_menu, "^Edit");

	xfl_add_menu_item(Edit_menu, "^Find", menu_cb, EDIT_FIND,
		FL_CONTROL_MASK | 'f', "Searches the current window for text.");

	/* Edit/Color menu */

	Color_menu = xfl_add_menu(Edit_menu, "^Color");

	xfl_add_menu_item(Color_menu, "^Reverse", menu_cb, COLOR_REVERSE,
		FL_CONTROL_MASK | 'Y', "Reverses the colors on the screen.");

	/* Edit/Color/Palette menu */

	Palette_menu = xfl_add_menu(Color_menu, "^Palette");

	xfl_add_menu_item(Palette_menu, "^Red", menu_cb, PALETTE_RED,
		FL_ALT_MASK | 'r', "Make it red.");
	xfl_add_menu_item(Palette_menu, "^Green", menu_cb, PALETTE_GREEN,
		FL_SHIFT_MASK | 'g', "Make it green.");
	xfl_add_menu_item(Palette_menu, "^Blue", menu_cb, PALETTE_BLUE,
		FL_SHIFT_MASK | FL_CONTROL_MASK | 'B', "Make it blue.");

	/* View menu */

	View_menu = xfl_add_menu(Main_menu, "^View");

	xfl_add_menu_item(View_menu, "^Details", menu_cb, VIEW_DETAILS, 0,
		"Displays information about each item in the window.");
	xfl_add_separator(View_menu);
	xfl_add_menu_item(View_menu, "^Refresh", menu_cb, VIEW_REFRESH, 0,
		"Refresh item information.");
	xfl_add_menu_item(View_menu, "^Options...", menu_cb, VIEW_OPTIONS, 0,
		"Changes the display options for this window.");

	/* Help menu */

	Help_menu = xfl_add_menu(Main_menu, "^Help");
	xfl_add_menu_item(Help_menu, "^Help Topics", menu_cb, HELP_TOPICS,
		XK_F1, "Opens Help.");
	xfl_add_menu_item(Help_menu, "^About xshm", menu_cb, HELP_ABOUT, 0,
		"Display program information, version number, and copyright.");

	xfl_end_menu(Main_menu);
}

/*----------------------------------------------------------------------*/

void menu_cb(FL_OBJECT *obj, long arg)
{
	static char fmt[] = "menu_cb: arg %s\n";

#define CASE(tag)	case tag: fprintf(stderr, fmt, #tag); break

	switch (arg)
	{
		CASE(MAIN_MENU);
		CASE(FILE_MENU);
		CASE(FILE_CLOSE);
		CASE(EDIT_MENU);
		CASE(EDIT_FIND);
		CASE(COLOR_MENU);
		CASE(COLOR_REVERSE);
		CASE(PALETTE_MENU);
		CASE(PALETTE_RED);
		CASE(PALETTE_GREEN);
		CASE(PALETTE_BLUE);
		CASE(VIEW_MENU);
		CASE(VIEW_DETAILS);
		CASE(VIEW_REFRESH);
		CASE(VIEW_OPTIONS);
		CASE(HELP_MENU);
		CASE(HELP_TOPICS);
		CASE(HELP_ABOUT);
	}

	if (arg == FILE_CLOSE)
		exit(0);

	if (arg == EDIT_FIND)
	{
#include "/usr/include/X11/bitmaps/keyboard16"
		SUBMENU_CLASS_DATA *cd = obj->c_vdata;
		fl_set_bitmap_data(cd->left,
			keyboard16_width, keyboard16_height,
			keyboard16_bits);
	}

	if (arg == VIEW_DETAILS)
	{
		SUBMENU_CLASS_DATA *cd = obj->c_vdata;
		XFL_MENU *item = cd->item;

		if (item->val == 0)
		{
			item->val = 1;
			fl_set_bitmap_data(cd->left,
				check_xbm_width, check_xbm_height,
				check_xbm_bits);
		}
		else
		{
			item->val = 0;
			fl_set_bitmap_data(cd->left,
				blank_xbm_width, blank_xbm_height,
				blank_xbm_bits);
		}
	}
}

#endif /* TESTMAIN */
