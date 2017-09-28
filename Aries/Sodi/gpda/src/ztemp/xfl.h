#ifndef _XFL_H
#define _XFL_H

/*   Example compile and load command:
gcc xps.c xps_fdui.c xtoggle.c argv.c libxfl.a -L/usr/local/Xforms/FORMS \
-lforms -L$HOME/Sodi/Lib/X11 -L/usr/X11R6/lib -lX11 
*/

/*----------------------------------------------------------------------*/
/*                          X C O L S . H                               */
/*----------------------------------------------------------------------*/
typedef enum { SORT_ASCENDING, SORT_DESCENDING } XFL_SORT_ORDER;
/*
** User structures.
*/
typedef struct {
	char *label;				/* Label on column button */
	int (*cmp)(const void *e1, const void *e2);
	int (*fmt)(char *buf, int width, void *data);
	int nchars;					/* Width of column in characters */
	int order;					/* Left-to-right ordering */
	XFL_SYSCOL *sys;			/* Private data */

} XFL_COLUMN;

typedef struct {
	/*
	** The following fields must be set by the user
	** before calling xfl_add_columns.
	*/
	XFL_COLUMN *cols;			/* List of columns */
	int n;						/* Number of columns in list */
	XFL_COLUMN *sort_key;		/* Column currently being sorted on */
	XFL_SORT_ORDER sort_order;	/* Ascending or descending */
	int btype;					/* Button type */
	int bstyle;					/* Style of button label */
	int bsize;					/* Size of button label */
	int bheight;				/* Height of buttons */
	int tstyle;					/* Style of text (detail lines) */
	int tsize;					/* Size of text (details lines) */
	FL_OBJECT *neighbor;		/* Attach buttons across top of this */
	FL_OBJECT *trigger;			/* Trigger this on button press */
	/*
	** The following fields are initialized by xfl_add_columns
	** and must not be modified by the user.
	*/
	XFL_COLUMN **ordered_cols;	/* A list of pointers to columns sorted in left-to-right 'order' */ 
	int n_in_view;				/* The number of columns currently in view */ 
	XFL_SYSCOL *sys;			/* Private data */

} XFL_COLUMN_CONTROL;

int xfl_add_columns(XFL_COLUMN_CONTROL *ctrl);
void xfl_order_column(XFL_COLUMN_CONTROL *ctrl, XFL_COLUMN *col, int order);
void xfl_config_columns(XFL_COLUMN_CONTROL *ctrl);

/*----------------------------------------------------------------------*/
/*                          X M I S C . H                               */
/*----------------------------------------------------------------------*/
#ifndef ERROR
#define ERROR	(-1)
#endif
#ifndef OK
#define OK	0
#endif
#ifndef YES
#define YES	1
#endif
#ifndef NO
#define NO	0
#endif
#ifndef MAX
#define MAX(a, b) (((a) > (b)) ? (a) : (b))
#endif

void xfl_set_input_time(FL_OBJECT *obj, time_t ts);
void xfl_set_input_long(FL_OBJECT *obj, long value);
void xfl_set_input_short(FL_OBJECT *obj, short value);
void xfl_deactivate_button(FL_OBJECT *obj);
void xfl_activate_button(FL_OBJECT *obj);
void xfl_trace_event(char *func, FL_OBJECT *o, int event, FL_Coord mx, FL_Coord my, int key, void *xev);

/*----------------------------------------------------------------------*/
/*                          X T O G G L E . H                           */
/*----------------------------------------------------------------------*/
/*
** The xfl_toggle_form() function toggles the state of a TOGGLE_FORM between
** the shown and visible states.  It is normally called by the callback
** routine for a button and/or a checkbox menu item.  It also the appearance
** of these controls based on the state of the form.
*/

typedef struct
{
	FL_FORM *form;			/* The form to hide/show */
	char *title;			/* The form's title */
	FL_OBJECT *button;		/* Optional button whose callback calls xfl_toggle_form() */ 
	char *show_button_label;/* Label to display in the button when the form is hidden */ 
	char *hide_button_label;/* Label to display in the button when the form is shown */ 
	XFL_MENU *menu_item;	/* Optional checkbox menu item whose callback calls xfl_toggle_form() */ 
} XFL_TOGGLE_FORM;

void xfl_toggle_form(XFL_TOGGLE_FORM *tf);
void xfl_close_toggle_form(XFL_TOGGLE_FORM *tf);

/*----------------------------------------------------------------------*/
/*                          X M E N U S . H                             */
/*----------------------------------------------------------------------*/
/*
** Every menu has an XFL_MENU_CTRL structure.
*/
#define XFL_MAX_SHORTCUTS 128

typedef struct
{
	FL_FORM *form;			/* The form on which to display the menu */
	FL_Coord x, y;			/* Display the first item in the main (horizontal) menu here */ 
	FL_Coord h;				/* The height of all menu items */
	struct xfl_menu *menu;	/* The main menu.  Note that this is not the 
							   first item in the main menu; its child is. */
	FL_OBJECT *status;		/* The status window */
	FL_OBJECT *timer;		/* The submenu expose timer */
	FL_RAW_CALLBACK raw_button_press;	/* Runtime variables */
	FL_RAW_CALLBACK raw_key_press;

	/* Points to the main menu item that is at the root.
	** of the active menu.  If this main menu item's submenu
	** is not exposed, active_menu will be NULL.
	*/

	struct xfl_menu *active_tree;

	/* Points to the menu that currently has the focus.
	** This would be the menu containing the active_item, if an item
	** is active, or the menu thatwould contain the active_item if the
	** XK_Up or XK_Down is pressed.
	*/

	struct xfl_menu *active_menu;
	struct xfl_menu *active_item;	/* Points to the currently highlighted item, if there is one.*/
	int pushed;						/* True after pushing a main menu item */

	struct {
		KeySym	keysym;
		struct xfl_menu *item;
	} shortcut [ XFL_MAX_SHORTCUTS ];
	int n_shortcuts;

} XFL_MENU_CTRL;

/*
** Both menus and menu items share this structure.
*/

typedef struct xfl_menu
{
	XFL_MENU_CTRL *mc;		/* Link to control */

	/* The text to appear in the menu item's button */

	const char *label;

	/* The text to appear in the status window with the item is selected */

	const char *status;

	/* Main menu items cannot have a hotkey.
	** A hotkey on a submenu's menu item (simple alpha character)
	** triggers the object of the menu item if the hotkey is pressed
	** when the item is on the active_menu.
	*/

	char hotkey;

	/* A shortcut on a main menu item (Alt+something)
	** displays the menu item's submenu.
	** A shortcut on a submenu's menu item (Ctrl-something)
	** triggers the object of the menu item.
	*/

	KeySym shortcut;
	const char *shortcut_label;

	/* The menu on which this item resides */

	struct xfl_menu *parent;

	/* If entry is a menu, these are not NULL.
	** If entry is a menu item, these are NULL.
	*/

	struct xfl_menu *first;
	struct xfl_menu *last;

	/* All entries (whether menus or menu items) reside on a menu.
	** This links all siblings on a menu.
	*/

	struct xfl_menu *prev;
	struct xfl_menu *next;

	/* Menu items have callbacks, menus don't */

	FL_CALLBACKPTR cb;
	long arg;

	/* For toggle buttons, val is the state */

	int val;

	/* Each entry is displayed in an object.
	** Main (horizontal) menu entries are FL_MAIN_MENU_BUTTON objects.
	** All others are FL_SUBMENU_BUTTONS objects.
	*/

	FL_OBJECT *obj;

	/* If this item has children, this frame
	** surrounds the children's items.
	*/

	FL_OBJECT *subframe;

} XFL_MENU;

#define XFL_IS_TOP_ITEM(m)	((m)->parent && !(m)->parent->parent)
#define XFL_HAS_SUBMENU(m)	((m)->first)
extern char XFL_separator[];
#define XFL_SEPARATOR		XFL_separator
#define XFL_IS_SEPARATOR(m)	((m)->label == XFL_SEPARATOR)

/*----------------------------------------------------------------------*/
/* Main menu buttons are displayed horizontally.
*/

#define FL_MAIN_MENU_BUTTON	(FL_USER_CLASS_START + 13)

/*----------------------------------------------------------------------*/
/* Submenu buttons are displayed vertically in a pulldown.
** A submenu button is composed of 3 or 4 objects:
**	1. A bitmap, for displaying an image to the left of the label,
**	2. A label, for displaying text in the center,
**	3. An optional shortcut label, for display to the right of the label,
**	   or an optional bitmap, for displaying a right-pointing triangle
**	   to the right of the label (if this button has children),
**	4. A hidden button superimposed over the first 2 (or 3) objects,
**	   for catching mice.
**
** .------------------------------------.
** |        |       | optional shortcut |
** | bitmap | label | or triangle       |
** `----------- h i d d e n ------------'
**
** The xfl_create_submenu_button function returns the hidden button.
** The hidden button's "c_vdata" pointer points to a structure that contains
** pointers to the 2 (or 3) underlying objects.
*/

typedef struct
{
	XFL_MENU *item;
	FL_OBJECT *left;
	FL_OBJECT *center;
	FL_OBJECT *right;
} SUBMENU_CLASS_DATA;

/* Object class */

#define FL_SUBMENU_BUTTON	(FL_USER_CLASS_START + 14)

/* Object class types */

#define FL_SUBMENU_LEAF		0	/* Item is an item */
#define FL_SUBMENU_BRANCH	1	/* It is a menu */

/*----------------------------------------------------------------------*/
/* menu.c */

XFL_MENU *xfl_bgn_menu(FL_Coord x, FL_Coord y, FL_Coord h);
XFL_MENU *xfl_add_menu(XFL_MENU *parent, const char *label);
XFL_MENU *xfl_add_menu_item(XFL_MENU *parent, const char *label, FL_CALLBACKPTR cb, long arg, KeySym shortcut, const char *status);
XFL_MENU *xfl_add_separator(XFL_MENU *parent);
int xfl_set_menu_item_shortcut(XFL_MENU *item, KeySym keysym);
int xfl_set_menu_item_status(XFL_MENU *item, const char *status);
void xfl_end_menu(XFL_MENU *main_menu);
int xfl_set_menu_fontsize(int size);
int xfl_set_menu_fontstyle(int style);

#endif _XFL_H
