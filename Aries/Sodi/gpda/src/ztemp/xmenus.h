/*----------------------------------------------------------------------*/
/* Every menu has an XFL_MENU_CTRL structure.
*/

typedef struct
{
	/* The form on which to display the menu */

	FL_FORM *form;

	/* Display the first item in the main (horizontal) menu here */

	FL_Coord x, y;

	/* The height of all menu items */

	FL_Coord h;

	/* The main menu.  Note that this is not the
	** first item in the main menu; its child is.
	*/

	struct xfl_menu *menu;

	/* The status window */

	FL_OBJECT *status;

	/* The submenu expose timer */

	FL_OBJECT *timer;

	/* Runtime variables */

	FL_RAW_CALLBACK raw_button_press;
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

	/* Points to the currently highlighted item, if there is one.
	*/

	struct xfl_menu *active_item;

	/* True after pushing a main menu item */

	int pushed;

#define XFL_MAX_SHORTCUTS 128
	struct
	{
		KeySym	keysym;
		struct xfl_menu *item;
	} shortcut [ XFL_MAX_SHORTCUTS ];
	int n_shortcuts;

} XFL_MENU_CTRL;

/*----------------------------------------------------------------------*/
/* Both menus and menu items share this structure.
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
