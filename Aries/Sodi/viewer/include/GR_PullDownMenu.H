#ifndef GR_PULLDOWN
#define GR_PULLDOWN

#include "GR_Widget.H"
#include <Xm/RowColumn.h>
#include <Xm/PushB.h>

class GR_MenuItem
{
	private:
		char			*p_menutitle;
		char			p_mnemonic;
		char			*p_accelerator;
		WidgetClass	*p_class;
		XtCallbackFunc p_callback;
		XtPointer		p_client_data;
		friend class GR_PullDownMenu;
	public:
		GR_MenuItem (char *title, XtCallbackFunc callback, XtPointer data);
		GR_MenuItem (char *title, GR_PullDownMenu &menu);
		GR_MenuItem (char *title, GR_PullDownMenu *menu);

		void setMnemonic (char);
		void setAccelerator (char *);
		void setWidgetClass (WidgetClass *);
};

class GR_PullDownMenu : public GR_Widget
{
	private:
		Widget v_createWidget (char*, GR_Widget*);

	public:
		void addItem (char *title, XtCallbackFunc callback, XtPointer data);
		void addItem (char *title, GR_PullDownMenu &menu);
		void addItem (char *title, GR_PullDownMenu *menu);
};

	/**********************************************/
	pulldown = new GR_PullDownMenu;
	pulldown->addItem ("Open", open_file, this);
	pulldown->addItem ("New", new_file, this);
	pulldown->addItem ("Close", close_file, this);
	pulldown->addItem ("Quit", quit_app, this);
	pulldown->createWidget ("FilePdown", menubar);

	/***********************************************/
	GR_MenuItem open_item ("Open", open_file, this);
	GR_MenuItem new_item ("New", new_file, this);
	GR_MenuItem close_item ("Close", close_file, this);
	GR_MenuItem quit_item ("Quit", quit_app, this);

	open_item.Mnemonic ('O');
	open_item.WidgetClass (&XmPushButtonClass);
	new_item.Mnemonic ('N');
	new_item.WidgetClass (&XmPushButtonClass);
	close_item.Mnemonic ('C');
	close_item.WidgetClass (&XmPushButtonClass);
	quit_item.Mnemonic ('Q');
	quit_item.WidgetClass (&XmPushButtonClass);

	pulldown = new GR_PullDownMenu;
	pulldown->addItem (open_item);
	pulldown->addItem (new_item);
	pulldown->addItem (close_item);
	pulldown->addItem (quit_item);

	/************************************************/

	GR_MenuItem open_item ("Open", open_file, this);
	GR_MenuItem new_item ("New", new_file, this);
	GR_MenuItem close_item ("Close", close_file, this);
	GR_MenuItem quit_item ("Quit", quit_file, this);

	pulldown = new GR_PullDownMenu;
	pulldown->createWidget ("pulldown", menubar);

	open_item.Mnemonic ('O');
	open_item.WidgetClass (&XmPushButtonClass);
	open_item.createWidget ("OpenMenu", pulldown);
	new_item.Mnemonic ('N');
	new_item.WidgetClass (&XmPushButtonClass);
	new_item.createWidget ("NewMenu", pulldown);
	close_item.Mnemonic ('C');
	close_item.WidgetClass (&XmPushButtonClass);
	close_item.createWidget ("CloseMenu", pulldown);
	quit_item.Mnemonic ('Q');
	quit_item.WidgetClass (&XmPushButtonClass);
	quit_item.createWidget ("QuitMenu", pulldown);












typedef struct _menu_item {
    char        *label;         /* the label for the item */
    WidgetClass *class;         /* pushbutton, label, separator... */
    char         mnemonic;      /* mnemonic; NULL if none */
    char        *accelerator;   /* accelerator; NULL if none */
    char        *accel_text;    /* to be converted to compound string */
    void       (*callback)();   /* routine to call; NULL if none */
    XtPointer    callback_data; /* client_data for callback() */
    struct _menu_item *subitems; /* pullright menu items, if not NULL */
} MenuItem;

Widget
v_createWidget (char *name, GR_Widget *parent)
Widget parent;
char *menu_title, menu_mnemonic;
MenuItem *items;
{
    Widget PullDown, cascade, widget;
    int i;
    XmString str;

    PullDown = XmCreatePulldownMenu(parent, "_pulldown", NULL, 0);

    str = XmStringCreateSimple(menu_title);
    cascade = XtVaCreateManagedWidget(menu_title,
        xmCascadeButtonGadgetClass, parent,
        XmNsubMenuId,   PullDown,
        XmNlabelString, str,
        XmNmnemonic,    menu_mnemonic,
        NULL);
    XmStringFree(str);

    /* Now add the menu items */
    for (i = 0; items[i].label != NULL; i++) {
        /* If subitems exist, create the pull-right menu by calling this
         * function recursively.  Since the function returns a cascade
         * button, the widget returned is used..
         */
        if (items[i].subitems)
            widget = BuildPulldownMenu(PullDown,
                items[i].label, items[i].mnemonic, items[i].subitems);
        else
            widget = XtVaCreateManagedWidget(items[i].label,
                *items[i].class, PullDown,
                NULL);
        /* Whether the item is a real item or a cascade button with a
         * menu, it can still have a mnemonic.
         */
        if (items[i].mnemonic)
            XtVaSetValues(widget, XmNmnemonic, items[i].mnemonic, NULL);
        /* any item can have an accelerator, except cascade menus. But,
         * we don't worry about that; we know better in our declarations.
         */
        if (items[i].accelerator) {
            str = XmStringCreateSimple(items[i].accel_text);
            XtVaSetValues(widget,
                XmNaccelerator, items[i].accelerator,
                XmNacceleratorText, str,
                NULL);
            XmStringFree(str);
        }
        /* again, anyone can have a callback -- however, this is an
         * activate-callback.  This may not be appropriate for all items.
         */
        if (items[i].callback)
            XtAddCallback(widget, XmNactivateCallback,
                items[i].callback, items[i].callback_data);
    }
    return cascade;
}

