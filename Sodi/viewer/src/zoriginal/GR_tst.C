#include "GR_Shell.H"
#include "GR_DispList.H"
#include "GR_Window.H"
#include "GR_DispObj.H"
#include "GR_PushButton.H"
#include <Xm/Form.h>
#include <stdio.h>
#include <unistd.h>
#include <Xm/MessageB.h>
#include <Xm/Frame.h>
#include <Xm/Label.h>

String colors[] = { "Black", "Red", "Green", "Blue" };

class Square : public GR_DispObj
{
	private:
		long		p_color;
		int			p_id;
		float		p_size;
		GR_DispObj	*p_objlist[32];
		int			p_nextobj;
	public:
		Square (int id, float size, long color);

		void objdraw ();
		void v_process_pick (GR_MouseEvent&, GR_Window*);
		void pick_event (GR_MouseEvent&, GR_DispObj*, GR_Window*);
		void drag_event (GR_MouseEvent&, GR_Window*);
};

Square::Square (int id, float size, long color)
{
	char string [3];

	p_id = id;
	sprintf (string, "%d", id);
	p_color = color;
	p_size = size;
	p_nextobj = 0;
}

void
Square::objdraw ()
{
	GR_color (p_color);
	GR_rectf (-p_size/2.0, -p_size/2.0, p_size/2.0, p_size/2.0);
}

void
Square::v_process_pick (GR_MouseEvent& event, GR_Window* window)
{
	Widget dialog;
	Arg arg[1];
	XmString t;
	char string [32];

	sprintf (string, "Square #%d picked\n", p_id);
	printf ("a square was picked\n");

	switch (event.button)
	{
		case GR_LEFTMOUSE:
			if (event.down)
			{
				printf ("GR_LEFTMOUSE...\n");
				printf ("%s\n", string);
				window->set_drag_event (this);
			}
			break;
		case GR_MIDDLEMOUSE:
			if (!event.down)
			{
				p_nextobj = 0;
				printf ("GR_MIDDLEMOUSE...Pick another square\n");
				window->set_pick_event (this);
			}
			break;
		case GR_RIGHTMOUSE:
			if (event.down)
			{
				printf ("GR_RIGHTMOUSE...Menu\n");
				t = XmStringCreateSimple (string);
				XtSetArg (arg[0], XmNmessageString, t);
				dialog = XmCreateMessageDialog (window->widget(), "message", arg, 1);
				XmStringFree (t);
				XtManageChild (dialog);
			}
			break;
	};
}

void
Square::drag_event (GR_MouseEvent& event, GR_Window *window)
{
	GR_DispObj 		*dispobj;
	float xwind1, ywind1, xwind2, ywind2;
	int i;

	if (event.buttonstate.left)
		printf ("(LEFT)");
	if (event.buttonstate.middle)
		printf ("(MIDDLE)");
	if (event.buttonstate.right)
		printf ("(RIGHT)");

	window->convert_screen2 (event.x, event.y, xwind1, ywind1);
	window->convert_screen2 (event.lastx, event.lasty, xwind2, ywind2);
	printf ("translate (%f, %f)\n", xwind1 - xwind2, ywind1 - ywind2);
	this->translate (xwind1 - xwind2, ywind1 - ywind2, 0.0);
	for (i=0; i<p_nextobj; i++)
		p_objlist[i]->translate (xwind1 - xwind2, ywind1 - ywind2, 0.0);
	window->draw ();
}

void
Square::pick_event (GR_MouseEvent& event, GR_DispObj *object, GR_Window* window)
{
	Square	*square;

	square = (Square *) object;

	if (p_nextobj < 32 && square != this)
	{
		p_objlist [p_nextobj] = object;
		p_nextobj++;
	}

	printf ("Square #%d got Square #%d\n", p_id, square->p_id);
	if (event.button == GR_MIDDLEMOUSE)
	{
		printf ("GR_MIDDLEMOUSE...Pick another square\n");
		window->set_pick_event (this);
	}
}

void
GR_initialize (int argc, char *argv[])
{
	GR_DispList		*displist;
	GR_Window *window;
	GR_Shell *shell;
	GR_PushButton	*pushbutton;
	Square *square;
	float size = 1.0;
	Widget		form;
	int i;
	Widget menubar, menu, widget;
	XmString file, edit, help, p_new, quit, red, green, blue, black;

	shell = new GR_Shell;
	shell->createWidget ("GR_TEST");

	form = XtCreateManagedWidget ("form", xmFormWidgetClass, shell->widget(), 0, 0);

	/* Create a simple MenuBar that contains three menus */
	file = XmStringCreateSimple("File");
	edit = XmStringCreateSimple("Edit");
	help = XmStringCreateSimple("Help");
	menubar = XmVaCreateSimpleMenuBar(form, "menubar",
			XmVaCASCADEBUTTON, file, 'F',
			XmVaCASCADEBUTTON, edit, 'E',
			XmVaCASCADEBUTTON, help, 'H',
			XmNtopAttachment, XmATTACH_FORM,
			XmNleftAttachment, XmATTACH_FORM,
			XmNrightAttachment, XmATTACH_FORM,
			NULL);
	XmStringFree(file);
	XmStringFree(edit);
	/* don't free "help" compound string yet -- reuse it later */

	/* Tell the menubar which button is the help menu  */
	if (widget = XtNameToWidget(menubar, "button_2"))
			XtVaSetValues(menubar, XmNmenuHelpWidget, widget, NULL);

	/* First menu is the File menu -- callback is file_cb() */
	p_new = XmStringCreateSimple("New ...");
	quit = XmStringCreateSimple("Quit");
	XmVaCreateSimplePulldownMenu(menubar, "file_menu", 0, 0,
			XmVaPUSHBUTTON, p_new, 'N', NULL, NULL,
			XmVaSEPARATOR,
			XmVaPUSHBUTTON, quit, 'Q', NULL, NULL,
			NULL);
	XmStringFree(p_new);
	XmStringFree(quit);

	/* Second menu is the Edit menu -- callback is change_color() */
	black = XmStringCreateSimple(colors[0]);
	red = XmStringCreateSimple(colors[1]);
	green = XmStringCreateSimple(colors[2]);
	blue = XmStringCreateSimple(colors[3]);
	menu = XmVaCreateSimplePulldownMenu(menubar, "edit_menu", 1, 0,
			XmVaRADIOBUTTON, black, 'k', NULL, NULL,
			XmVaRADIOBUTTON, red, 'R', NULL, NULL,
			XmVaRADIOBUTTON, green, 'G', NULL, NULL,
			XmVaRADIOBUTTON, blue, 'B', NULL, NULL,
			XmNradioBehavior, True,     /* RowColumn resources to enforce */
			XmNradioAlwaysOne, True,    /* radio behavior in Menu */
			NULL);
	XmStringFree(black);
	XmStringFree(red);
	XmStringFree(green);
	XmStringFree(blue);

	/* Initialize menu so that "black" is selected. */
	if (widget = XtNameToWidget(menu, "button_0"))
			XtVaSetValues(widget, XmNset, True, NULL);

	/* Third menu is the help menu -- callback is help_cb() */
	XmVaCreateSimplePulldownMenu(menubar, "help_menu", 2, 0,
			XmVaPUSHBUTTON, help, 'H', NULL, NULL,
			NULL);
	XmStringFree(help); /* we're done with it; now we can free it */

	XtManageChild(menubar);

	if (argc > 1)
		size = atof (argv[1]);

	for (i=0; i<argc; i++)
		printf ("argv [%d] = %s\n", i, argv[i]);

	displist = new GR_DispList;
	window = new GR_Window;
	window->rgbmode ();
	window->doublebuffer ();
	window->createWidget ("TstWindow", form);
	XtVaSetValues (window->widget (),
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, menubar,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		NULL);

	shell->realize ();
	window->set_viewmode (GR_ORTHO2);
	window->left (0.0);
	window->right (10.0);
	window->bottom (0.0);
	window->top (10.0);
	window->set_display_list (displist);

	for (i=0; i<=36; i++)
	{
		square = new Square (i, size, GR_color (255, 125, 0));
		square->translate (5.0 - 0.5 * size, 0.0, 0.0);
		square->rotate_z ((float) i * 10.0);
		square->translate (5.0, 5.0, 0.0);
		displist->add_object (*square);
	}
}
