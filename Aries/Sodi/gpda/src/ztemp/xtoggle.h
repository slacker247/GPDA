/*----------------------------------------------------------------------*/
/* The xfl_toggle_form() function toggles the state of a TOGGLE_FORM between
** the shown and visible states.  It is normally called by the callback
** routine for a button and/or a checkbox menu item.  It also the appearance
** of these controls based on the state of the form.
*/

typedef struct
{
	/* The form to hide/show */

	FL_FORM *form;

	/* The form's title */

	char *title;

	/* Optional button whose callback calls xfl_toggle_form() */

	FL_OBJECT *button;

	/* Label to display in the button when the form is hidden */

	char *show_button_label;

	/* Label to display in the button when the form is shown */

	char *hide_button_label;

	/* Optional checkbox menu item whose callback calls xfl_toggle_form() */

	XFL_MENU *menu_item;

} XFL_TOGGLE_FORM;

/*----------------------------------------------------------------------*/

void xfl_toggle_form(XFL_TOGGLE_FORM *tf);
void xfl_close_toggle_form(XFL_TOGGLE_FORM *tf);
