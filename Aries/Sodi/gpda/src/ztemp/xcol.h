/*----------------------------------------------------------------------*/

#ifndef _XFL_SYSCOL_DEFINED
#define _XFL_SYSCOL_DEFINED
#define XFL_SYSCOL void
#endif

/*----------------------------------------------------------------------*/
/* User structure.
*/

typedef struct
{
	char *label;		/* Label on column button */
	int (*cmp)(const void *e1, const void *e2);
	int (*fmt)(char *buf, int width, void *data);
	int nchars;		/* Width of column in characters */
	int order;		/* Left-to-right ordering */
	XFL_SYSCOL *sys;	/* Private data */

} XFL_COLUMN;

/*----------------------------------------------------------------------*/
/* Values for the sort_order field.
*/

typedef enum { SORT_ASCENDING, SORT_DESCENDING } XFL_SORT_ORDER;

/*----------------------------------------------------------------------*/
/* User structure.
*/

typedef struct
{
	/* The following fields must be set by the user
	** before calling xfl_add_columns.
	*/

	XFL_COLUMN *cols;		/* List of columns */
	int n;				/* Number of columns in list */
	XFL_COLUMN *sort_key;		/* Column currently being sorted on */
	XFL_SORT_ORDER sort_order;	/* Ascending or descending */
	int btype;			/* Button type */
	int bstyle;			/* Style of button label */
	int bsize;			/* Size of button label */
	int bheight;			/* Height of buttons */
	int tstyle;			/* Style of text (detail lines) */
	int tsize;			/* Size of text (details lines) */
	FL_OBJECT *neighbor;		/* Attach buttons across top of this */
	FL_OBJECT *trigger;		/* Trigger this on button press */

	/* The following fields are initialized by xfl_add_columns
	** and must not be modified by the user.
	*/

	/* A list of pointers to columns sorted in left-to-right 'order' */

	XFL_COLUMN **ordered_cols;

	/* The number of columns currently in view */

	int n_in_view;

	XFL_SYSCOL *sys;	/* Private data */

} XFL_COLUMN_CONTROL;

/*----------------------------------------------------------------------*/

/* xcol.c */
int xfl_add_columns(XFL_COLUMN_CONTROL *ctrl);
void xfl_order_column(XFL_COLUMN_CONTROL *ctrl, XFL_COLUMN *col, int order);
void xfl_config_columns(XFL_COLUMN_CONTROL *ctrl);
