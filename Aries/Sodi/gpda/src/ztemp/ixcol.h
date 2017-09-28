/*----------------------------------------------------------------------*/
/* XFL internal structure.
*/

typedef struct
{
	FL_Coord width;		/* Width of column in coordinate units */
	FL_OBJECT *column_button;	/* Non-null if column in view */
	FL_OBJECT *config_button;	/* On the config form */

} XFL_SYSCOL;

#define _XFL_SYSCOL_DEFINED

#define SYSCOL(s) ((XFL_SYSCOL*)(s->sys))
