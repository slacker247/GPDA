#ifndef _ARGV_H
#define _ARGV_H

/*----------------------------------------------------------------------*/

#ifndef YES
#define YES 1
#endif

#ifndef NO
#define NO 0
#endif

#ifndef ERROR
#define ERROR (-2)	/* Don't conflict with with EOF */
#endif

#ifndef OK
#define OK 0
#endif

/*----------------------------------------------------------------------*/
/* Object for dynamically maintaining lists of arbitrary objects.  Used by
** build_list(), delete_list_element() and destroy_list().
*/

typedef struct
{
	int argc;	/* # elements in list */
	void **argv;	/* The list */
	size_t max;	/* Don't touch */
	int argn;	/* User variable */
} LIST;

/*----------------------------------------------------------------------*/
/* Object for dynamically maintaining lists of strings.  Used by
** build_argv(), delete_argv_element() and destroy_argv().
*/

typedef struct
{
	int argc;	/* # elements in list */
	char **argv;	/* The list */
	size_t max;	/* Don't touch */
	int argn;	/* User variable */
} ARGV;

/*----------------------------------------------------------------------*/

int build_list(LIST **p_list, const void *data, size_t size);
void delete_list_element(LIST **p_list, int idx);
void destroy_list(LIST **p_list);
int build_argv(ARGV **p_argv, const char *data);
void delete_argv_element(ARGV **p_argv, int idx);
void destroy_argv(ARGV **p_argv);

/*----------------------------------------------------------------------*/

#endif /* _ARGV_H */
