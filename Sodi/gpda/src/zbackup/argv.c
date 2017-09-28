/* Module:	argv.c
**
** Author:	Russel Lane & Associates, Inc.
**
** Description:	This module provides routines for manipulating pointer lists.
**		It is modeled after the "argc" and "argv" arguments to main().
**
**		build_list allocates memory for and saves a copy of an
**		object.  A pointer to the copy is appended to a pointer list
**		(argv) and a count is incremented (argc).  argc and argv are
**		maintained in a structure whose space is also allocated.
**
**		There are two sets of routines:
**			*_list()	Manipulate objects of type LIST, and
**			*_argv()	Manipulate objects of type ARGV.
**
**		There are two data types LIST and ARGV.  They are essentially
**		the same; 
**			ARGV	list of strings, and
**			LIST	list of opaque objects.
**
**		Both data types contain argc and argv members:
**			argc	the number of items in the list.
**			argv	the list of pointers to the objects.
------------------------------------------------------------------------*/

#include <errno.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <string.h>
#include <unistd.h>

#include "argv.h"

/*----------------------------------------------------------------------*/

void error(int err, char *fmt, ...);

/*----------------------------------------------------------------------*/

int build_list(LIST **p_list, const void *data, size_t size)
{
	static char func[] = "build_list";

	LIST *list;

	if (!p_list)
	{
		error(EINVAL, func);
		return(ERROR);
	}

	if (*p_list == NULL)
	{
		/* Get space for the control structure */

		if ((list = malloc(sizeof(LIST))) == NULL)
		{
			error(errno, "%s: malloc ctrl", func);
			return(ERROR);
		}

		*p_list = list;

		list->argc = 0;
		list->argn = 0;

		/* Get space for the initial list */

		list->max = ((size / sizeof(void*)) + (2 * sizeof(void*)))
				/ sizeof(void*);

		if (list->max < 2)
		{
			list->max = 2;
		}

		if ((list->argv = calloc(list->max, sizeof(void*))) == NULL)
		{
			error(errno, "%s: calloc list", func);
			free(list);
			*p_list = NULL;
			return(ERROR);
		}
	}
	else
	{
		/* I'd rather use a single pointer
		** than a double pointer down below.
		*/

		list = *p_list;
	}

	if (list->argc >= list->max - 1)
	{
		/* Out of room, double the size of the list */

		void *tmp;

		list->max <<= 2;

		if ((tmp = realloc((void*) list->argv,
			list->max * sizeof(void*))) == NULL)
		{
			error(errno, "%s: realloc list", func);
			return(ERROR);
		}

		list->argv = tmp;
	}

	/* Get space for the object */

	if ((list->argv[list->argc] = malloc(size)) == NULL)
	{
		error(errno, "%s: malloc object", func);
		return(ERROR);
	}

	memcpy(list->argv[list->argc], data, size);
	list->argc++;
	list->argv[list->argc] = NULL;
	return(OK);
}

/*----------------------------------------------------------------------*/

void delete_list_element(LIST **p_list, int idx)
{
	LIST *list;

	if (!p_list || (list = *p_list) == NULL)
	{
		return;
	}

	if (idx < 0 || idx >= list->argc)
	{
		return;
	}

	free(list->argv[idx]);

	while (++idx < list->argc)
	{
		list->argv[idx - 1] = list->argv[idx];
	}

	list->argv[--list->argc] = NULL;
}

/*----------------------------------------------------------------------*/

void destroy_list(LIST **p_list)
{
	if (p_list)
	{
		LIST *list = *p_list;

		if (list)
		{
			if (list->argv)
			{
				int i;

				for (i = 0 ; i < list->argc ; i++)
				{
					free(list->argv[i]);
				}

				free(list->argv);
			}

			free(list);
		}

		*p_list = NULL;
	}
}

/*----------------------------------------------------------------------*/

int build_argv(ARGV **p_argv, const char *data)
{
	return build_list((LIST**) p_argv, data, data ? strlen(data)+1 : 0);
}

/*----------------------------------------------------------------------*/

void delete_argv_element(ARGV **p_argv, int idx)
{
	delete_list_element((LIST**) p_argv, idx);
}

/*----------------------------------------------------------------------*/

void destroy_argv(ARGV **p_argv)
{
	destroy_list((LIST**) p_argv);
}

/*----------------------------------------------------------------------*/

void error(int err, char *fmt, ...)
{
	va_list args;
	va_start(args, fmt);

	vfprintf(stderr, fmt, args);

	if (err)
	{
		fprintf(stderr, " - %s (%d)", strerror(err), err);
	}

	fputc('\n', stderr);
}

/*----------------------------------------------------------------------*/

#ifdef TESTMAIN

int compare(const void *e1, const void *e2);

int compare(const void *e1, const void *e2)
{
	char *const *s1 = e1;
	char *const *s2 = e2;

	return strcmp(*s1, *s2);
}

void main(void)
{
	char buf [BUFSIZ];
	ARGV *list = NULL;

	while (gets(buf))
	{
		if (build_argv(&list, buf) != OK)
		{
			perror("build_list");
			exit(1);
		}
	}

	if (!list)
	{
		puts("Empty list");
		exit(0);
	}

	qsort(list->argv, list->argc, sizeof(char*),
		(int (*)(const void*, const void *)) compare);

	for (list->argn = 0 ; list->argn < list->argc ; list->argn++)
	{
		puts(list->argv[list->argn]);
	}

	exit(0);
}

#endif /* TESTMAIN */
