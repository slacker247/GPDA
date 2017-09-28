/* Written by Dan Heller and Paula Ferguson.  
 * Copyright 1994, O'Reilly & Associates, Inc.
 * Permission to use, copy, and modify this program without
 * restriction is hereby granted, as long as this copyright
 * notice appears in each copy of the program source code.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* file_sel.c -- file selection dialog displays a list of all the writable
 * files in the directory described by the XmNmask of the dialog.
 * This program demonstrates how to use the XmNfileSearchProc for
 * file selection dialog widgets.
 */
#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/FileSB.h>
#include <Xm/DialogS.h>
#include <Xm/PushBG.h>
#include <Xm/PushB.h>
#include <X11/Xos.h>
#include <sys/stat.h>

#include "GR_Interface.H"

char *selected_file;
int   answered_file;

void do_search(Widget widget, XtPointer search_data);
void cancelCB(Widget, XtPointer answer, XtPointer cbs);
void new_file_cb(Widget widget, XtPointer client_data, XtPointer call_data);

/*
 * Routine to determine if a file is accessible, a directory, or a normal file.
 *
 *   Return -1 on all errors.
 *   Return  0 if it's a directory
 *   Return  1 if it's a plain file.
 */
int
is_writable(char *file)
{
struct stat s_buf;

    if (stat (file, &s_buf) == -1)                   /* file can't be accessed (via stat()) */
        return -1;
    else if ((s_buf.st_mode & S_IFMT) == S_IFDIR)    /* a directory */
        return 0;
    else if (!(s_buf.st_mode & S_IFREG)/* || access (file, W_OK) == -1*/ )  /* not a normal file */
        return -1;
    return 1;                                        /* legitimate file */
}

char *
FileSelector(Widget parent)
{
Widget          dialog;
XtAppContext    app;
Arg             args[5];
int             n = 0;
static int      answer;

    answered_file = -1;
    XtSetArg (args[n], XmNfileSearchProc, do_search); n++;
    XtSetArg (args[n], XmNdialogStyle, XmDIALOG_PRIMARY_APPLICATION_MODAL); n++;
    dialog = XmCreateFileSelectionDialog (parent, "Files", args, n);
    XtSetSensitive (
        XmFileSelectionBoxGetChild (dialog, XmDIALOG_HELP_BUTTON), False);
    /* if user presses OK button, call new_file_cb() */
    XtAddCallback(dialog, XmNokCallback,     new_file_cb, &answer);
    XtAddCallback(dialog, XmNcancelCallback, cancelCB,    &answer);
    XtManageChild (dialog);
    XtPopup(XtParent(dialog), XtGrabNone);
    while (answered_file < 0) XtAppProcessEvent(GR_appcontext, XtIMAll);
    XtDestroyWidget(dialog);
    return(selected_file);
}

void
cancelCB(Widget w, XtPointer answer, XtPointer cbs)
{
     answered_file = 0;
     selected_file = NULL;
}

/* a new file was selected -- check to see if it's readable and not
 * a directory.  If it's not readable, report an error.  If it's a
 * directory, scan it just as tho the user had typed it in the mask
 * Text field and selected "Search".
 */
void
new_file_cb(Widget widget, XtPointer answer, XtPointer call_data)
{
char       *file;
char       *dir, *newfile;
XmString   str;

    XmFileSelectionBoxCallbackStruct *cbs = 
        (XmFileSelectionBoxCallbackStruct *) call_data;

    /* get the string typed in the text field in char * format */
    if (!XmStringGetLtoR (cbs->value, XmSTRING_DEFAULT_CHARSET, &file))
        return;
    if (*file != '/') {
        /* if it's not a directory, determine the full pathname
         * of the selection by concatenating it to the "dir" part
         */
        if (XmStringGetLtoR (cbs->dir, XmSTRING_DEFAULT_CHARSET, &dir)) {
            newfile = XtMalloc (strlen (dir) + 1 + strlen (file) + 1);
            sprintf (newfile, "%s/%s", dir, file);
            XtFree( file);
            XtFree (dir);
            file = newfile;
        }
    }
    switch (is_writable (file)) {
        case 0 :
            /* a directory was selected, scan it */
            str = XmStringCreateSimple (file);
            XmFileSelectionDoSearch (widget, str);
            XmStringFree (str);
            break;

        case 1 :
	    /* a normal file was selected */
            selected_file = file;
            answered_file = 1;
            break;

        case -1 :
            /* a system error on this file */
            perror (file);
            XtFree (file);
	    break;
    }
}

/* do_search() -- scan a directory and report only those files that
 * are writable.  Here, we let the shell expand the (possible)
 * wildcards and return a directory listing by using popen().
 * A *real* application should -not- do this; it should use the
 * system's directory routines: opendir(), readdir() and closedir().
 */
void
do_search(Widget widget, XtPointer search_data)
{
char          *mask, buf[BUFSIZ], *p;
XmString      names[256]; /* maximum of 256 files in dir */
int           i = 0;
FILE          *pp; // *popen(const char *command, const char *type);

    XmFileSelectionBoxCallbackStruct *cbs = 
        (XmFileSelectionBoxCallbackStruct *) search_data;

    if (!XmStringGetLtoR (cbs->mask, XmSTRING_DEFAULT_CHARSET, &mask))
        return; /* can't do anything */

    sprintf (buf, "/bin/ls %s", mask);
    XtFree (mask);
    /* let the shell read the directory and expand the filenames */
    if (!(pp = popen (buf, "r")))
        return;
    /* read output from popen() -- this will be the list of files */
    while (fgets (buf, sizeof buf, pp)) {
        if (p = index (buf, '\n'))
            *p = 0;
        /* only list files that are not directories */
        if (is_writable (buf) != 0 &&
            (names[i] = XmStringCreateSimple (buf)))
            i++;
    }
    pclose (pp);
    if (i) {
        XtVaSetValues (widget,
            XmNfileListItems,      names,
            XmNfileListItemCount,  i,
            XmNdirSpec,            names[0],
            XmNlistUpdated,        True,
            NULL);
        while (i > 0)
            XmStringFree (names[--i]);
    } else
        XtVaSetValues (widget,
            XmNfileListItems,      NULL,
            XmNfileListItemCount,  0,
            XmNlistUpdated,        True,
            NULL);
}
