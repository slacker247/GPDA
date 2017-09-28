/***********************************************************************
 *
 * Module: textbuf.c
 * Author: Marc van Kempen (marc@bowtie.nl)
 * Desc:   Textbuffer manipulation routines. Implementation of a 
 *	   texteditor
 *
 * Copyright (c) 1996, Marc van Kempen
 *
 * All rights reserved.
 *
 * This software may be used, modified, copied, distributed, and
 * sold, in both source and binary form provided that the above
 * copyright and these terms are retained, verbatim, as the first
 * lines of this file.  Under no circumstances is the author
 * responsible for the proper functioning of this software, nor does
 * the author assume any responsibility for damages incurred with
 * its use.
 *
 ***********************************************************************/

/*
 * Copyright(c) 1997-1998 by Gennady B. Sorokopud (gena@NetVision.net.il)
 *
 * This software can be freely redistributed and modified for 
 * non-commercial purposes as long as above copyright
 * message and this permission notice appear in all
 * copies of distributed source code and included as separate file
 * in binary distribution.
 *
 * Any commercial use of this software requires author's permission.
 *
 * This software is provided "as is" without expressed or implied
 * warranty of any kind.
 * Under no circumstances is the author responsible for the proper
 * functioning of this software, nor does the author assume any
 * responsibility for damages incurred with its use.
 *
 */

/*       $Id: textbuf.c,v 2.12 1998/04/21 13:53:57 gena Exp $
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include "textbuf.h"
#include "fl_error.h"

#ifndef TRUE
#define TRUE 1
#endif
#ifndef FALSE
#define FALSE 0
#endif

#define TB_BUFINC	80	/* to be added space to an existing buffer */
#define	TB_TABLEN	8	/* default tablen */

#define tb_error fl_edit_error

/***********************************************************************
 *
 * Textbuffer manipulation functions 
 *
 ***********************************************************************/

void
tb_init(TextBuf *tb)
/*
 * Desc: initialize the textbuffer structure
 * Pre:  <tb> is a pointer to an existing TextBuf structure
 * Post: <tb> has been initialized
 */
{
    tb->firstline = NULL;
    tb->currentline = NULL;
    tb->lastline = NULL;
    tb->n = 0;
    tb->i = 0;
    tb->bufchanged = FALSE;
    tb->tablen = TB_TABLEN;
    tb->bgcolor_def = 0;
    tb->fgcolor_def = 1;
    tb->linewrap = 0;
    tb->flags = 0;
    tb->maxchars = 0;
    tb->attr_def = ATTR_EMPTY;

    return;
} /* tb_init() */

void
tb_clear(TextBuf *tb)
/*
 * Desc: release the textbuffer, free the used memory by the TextLine 
 * 	 structures.
 */
{
    TextLine *tl, *tlold;

    /* free the memory */
    tl = tb->firstline;
    while (tl) {
	tlold = tl;
	if (tl->buf)
		free(tl->buf);
	if (tl->attr)
		free(tl->attr);
	tl = tl->next;
	free(tlold);
    }

    /* initialize the structure */
    tb->firstline = NULL;
    tb->currentline = NULL;
    tb->lastline = NULL;
    tb->n = 0;
    tb->i = 0;
    tb->bufchanged = FALSE;
    tb->maxchars = 0;

    return;
} /* tb_clear() */


/***********************************************************************
 *
 * Retrieval
 *
 ***********************************************************************/

int
tb_get_char(TextBuf *tb, int n, char *ch)
/*
 * Desc: retrieve a single character from the n-th position in the 
 *	 current line.
 * Pre:  ch is a pointer to a char variable, 
	 n < strlen(currentline)
 * Post: *ch contains the retrieved value
 */
{
    if (n < tb->currentline->strlen) {
	*ch = tb->currentline->buf[n];
	return(TRUE);
    } else {
	return(FALSE);
    }

} /* tb_get_char() */

void
tb_get_line(TextBuf *tb, char **line)
/*
 * Desc: retrieve the current line
 * Pre:  *line is a pointer to a character buffer, not allocated
 * Post: *line points to the line requested
 */
{
    if (tb->currentline) {
	*line = tb->currentline->buf;
    } else {
	*line = NULL;
    }

    return;
} /* tb_get_line() */

char *
tb_return_line(TextBuf *tb)
/*
 * Desc: returns pointer to the text of current line
 */
{
    return tb->currentline ? tb->currentline->buf : NULL;
} /* tb_return_line() */

TextLine *
tb_get_lineptr_by_num(TextBuf *tb, int n)
/*
 * Desc: returns pointer to the line with number <n>
 */
{
   int i;
   TextLine *tl;

   i = 0;
   tl = tb->firstline;
   while (tl && i<n) {
       tl = tl->next;
       i++;
   }
   if (tl && i==n) {
       /* we've got the right line */
       return tl;
   } else {
       /* the line does not exist */
       return NULL;
   }

   return NULL;
}

void
tb_get_line_by_num(TextBuf *tb, char **line, int n)
/*
 * Desc: retrieve the line with number <n>
 * Pre:  *line is a pointer to a character buffer, not allocated
 * Post: *line points to the line requested
 */
{
   int i;
   TextLine *tl;

   i = 0;
   tl = tb->firstline;
   while (tl && i<n) {
       tl = tl->next;
       i++;
   }
   if (tl && i==n) {
       /* we've got the right line */
       *line = tl->buf;
       return;
   } else {
       /* the line does not exist */
       *line = NULL;
       return;
   }
}

void 
tb_set_block_attr(TextBuf *tb, int r0, int c0, int r1, int c1, int attr)
/*
 * Desc: set attrbutes of specific block
 */	 
{
int i, k, rtop, rbottom;
struct TextLine *tl;

    if (r0 > r1)  {
     rtop = r0;
     rbottom = r1; }
    else	{
     rtop = r1;
     rbottom = r0;
		}

    if (rtop == rbottom)	{
     if ((tl = tb_get_lineptr_by_num(tb, rtop)) == NULL)
	return;

     if (c0 < 0)
	c0 = tl->strlen;
     if (c1 < 0)
	c1 = tl->strlen;

     if (c1 < c0)	{
	i = c0;
	c0 = c1;
	c1 = i;		}

     if (c1 > tl->strlen)
	c1 = tl->strlen;

     if (c0 > tl->strlen)
	 c0 = tl->strlen;

     if (c0 == c1)
	return;

     for (i = c0; i < c1; i++)
	tl->attr[i] = attr;

     tl->flags |= TLINE_MODIFIED;

     return;			}

    for (i = rbottom; i <= rtop; i++)	{
     if ((tl = tb_get_lineptr_by_num(tb, i)) == NULL)
	continue;

     if (i == rbottom)	{
        if (c0 >= tl->strlen)
		continue;
	for (k = c0; k < tl->strlen; k++)
		tl->attr[k] = attr;
			}
     else
     if (i == rtop)	{
	if ((c1 >= tl->strlen) || (c1 < 0))
		c1 = tl->strlen;
	for (k = 0; k < c1; k++)
		tl->attr[k] = attr;
			}
     else	{
	for (k = 0; k < tl->strlen; k++)
		tl->attr[k] = attr;
		}

     tl->flags |= TLINE_MODIFIED;
					}

    return;
}

void 
tb_get_block(TextBuf *tb, int r0, int c0, int r1, int c1, char **buf)
/*
 * Desc: retrieve a block of characters from the TextBuf <tb> and copy
 *	 it into a to be allocated buffer <buf>
 */	 
{
int i, addcr, rtop, rbottom, buflen, llen;
char *block, *line;

    if (r0 > r1)  {
     rtop = r0;
     rbottom = r1; }
    else	{
     rtop = r1;
     rbottom = r0;
		}

    *buf = NULL;
    block = (char *)malloc(1);
    block[0] = '\0';
    buflen = 1;
    addcr = 0;

    if (rtop == rbottom)	{
	tb_get_line_by_num(tb, &line, rtop);
	if (line == NULL)
		return;
	llen = strlen(line);
	if (c0 < 0)
	 c0 = llen;
	if (c1 < 0)
	 c1 = llen;

	if (c1 < c0)	{
	 i = c0;
	 c0 = c1;
	 c1 = i;	}

	if (c1 > llen)
	 c1 = llen;

	if (c0 > llen)
	 c0 = llen;

	if (c0 == c1)
		return;

	buflen += (c1 - c0);
	if (c0 == 0) {
	 addcr = 1;
	 buflen++;   }

	block = (char *)realloc(block, buflen);
	strncat(block, line + c0, c1 - c0);
	if (addcr)
	 strcat(block, "\n");
	*buf = block;
	return;
				}

    for (i = rbottom; i <= rtop; i++)	{
	tb_get_line_by_num(tb, &line, i);
	if (line == NULL)
		continue;

	if (i == rbottom)	{
	  if (c0 >= strlen(line))
		continue;
	  buflen += strlen(line) - c0;
	  if (i < rtop)
		buflen++;
	  block = (char *)realloc(block, buflen);
	  strcat(block, line + c0);
	  if (i < rtop)
	  	strcat(block, "\n");
			}
	else
	if (i == rtop) {
	  if ((c1 >= strlen(line)) || (c1 < 0))	{
		c1 = strlen(line) + 1;
		addcr = 1;			}
	  buflen += c1;
	  block = (char *)realloc(block, buflen);
	  strncat(block, line, c1);
	  if (addcr)
	   strcat(block, "\n");
			  }
	else	{
	  buflen += (strlen(line) + 1);
	  block = (char *)realloc(block, buflen);
	  strcat(block, line);
	  strcat(block, "\n");
		}
					}

    *buf = block;

    return;
} /* tb_get() */

void
tb_get_rect(TextBuf *tb, int r0, int c0, int r1, int c1, char **buf)
/*
 * Desc: retrieve a rectangular block of characters from the TextBuf <tb>
 *	 and copy it into a newly allocated buffer <buf>
 */
{

    return;
} /* tb_get_rect() */


/***********************************************************************
 *
 * Insertion
 *
 ***********************************************************************/

int
tb_insert_char(TextBuf *tb, int n, char ch)
/*
 * Desc: insert the character <ch> into <tb> at the n-th position 
 *	 in the current line.
 * returns number of characters added or minus this number if the line
 * was wrapped
 */
{
    TextLine *tl;
    int i, added;
    char *newbuf;

    /* get the current TextLine */
    tl = tb->currentline;

    /* check if there is a current line at all */
    if (!tb->currentline) {
	/* allocate a line */
	tb_insert_line(tb, "");
	tl = tb->currentline;
    }

    /* check if the character can be fitted in the buffer */
    tl->flags |= TLINE_MODIFIED;
    if (tl->strlen+1 >= tl->buflen) {
	/* reallocate the buffer space and add a fixed amount of space 
	   to it */
	newbuf = realloc(tl->buf, tl->buflen+TB_BUFINC);
	if (!newbuf) {
	    tb_error("tb_insert_char(): Could not realloc, character not inserted");
	    return 0;
	}
	tl->buf = newbuf;

	newbuf = realloc(tl->attr, tl->buflen+TB_BUFINC);
	if (!newbuf) {
	    tb_error("tb_insert_char(): Could not realloc attr, character not inserted");
	    return 0;
	}
	tl->attr = newbuf;

	tl->buflen = tl->buflen + TB_BUFINC;
    }

    /* first move the charachter after n one position */
    for (i=tl->strlen; i>=n; i--) {
	tl->buf[i+1] = tl->buf[i];
	tl->attr[i+1] = tl->attr[i];
    }
    /* now insert the character */
    tl->buf[n] = ch;
    tl->attr[n] = tb->attr_def;
    
    /* adjust strlen */
    tl->strlen++;
    tb->bufchanged = TRUE;

    if (ch == '\t')
	added = tb_handle_tabs(tb);
    else
	added = 1;
    i = tb_reformat(tb);
    tb_fix_line(tl);

    return i ? -1*added : added;

} /* tb_insert_char() */

void
tb_insert_line(TextBuf *tb, char *line)
/*
 * Desc: insert the line <line> into <tb>
 */
{
    TextLine *tlnew;
    int i;

    /* Allocate a new TextLine buffer */
    tlnew = (TextLine *) malloc( sizeof(TextLine) );
    if (!tlnew) {
	tb_error("tb_insert_line(): could not malloc textline");
	return;
    }
    /* allocate a new line buffer and copy the line into it */
    tlnew->buf = (char *) malloc(strlen(line) + 1);
    if (!tlnew->buf) {
	tb_error("tb_insert_line(): could not malloc line buffer");
	free(tlnew);
	return;
    }
    strcpy(tlnew->buf, line);
    tlnew->strlen = strlen(line);

    tlnew->attr = (char *) malloc(tlnew->strlen + 1);
    if (!tlnew->attr) {
	tb_error("tb_insert_line(): could not malloc line attr buffer");
	free(tlnew->buf);
	free(tlnew);
	return;
    }
    for (i = 0; i < tlnew->strlen; i++)
	tlnew->attr[i] = tb->attr_def;
    tlnew->attr[tlnew->strlen] = '\0';

    tlnew->buflen = strlen(line)+1;
    tlnew->bgcolor = tb->bgcolor_def;
    tlnew->fgcolor = tb->fgcolor_def;
    tlnew->cont = NULL;
    tlnew->flags = TLINE_MODIFIED;
    tlnew->clk_callback = NULL;
    tlnew->dbl_callback = NULL;
    tlnew->cur_callback = NULL;
    tlnew->callback_data = 0;

    /* check if there are any lines */
    if (tb->currentline) {
	/* insert the new TextLine in the linked list */
	tlnew->prev = tb->currentline->prev;
	tlnew->next = tb->currentline;
	if (tb->currentline->prev)
	 tb->currentline->prev->next = tlnew;
	else
	 tb->firstline = tlnew;
	tb->currentline->prev = tlnew;
	tb->currentline = tlnew;
    } else {
	tlnew->prev = NULL;
	tlnew->next = NULL;
	tb->firstline = tlnew;
	tb->currentline = tlnew;
	tb->lastline = tlnew;
    }
    if (tlnew->prev)
	tlnew->prev->cont = NULL;
    tb_fix_line(tlnew);

    while (tlnew) {
	tlnew->flags |= TLINE_MODIFIED;
	tlnew = tlnew->next;
		  }

    tb->n++;
    tb->bufchanged = TRUE;
    
    tb_handle_tabs(tb);
    tb_reformat(tb);
    return;
} /* tb_insert_line() */

void 
tb_insert_block(TextBuf *tb, int r, int c, char *buf)
/*
 * Desc: insert the buffer <buf> into <tb> at the position (r,c)
 */
{
    TextLine *tl, *tlold;
    int i, len, oneline, oldwrap;
    char *newbuf, *p;

    /* store the current line */
    tlold = tb->currentline;

    /* no lines yet. insert as a new line(s) */
    if ((tlold == NULL) && (r == 0))	{
     /* insert one line */
     if ((p = strchr(buf, '\n')) == NULL)
	tb_insert_line(tb, buf);
     else	{
      /* append multiple lines */
      do {
	tb_append_buf(tb, buf, p - buf);
	buf = ++p;
	 } while ((p = strchr(buf, '\n')) != NULL);
      tb_append_line(tb, buf);
		}

     tb->bufchanged = TRUE;
     return;
					}

    /* check if line <r> exists */
    if (tb_set_current_line(tb, r) == FALSE)
	return;

    /* check if position is valid */
    if ((c > tb->currentline->strlen) || (c < 0))
	c = tb->currentline->strlen;

    tl = tb->currentline;
    tl->flags |= TLINE_MODIFIED;

    /* check if we inserting one-line buffer */
    if ((p = strchr(buf, '\n')) == NULL) {
	len = strlen(buf);
	oneline = 1;			 }
    else	{
	len = p - buf;
	oneline = 0;
		}

    /* check if the buffer can be fitted in string's buffer */
    if (tl->strlen+len >= tl->buflen) {
	/* reallocate the buffer space and add needed amount of space to it */
	newbuf = realloc(tl->buf, tl->buflen+len+1);
	if (!newbuf) {
	    tb_error("tb_insert_buf(): Could not realloc, character not inserted");
	    tb->currentline = tlold;
	    return;
	}
	tl->buf = newbuf;

	newbuf = realloc(tl->attr, tl->buflen+len+1);
	if (!newbuf) {
	    tb_error("tb_insert_buf(): Could not realloc attr, character not inserted");
	    tb->currentline = tlold;
	    return;
	}
	tl->attr = newbuf;

	tl->buflen = tl->buflen + len + 1;
    				      }
    /* store part of the string after <c> */
    newbuf = strdup(tl->buf + c);
    tl->buf[c] = '\0';
    /* add buf */
    strncat(tl->buf, buf, len);
    /* add remaining part of the string */
    strcat(tl->buf, newbuf);
    tl->strlen += len;
    free(newbuf);

    newbuf = strdup(tl->attr + c);
    tl->attr[c] = '\0';
    for (i = c; i < len + c; i++)
	tl->attr[i] = tb->attr_def;
    tl->attr[len + c] = '\0';
    strcat(tl->attr, newbuf);
    free(newbuf);

    tb_handle_tabs(tb);
    if (oneline) {
     /* wrap the line if needed */
     tb_reformat(tb);
     /* restore currentline */
     tb->currentline = tlold;
     tb->bufchanged = TRUE;
     return;     }

    /* insert newline at the end of inserted text */
    tb_insert_cr(tb, c + len);
    /* start inserting new lines at the next line */
    tb_set_next_line(tb);

    /* temporary disable line/word wrapping */
    oldwrap = tb->linewrap;
    tb->linewrap = 0;

    /* insert multiple lines */
    buf = ++p;
    while ((p = strchr(buf, '\n')) != NULL)	{
	len = p - buf;
	newbuf = (char *)malloc(len + 1);
	strncpy(newbuf, buf, len);
	newbuf[len] = '\0';
	tb_insert_line(tb, newbuf);
	free(newbuf);
	tb_set_next_line(tb);
	buf = ++p;				}

    /* the last line should be inserted at the beginning of the next one */
    tl = tb->currentline;
    tl->flags |= TLINE_MODIFIED;
    if (*buf && tl)	{
     len = strlen(buf);
     /* check if the buffer can be fitted in string's buffer */
     if (tl->strlen+len >= tl->buflen) {
	/* reallocate the buffer space and add needed amount of space to it */
	newbuf = realloc(tl->buf, tl->buflen+len+1);
	if (!newbuf) 	{
	    tb_error("tb_insert_buf(): Could not realloc, character not inserted");
	    tb->currentline = tlold;
	    return;
			}
        tl->buf = newbuf;

	newbuf = realloc(tl->attr, tl->buflen+len+1);
	if (!newbuf) 	{
	    tb_error("tb_insert_buf(): Could not realloc attr, character not inserted");
	    tb->currentline = tlold;
	    return;
			}
        tl->attr = newbuf;

        tl->buflen = tl->buflen + len + 1;
    				      }
     tl->strlen += len;
     /* store part of the string after <c> */
     newbuf = strdup(tl->buf);
     /* add buf */
     strcpy(tl->buf, buf);
     /* add remaining part of the string */
     strcat(tl->buf, newbuf);
     free(newbuf);

     newbuf = strdup(tl->attr);
     for (i = 0; i < len; i++)
	tl->attr[i] = tb->attr_def;
     tl->attr[len] = '\0';
     strcat(tl->attr, newbuf);
     free(newbuf);

     tb_handle_tabs(tb);
			}

    /* restore wrap value */
    tb->linewrap = oldwrap;
    /* wrap the whole buffer if needed */
    tb_wrap_lines(tb);
    tb_reformat(tb);

    /* restore currentline */
    tb->currentline = tlold;
    tb->bufchanged = TRUE;

    return;
} /* tb_insert() */

void 
tb_insert_rect(TextBuf *tb, int r, int c, int w, int h, char *buf)
/*
 * Desc: insert the block <buf> into <tb> at the position (r,c)
 * Pre:  <buf> has dimensions (w,h)
 */
{
    tb->bufchanged = TRUE;

    return;
} /* tb_insert_rect() */

void
tb_insert_cr(TextBuf *tb, int c)
/*
 * Desc: at position <c> in the current line insert a newline character
 *	  and move the remainder of the line one line down.
 */
{
    char *newline;
    TextLine *tl;

    if (!tb->currentline) {
	/* we must have a virgin textbuffer!, just append two lines */
	tb_append_line(tb, "");
	tb_append_line(tb, "");
	return;		  }

    /* check to see if c is valid */
    if ((c < 0) || (c > tb->currentline->strlen))
	return;
    
    if (c < tb->currentline->strlen) {
	/* get the line from the c-th position */
	newline = strdup(tb->currentline->buf + c);
	if (!newline) {
	    tb_error("tb_insert_newline(): Could not allocate space for newline");
	    return;   }
	tb->currentline->buf[c] = '\0';
	tb->currentline->strlen = c;
	tb->currentline->flags |= TLINE_MODIFIED;
	if (tb->currentline == tb->lastline)
	    tb_append_line(tb, newline);
	else {
	    /* insert a line at the next line */
	    tb_set_next_line(tb);
	    tb_insert_line(tb, newline);
	    tb_set_prev_line(tb);
	     }
	free(newline);

	newline = strdup(tb->currentline->attr + c);
	if (!newline) {
	    tb_error("tb_insert_newline(): Could not allocate space for newline attr");
	    return;   }
	tb->currentline->attr[c] = '\0';
	if (tb->currentline->next)
		strcpy(tb->currentline->next->attr, newline);
	free(newline);
    } else {
	/* we are at the end of the line, c == tb->currentline->strlen */
	if (tb->currentline == tb->lastline) {
	    tb_append_line(tb, "");
	} else {
	    tb_set_next_line(tb);
	    tb_insert_line(tb, "");
	    tb_set_prev_line(tb);
	}
    }

    tb->currentline->cont = NULL;
    tl = tb->currentline;
    while (tl) {
	tl->flags |= TLINE_MODIFIED;
	tl = tl->next;
		}

    return;
} /* tb_insert_newline() */


/***********************************************************************
 *
 * Appending
 *
 ***********************************************************************/

void
tb_append_line(TextBuf *tb, char *line)
/*
 * Desc: Append the <line> to <tb>
 */
{
    TextLine *tlnew, *tlcur;
    int i;

    /* allocate a new TextLine */
    tlnew = (TextLine *)malloc(sizeof(TextLine));
    if (!tlnew) {
	tb_error("tb_append_line(): Could not allocate TextLine");
	return; }
    /* Allocate the buffer in TextLine and copy the line into it */
    tlnew->buf = strdup(line ? line : "");
    if (!tlnew->buf) {
	tb_error("tb_append_line(): Could not allocate line buffer");
	free(tlnew);
	return;      }

    tlnew->attr = strdup(line ? line : "");
    if (!tlnew->attr) {
	tb_error("tb_append_line(): Could not allocate line attr buffer");
	free(tlnew->buf);
	free(tlnew);
	return;      }

    tlnew->strlen = strlen(tlnew->buf);
    for (i = 0; i < tlnew->strlen; i++)
	tlnew->attr[i] = tb->attr_def;

    tlnew->buflen = tlnew->strlen + 1;
    tlnew->bgcolor = tb->bgcolor_def;
    tlnew->fgcolor = tb->fgcolor_def;
    tlnew->cont = NULL;
    tlnew->flags = TLINE_MODIFIED;
    tlnew->clk_callback = NULL;
    tlnew->dbl_callback = NULL;
    tlnew->cur_callback = NULL;
    tlnew->callback_data = 0;
    tb_fix_line(tlnew);

    /* check if there are any lines at all yet */
    if (tb->lastline) {
	tlnew->next = NULL;
	tlnew->prev = tb->lastline;
	
	tb->lastline->next = tlnew;
	tb->lastline = tlnew;
    } else {
	tlnew->next = NULL;
	tlnew->prev = NULL;
	tb->firstline = tlnew;
	tb->currentline = tlnew;
	tb->lastline = tlnew;
    }
    tb->n++;
    tb->bufchanged = TRUE;

    if (tlnew->prev)
	tlnew->prev->cont = NULL;

    tlcur = tb->currentline;
    tb->currentline = tlnew;
    tb_handle_tabs(tb);
    tb_reformat(tb);
    tb->currentline = tlcur;

    return;
} /* tb_append_line() */

void
tb_append_buf(TextBuf *tb, char *buf, u_long len)
/*
 * Desc: Append the <buf> with length <len> to <tb>
 */
{
    TextLine *tlnew, *tlcur;
    int i;

    /* allocate a new TextLine */
    tlnew = (TextLine *) malloc( sizeof(TextLine) );
    if (!tlnew) {
	tb_error("tb_append_buf(): Could not allocate TextLine");
	return; }
    /* Allocate the buffer in TextLine and copy the line into it */
    tlnew->buf = (char *)malloc(len + 1);
    if (!tlnew->buf) {
	tb_error("tb_append_buf(): Could not allocate line buffer");
	free(tlnew);
	return;      }
    tlnew->attr = (char *)malloc(len + 1);
    if (!tlnew->attr) {
	tb_error("tb_append_buf(): Could not allocate line attr buffer");
	free(tlnew->buf);
	free(tlnew);
	return;      }

    for (i = 0; i < len; i++)
	tlnew->attr[i] = tb->attr_def;

    strncpy(tlnew->buf, buf, len);
    tlnew->buf[len] = '\0';
    tlnew->attr[len] = '\0';
    tlnew->buflen = len+1;
    tlnew->strlen = len;
    tlnew->bgcolor = tb->bgcolor_def;
    tlnew->fgcolor = tb->fgcolor_def;
    tlnew->cont = NULL;
    tlnew->flags = TLINE_MODIFIED;
    tlnew->clk_callback = NULL;
    tlnew->dbl_callback = NULL;
    tlnew->cur_callback = NULL;
    tlnew->callback_data = 0;
    tb_fix_line(tlnew);

    /* check if there are any lines at all yet */
    if (tb->lastline) {
	tlnew->next = NULL;
	tlnew->prev = tb->lastline;
	tb->lastline->next = tlnew;
	tb->lastline = tlnew;
    } else {
	tlnew->next = NULL;
	tlnew->prev = NULL;
	tb->firstline = tlnew;
	tb->currentline = tlnew;
	tb->lastline = tlnew;
    }
    tb->n++;
    tb->bufchanged = TRUE;

    if (tlnew->prev)
	tlnew->prev->cont = NULL;

    tlcur = tb->currentline;
    tb->currentline = tlnew;
    tb_handle_tabs(tb);
    tb_reformat(tb);
    tb->currentline = tlcur;

    return;
} /* tb_append_buf() */

void
tb_append_to_line(TextBuf *tb, char *line)
/*
 * Desc: Append the <line> to the current line
 */
{
TextLine *tl, *tlcur;
char *newbuf;
int incr, i;

  if (!tb->currentline)
	return;

  tl = tb->currentline;
  tl->flags |= TLINE_MODIFIED;
  if (tl->strlen + strlen(line) + 1 >= tl->buflen) {
	/* reallocate the buffer space and add needed amount of space to it */
	incr = (strlen(line) + 1 > TB_BUFINC) ? (strlen(line) + 1) : TB_BUFINC;
	newbuf = realloc(tl->buf, tl->buflen + incr);
	if (!newbuf) {
	    tb_error("tb_append_to_line(): Could not realloc, buffer not appended");
	    return;
	}
	tl->buf = newbuf;

	newbuf = realloc(tl->attr, tl->buflen + incr);
	if (!newbuf) {
	    tb_error("tb_append_to_line(): Could not realloc attr, buffer not appended");
	    return;
	}
	tl->attr = newbuf;

	tl->buflen += incr;
    						  }

  for (i = tl->strlen; i < tl->strlen + strlen(line); i++)
	tl->attr[i] = '\0';

  strcat(tl->buf, line);
  tl->strlen = strlen(tl->buf);
  tl->attr[tl->strlen] = '\0';

  tlcur = tb->currentline;
  tb_fix_line(tl);
  tb_handle_tabs(tb);
  tb_reformat(tb);
  tb->currentline = tlcur;

  return;
}

int
tb_wrap_line(TextBuf *tb)
/*
 * Desc: wrap current line if needed
 */
{
char *p, *nextline, *nextattr, *newbuf, c;
TextLine *tlwrap;
int nlen, i, linewrap = tb->linewrap;

    tlwrap = tb->currentline;
    if (!tlwrap || (linewrap == 0))	{ /* is wrapping on? */
	if (tlwrap->strlen > tb->maxchars)
		tb->maxchars = tlwrap->strlen;
	return 0;			}

    /* check if line should be wrapped */
    if (tlwrap->strlen <= abs(linewrap))	{
	if (tlwrap->strlen > tb->maxchars)
		tb->maxchars = tlwrap->strlen;
	return 0;				}

    tlwrap->strlen = abs(linewrap);
    nextline = tlwrap->buf + tlwrap->strlen;

    if (linewrap < 0)	{	/* word wrap */
	c = *nextline;
	*nextline = '\0';
	if ((p = strrchr(tlwrap->buf, ' ')) != NULL)	{
	 *nextline = c;
	tlwrap->strlen = p - tlwrap->buf;
	nextline = ++p;					}
	else	{
	 /* can not word wrap - wrap as usual */
	 *nextline = c;
	 linewrap = abs(linewrap);
		}
	  		}
    nlen = strlen(nextline);
    nextattr  = tlwrap->attr + tlwrap->strlen;

    /* if the line was already wrapped then add the rest */
    /* at the beginning of the next line */
    if (tlwrap->cont && (tlwrap->cont == tlwrap->next)) {
    /* if it's a word wrapping don't forget about the extra space */
    if (linewrap < 0)
	nlen++;
     tlwrap->flags |= TLINE_MODIFIED;
    /* check if the nextline can be fitted in the buffer */
     if (tlwrap->next->strlen + nlen >= tlwrap->next->buflen)  {
	/* reallocate the buffer space and add a fixed amount of space 
	   to it */
	newbuf = realloc(tlwrap->next->buf, tlwrap->next->buflen+nlen+1);
	if (!newbuf) {
	    tb_error("tb_wrap_line(): Could not realloc, character not inserted");
	    return 0;
	}
	tlwrap->next->buf = newbuf;
	newbuf = realloc(tlwrap->next->attr, tlwrap->next->buflen+nlen+1);
	if (!newbuf) {
	    tb_error("tb_wrap_line(): Could not realloc attr, character not inserted");
	    return 0;
	}
	tlwrap->next->attr = newbuf;

	tlwrap->next->buflen = tlwrap->next->buflen + nlen + 1;
    								}

     /* shift the line <nlen> places right */
     for (i=tlwrap->next->strlen; i >= 0; i--)	{
	tlwrap->next->buf[i+nlen] = tlwrap->next->buf[i];
	tlwrap->next->attr[i+nlen] = tlwrap->next->attr[i];
						}
     /* insert the newline */
     for (i = 0; i < nlen; i++)	{
	tlwrap->next->buf[i] = nextline[i];
	tlwrap->next->attr[i] = nextattr[i];
				}
     /* add extra space */
     if (linewrap < 0)
	tlwrap->next->buf[nlen-1] = ' ';
     /* adjust the string length */
     tlwrap->next->strlen += nlen;
							}
    else {
    /* otherwise add the rest of the line as a new line */
    if (tb->currentline == tb->lastline) {
    /* append a line */
      tb_append_line(tb, nextline);
    } else {
    /* insert a line at the next line */
      tb_set_next_line(tb);
      tb_insert_line(tb, nextline);
      tb_set_prev_line(tb);
	   }
    /* mark that the new line is a continuation of the current line */
    tlwrap->cont = tlwrap->next;

    /* new line has the same colors as current */
    tlwrap->next->bgcolor = tlwrap->bgcolor;
    tlwrap->next->fgcolor = tlwrap->fgcolor;
	}

    /* terminate the wrapped line */
    tlwrap->buf[tlwrap->strlen] = '\0';
    tlwrap->attr[tlwrap->strlen] = '\0';

    if (tlwrap->strlen > tb->maxchars)
	tb->maxchars = tlwrap->strlen;
    while (tlwrap) {
	tlwrap->flags |= TLINE_MODIFIED;
	tlwrap = tlwrap->next;
		   }

    /* check if new (next) line also needs wrapping */
    tb_set_next_line(tb);
    tb_wrap_line(tb);
    tb_set_prev_line(tb);

    return 1;
} /* tb_wrap_line() */

int
tb_reformat(TextBuf *tb)
{
static int formatting;
int i, pstart, pend;


    /* reformat paragraph if needed */
    if ((i = tb_wrap_line(tb)) == 1) {
     if (tb->flags & TEXT_AUTOFORMAT)	{
      if (formatting)
	return i;
      formatting = 1;
      tb_get_paragraph(tb, &pstart, &pend); 
      tb_fill_region(tb, tb->i, 0, pend, -1);
      formatting = 0;
					}
				     }

    return i;
}

void
tb_wrap_lines(TextBuf *tb)
/*
 * Desc: wrap all lines in buffer
 */
{
  if (tb_get_nlines(tb) > 0) {
        tb_set_current_line(tb, 0);
        tb_wrap_line(tb);
        while (tb_set_next_line(tb))
         tb_wrap_line(tb);
			     }
}

void
tb_fill_region(TextBuf *tb, int r0, int c0, int r1, int c1)
/*
 * Desc: fill specified region with text
 */
{
char *buf, *p;

 /* only meaningfull if word wrap is set */
 if (tb->linewrap >= 0)
	return;

 /* get the text */
 tb_get_block(tb, r0, c0, r1, c1, &buf);
 if (buf == NULL)
	return;

 /* don't process empty lines */
 if (strlen(buf) <= 1)	{
	free(buf);
	return;		}

 /* remove all CRs except the last one */
 p = buf;
 while ((p = strchr(p, '\n')) != NULL) 	{
  if (*(p + 1) != '\0')
   *p = ' ';
  else
    break;				}

 /* nothing to wrap here */
 if (!strchr(buf, ' '))	{
	free(buf);
	return;		}

 /* delete block */
 if (tb_del_block(tb, r0, c0, r1, c1) == FALSE) {
	free(buf);
	return;					}

 if ((tb_set_current_line(tb, r0) == FALSE) && (r0 > 0)) {
	tb_set_current_line(tb, r0 - 1);
	tb_append_line(tb, buf);			 }
 else
	tb_insert_block(tb, r0, c0, buf);

 free(buf);

 return;
}

void
tb_get_paragraph(TextBuf *tb, int *pstart, int *pend)
/*
 * Desc: returns start/end lines number of paragraph around current line
 */
{
TextLine *tl;
int i;

 tl = tb->currentline;
 i = tb->i;

 if (tl->strlen == 0) {
	*pstart = *pend = i;
	return;	      }

 while (tl->prev && (tl->prev->strlen > 0) && (i > 0))	{
	i--;
	tl = tl->prev;					}

 *pstart = i;

 tl = tb->currentline;
 i = tb->i;

 while (tl->next && (tl->next->strlen > 0))	{
	i++;
	tl = tl->next;				}

 *pend = i;

 return;
}

void
tb_modify_lines(TextBuf *tb)
/*
 * Desc: mark all lines below current as modified
 */
{
TextLine *tl;

 tl = tb->currentline;
 while (tl) {
  tl->flags |= TLINE_MODIFIED;
  tl = tl->next;
	    }
}

/***********************************************************************
 *
 * Deletion
 *
 ***********************************************************************/

int
tb_del_char(TextBuf *tb, int n)
/*
 * Desc: delete the character at the n-th position, returns TRUE
 *	  when the screen needs redrawing
 */
{
    int i, l;
    char *line, *newbuf;
    char *nline, *nattr;
    TextLine *tl;

    if (n > tb_get_linelen(tb))
	return(FALSE);

    /* get the current line */
    tb_get_line(tb, &line);
    if (!line)
	return(FALSE);

    tb->bufchanged = TRUE;
    if (n == tb_get_linelen(tb)) {
	/* we are at eol, join the current line with the next one */
	if (tb_set_next_line(tb))	{
	    tb_get_line(tb, &nline);
	    nattr = tb->currentline->attr;
	    tb_set_prev_line(tb);	}
	else
	    return FALSE;

	l = strlen(nline);
	tl = tb->currentline;
	tl->flags |= TLINE_MODIFIED;

	/* check if the next line can be fitted in the buffer */
	if (tl->strlen + l >= tl->buflen) {
	/* reallocate the buffer space and add a fixed amount of space 
	   to it */
	l = tl->strlen + l + TB_BUFINC;
	newbuf = realloc(tl->buf, l);
	if (!newbuf) {
	    tb_error("tb_del_char(): Could not realloc, character not deleted");
	    return(FALSE);
	}
	tl->buf = newbuf;

	newbuf = realloc(tl->attr, l);
	if (!newbuf) {
	    tb_error("tb_del_char(): Could not realloc attr, character not deleted");
	    return(FALSE);
	}
	tl->attr = newbuf;
	tl->buflen = l;
					  }
	strcat(tl->buf, nline);
	strcat(tl->attr, nattr);
	tl->strlen = strlen(tl->buf);

	/* delete the next line */
	tb_set_next_line(tb);
	tb_del_line(tb);

	/* restore the current line */

	tb->currentline = tl;
	/* check if current line needs wrapping */
	tb_reformat(tb);

	return(TRUE);
    }

    /* check if the line has any characters in it */
    if (tb->currentline->strlen > 0) {
	tb->currentline->flags |= TLINE_MODIFIED;
	for (i = n; i < tb->currentline->strlen; i++) {
	    tb->currentline->buf[i] = tb->currentline->buf[i + 1];
	    tb->currentline->attr[i] = tb->currentline->attr[i + 1];
	}
	/* adjust strlen */
	tb->currentline->strlen--;
    }

    return(FALSE);
} /* tb_del_char() */

void
tb_del_line(TextBuf *tb)
/*
 * Desc: delete the current line 
 */
{
    TextLine *tl = tb->currentline;

    if (!tl) {
	/* no lines to remove */
	return;
    }

    tb->bufchanged = TRUE;
    /* check if we are in the middle or the first line of the text */
    if (tl->prev) {
	tl->prev->next = tl->next;
	/* if there is a next line make that the current one, otherwise
	   the previous one */
	if (tl->next) {
	    tl->next->prev = tl->prev;
	    tb->currentline = tl->next;
	    /* tb->i stays the same */
	} else {
	    /* the current line is the last one in the buffer */
	    tb->currentline = tl->prev;
	    if (tb->i > 0)
		tb->i--;
	    /* adjust the last line, we are about to delete it! */
	    tb->lastline = tl->prev;
	}
    } else {
	/* we are at the first line */
	/* see also above */
	if (tl->next) {
	    tl->next->prev = NULL;
	    tb->currentline = tl->next;
	    /* adjust the first line, we are about to delete it */
	    tb->firstline = tl->next;
	    /* tb->i stays the same */
	} else {
	    /* this must the only line in the buffer */
	    tb->firstline = NULL;
	    tb->currentline = NULL;
	    tb->lastline = NULL;
	    if (tb->i > 0)
		tb->i--;
	}
    }
    free(tl->buf);
    free(tl->attr);
    free(tl);

    tl = tb->currentline;
    while (tl) 	{
	tl->flags |= TLINE_MODIFIED;
	tl = tl->next;
		}

    tb->n--;

    return;
} /* tb_del_line() */

int
tb_del_block(TextBuf *tb, int r0, int c0, int r1, int c1)
/*
 * Desc: delete the block at the specified coordinates
 */
{
int i, k, rtop, rbottom, sellen;

    if (r0 > r1)  {
     rtop = r0;
     rbottom = 1; }
    else	{
     rtop = r1;
     rbottom = r0;
		}

    tb->bufchanged = TRUE;

    if (rtop == rbottom)	{
	tb_set_current_line(tb, rtop);
	if (!tb->currentline)
		return FALSE;

	if (c1 < 0)
		c1 = tb->currentline->strlen;
	if (c0 < 0)
		c0 = 0;
	if (c0 > c1)    {
		i = c1;
		c1 = c0;
		c0 = i; }
	if (c1 == c0)
		return TRUE;

	if ((c0 == 0) && (c1 >= tb->currentline->strlen))	{
		tb_del_line(tb);
		return TRUE;
								}

	sellen = c1 - c0;
	for (i = c1; i <= tb->currentline->strlen; i++)	{
		tb->currentline->buf[i - sellen] = tb->currentline->buf[i];
		tb->currentline->attr[i - sellen] = tb->currentline->attr[i];
							}

	tb->currentline->strlen -= sellen;
	tb->currentline->flags |= TLINE_MODIFIED;
	return TRUE;
				}

    for (i = rbottom; i <= rtop; i++)	{
	tb_set_current_line(tb, i);
	if (!tb->currentline)
		continue;

	if (i == rtop)		{
	   if ((c1 < 0) || (c1 >= tb->currentline->strlen))	{
		tb_del_line(tb);
		i--;
		rtop--;
		if (rtop < rbottom)
			break;
								}
	   else	{
		for (k = c1; k <= tb->currentline->strlen; k++)	{
			tb->currentline->buf[k - c1] = tb->currentline->buf[k];
			tb->currentline->attr[k - c1] = tb->currentline->attr[k];
								}
		tb->currentline->strlen -= c1;
		tb->currentline->flags |= TLINE_MODIFIED;
		}
				}
	else
	if (i == rbottom)	{
	   if (c0 == 0)	{
		tb_del_line(tb);
		i--;
		rtop--;
		if (rtop < rbottom)
			break;
			}
	   else	{
		tb->currentline->buf[c0] = '\0';
		tb->currentline->attr[c0] = '\0';
		tb->currentline->strlen = c0;
		tb->currentline->flags |= TLINE_MODIFIED;
		}
				}
	else			{
		tb_del_line(tb);
		i--;
		rtop--;
		if (rtop < rbottom)
			break;
				}
					}

    return TRUE;
} /* tb_del_block() */

int
tb_handle_tabs(TextBuf *tb)
/*
 * Desc: replace TABs with SPACEs in the current line according to <tablen>
 * returns number of SPACES that was inserted
 */
{
    TextLine *tl = tb->currentline;
    char *p, *p1, *newbuf;
    int pos, spaces, i, moved;

    if (!tl)
	return 0;

    moved = 0;
    /* scan line for TABs */
    p = tl->buf;
    while ((p1 = strchr(p, '\t')) != NULL) {
     /* calculate TAB position */
     pos = p1 - tl->buf;
     /* calculate number of SPACEs to insert */
     if (tb->tablen > pos)
	spaces = tb->tablen - pos;
     else
	spaces = tb->tablen - (pos % tb->tablen);
     moved += spaces; /* record how much spaces we added */
     spaces--;
     /* if only one SPACE then replace TAB with it and go on */
     if (spaces == 0) 	{
	*p1 = ' ';
	p1++;
	p = p1;
	continue;	}
     /* check if the spaces can be fitted in the buffer */
     if (tl->strlen+spaces >= tl->buflen) {
	/* reallocate the buffer space and add a fixed amount of space 
	   to it */
	newbuf = realloc(tl->buf, tl->buflen+TB_BUFINC);
	if (!newbuf) {
	   tb_error("tb_insert_char(): Could not realloc, spaces not inserted");
	   return moved;
	}
	tl->buf = newbuf;

	newbuf = realloc(tl->attr, tl->buflen+TB_BUFINC);
	if (!newbuf) {
	   tb_error("tb_insert_char(): Could not realloc attr, spaces not inserted");
	   return moved;
	}
	tl->attr = newbuf;

	tl->buflen = tl->buflen + TB_BUFINC;
    }
    /* tl->strlen+spaces < tl->buflen !!! */

    /* first move the characters after pos spaces position */
    for (i=tl->strlen; i>pos; i--)	{
	tl->buf[i+spaces] = tl->buf[i];
	tl->attr[i+spaces] = tl->attr[i]; }
    /* insert spaces */
    for (i=pos; i<=pos+spaces;i++)	{
	tl->buf[i] = ' ';
	tl->attr[i] = tb->attr_def;	}
    /* adjust strlen */
    tl->strlen += spaces;
    /* continue the search */
    p = tl->buf + pos + 1;
					   }

    if (moved)
	tl->flags |= TLINE_MODIFIED;

    return moved;
}

/***********************************************************************
 * 
 * Memory buffer
 *
 ***********************************************************************/

void
tb_set_text(TextBuf *tb, char *buf, long len)
/* 
 * Desc: the text from <buf> is loaded into <tb>. The current buffer
 *	is cleared
 *	<len> is the length of <buf>. If it's negative <buf> is assumed
 *	to be NULL terminated.
 */
{
char *t;
long buflen;

    /* clear the current buffer */
    tb_clear(tb);
    tb->bufchanged = FALSE;
    if (!buf || (len == 0))
	return;

    if (len < 0)
	len = strlen(buf);

    /* scan buffer for newlines */
    /* Note: buf can not be modified! */
    while ((t = memchr((void *)buf, '\n', len)) != NULL) {
	buflen = ++t - buf;
	len -= buflen;
	buflen--;
	if (buflen && (buf[buflen - 1] == '\r'))
		buflen--;
	tb_append_buf(tb, buf, buflen);
	tb_handle_tabs(tb);
	tb->currentline = tb->lastline;
	tb_set_next_line(tb);
	buf = t;
							 }

    if (len > 0)
	tb_append_buf(tb, buf, len);

    return;
}

u_long
tb_get_textlen(TextBuf *tb)
/*
 * Desc: calculate length of text in buffer
 */
{
u_long buflen = 0;
TextLine *tl = tb->firstline;

 while (tl) {
  buflen += tl->strlen;
  tl = tl->next;
  if (tl)
	buflen++;
	    }

 return buflen;
}

char *
tb_get_text(TextBuf *tb, u_long *len)
/* 
 * Desc: put text from <tb> in newly allocated buffer.
 * returned buffer should be freed.
 * returns pointer to buffer. buffer length is returned in len.
 */
{
char *line, *buf, *p;
u_long *buflen, mlen;
TextLine *tl;
int i;

    /* save the current line */
    i = tb->i;
    tl = tb->currentline;

    if (len == NULL)
	buflen = &mlen;
    else
	buflen = len;

    /* first caclulate the buffer length */
    *buflen = 0;
    tb_set_current_line(tb, 0);
    do {
	tb_get_line(tb, &line);
	if (!line)
		break;
	*buflen += (strlen(line) + 1);
    } while (tb_set_next_line(tb));

    /* allocate the buffer */
    if ((buf = (char *)malloc(*buflen + 1)) == NULL) {
	tb_error("Can not allocate text buffer");
	tb->i = i;
	tb->currentline = tl;
	return NULL;
					             }

    /* insert text into the buffer */
    p = buf;
    *p = '\0';
    tb_set_current_line(tb, 0);
    do {
	tb_get_line(tb, &line);
	if (!line)
		break;
	strcpy(p, line);
	p += strlen(line);
	*p = '\n';
	p++;
	*p = '\0';
    } while (tb_set_next_line(tb));
    tb->bufchanged = FALSE;

    /* remove last CR
    if (*buflen > 0) 	{
	(*buflen)--;
	buf[*buflen] = '\0';
			}
	*/

    /* restore the current line */
    tb->i = i;
    tb->currentline = tl;

    return buf;
}

/***********************************************************************
 * 
 * File I/O
 *
 ***********************************************************************/

void
tb_load_file(TextBuf *tb, char *fname)
/* 
 * Desc: the file <fname> is loaded into <tb>. The current buffer 
 *	 is cleared.
 * Note: This routine can be highly optimized, aswell as tb_append_line()
 */
{
    FILE *f;
    char line[8192];	/* this limits the loadable line length to 8192 */

    f = fopen(fname, "r");
    if (!f) {
	tb_error("Could not read file %s"/*, fname*/);
	return;
    }

    /* clear the current buffer */
    tb_clear(tb);
    while (fgets(line, 8192, f)) {
	tb_append_line(tb, line);
	tb_handle_tabs(tb);
	/* change tabs to spaces */
	tb->currentline = tb->lastline;

	tb_set_next_line(tb);
    				 }
    fclose(f);
    tb->bufchanged = FALSE;

    return;
} /* tb_load_file() */

void
tb_insert_file(TextBuf *tb, int r, int c, char *fname)
/*
 * Desc: insert file <fname> starting from (r,c) position
 */
{
    FILE *f;
    char line[8192];	/* this limits the loadable line length to 8192 */
    TextLine *tl, *tlold;
    int len, oldwrap;

    /* store the current line */
    tlold = tb->currentline;

    /* no lines yet. insert as a new line(s) */
    if ((tlold == NULL) && (r == 0))	{
	tb_load_file(tb, fname);
	return;
					}

    /* check if line <r> exists */
    if (tb_set_current_line(tb, r) == FALSE)
	return;

    /* check if position is valid */
    if (c > tb->currentline->strlen) {
	tb->currentline = tlold;
	return;			     }

    tl = tb->currentline;

    f = fopen(fname, "r");
    if (!f) {
	tb_error("Could not read file %s"/*, fname*/);
	return;
    }

    /*read the first line */
    if (!fgets(line, 8191, f)) 	{
	fclose(f);
	return;			}

    /* make sure that line ends with CR */
    len = strlen(line);
    if (line[len - 1] != '\n')	{
	line[len] = '\n';
	line[len + 1] = '\0';	}

    /* temporary disable wrapping */
    oldwrap = tb->linewrap;
    tb->linewrap = 0;

    /* insert it as a block */
    tb_insert_block(tb, r, c, line);
    tb_set_next_line(tb);

    while (fgets(line, 8191, f)) {
	/* insert each line */
	tb_insert_line(tb, line);
	tb_set_next_line(tb);
				 }

    fclose(f);

    /* rewrap everything */
    tb->linewrap = oldwrap;
    tb_wrap_lines(tb);

    tb->currentline = tlold;

    return;
}

void
tb_save_file(TextBuf *tb, char *fname)
/*
 * Desc: save the current buffer <tb> to the file <fname>
 */
{
    FILE *f;
    char *line;

    f = fopen(fname, "w");
    if (!f) {
	tb_error("Could not save to file %s"/*, fname*/);
	return;
    }
    
    tb_set_current_line(tb, 0);
    do {
	tb_get_line(tb, &line);
	if (!line)
		break;
	fprintf(f, "%s\n", line);
    } while (tb_set_next_line(tb));
    fclose(f);
    tb->bufchanged = FALSE;

    return;
} /* tb_save_file() */

/***********************************************************************
 *
 * Miscellaneous
 *
 ***********************************************************************/

int
tb_set_current_line(TextBuf *tb, int n)
/*
 * Desc: Set the current line to the n-th line
 * n == -1 - lastline n == 0 - firstline
 */
{
int i;
TextLine *tl;

   i = 0;
   tl = tb->firstline;

   if (n == -1) {
    while (tl && (tl != tb->lastline)) {
       tl = tl->next;
       i++;			       }

    if (tl) {
	tb->currentline = tl;
	tb->i = i;
	return TRUE;
	    }

    return FALSE;
		}

   while (tl && i<n) {
       tl = tl->next;
       i++;
   }

   if (tl && i==n) {
       /* we've got the right linenr */
       tb->currentline = tl;
       tb->i = n;
       return TRUE;
   } else /* the linenr does not exist */
       return FALSE;

} /* tb_set_line() */

int
tb_set_next_line(TextBuf *tb)
/*
 * Desc: set the current line to the next one in the buffer, 
 *	 return FALSE if at EOF, else TRUE
 */
{
    if (tb->currentline->next) {
	tb->currentline = tb->currentline->next;
	tb->i++;
	return(TRUE);
    } else {
	return(FALSE);
    }
} /* tb_set_next_line() */

int
tb_set_prev_line(TextBuf *tb)
/*
 * Desc: set the current line to the previous one in the buffer 
 *	 return FALSE if at BOF, else TRUE
 */
{
    if (tb->currentline->prev) {
	tb->currentline = tb->currentline->prev;
	tb->i--;
	return(TRUE);
    } else {
	return(FALSE);
    }
} /* tb_set_prev_line() */

int
tb_get_nlines(TextBuf *tb)
/*
 * Desc: return the # of lines in <tb>
 */
{
    return(tb->n);
} /* tb_get_nlines() */

int
tb_get_current_line_nr(TextBuf *tb)
/*
 * Desc: return the index of the current line 
 */
{
    return(tb->i);
} /* tb_get_current_line_nr() */

int
tb_get_linelen(TextBuf *tb)
/*
 * Desc: return the lenght of the current line
 */
{
    if (tb->currentline)
	return tb->currentline->strlen;
    else
	return(0);
}/* tb_get_linelen() */

void
tb_set_linelen(TextBuf *tb, int len)
/*
 * Desc: set lenght of current line (cut the line)
 */
{
    if (!tb->currentline)
	return;

    if (len < tb->currentline->strlen)    {
	tb->bufchanged = TRUE;
	tb->currentline->flags |= TLINE_MODIFIED;
	tb->currentline->strlen = len;
	tb->currentline->attr[len] = '\0';
	tb->currentline->buf[len] = '\0'; }
}

void
tb_set_linewrap(TextBuf *tb, int wrap)
/*
 * Desc: set line (word) wrap length
 */
{
 tb->linewrap = wrap;
}

int
tb_get_linewrap(TextBuf *tb)
/*
 * Desc: get line (word) wrap length
 */
{
 return tb->linewrap;
}

void
tb_set_linefgcolor(TextBuf *tb, int c)
/*
 * Desc: set the current line color 
 */
{
TextLine *tl;

    tl = tb->currentline;
    tl->fgcolor = c;

    while (tl->cont && (tl->next == tl->cont))	{
	tl = tl->cont;
	tl->fgcolor = c;			}

    tl = tb->currentline;
    while (tl->prev && (tl->prev->cont == tl))	{
	tl = tl->prev;
	tl->fgcolor = c;			}

    return;
} /* tb_set_linefgcolor() */

void
tb_set_linebgcolor(TextBuf *tb, int c)
/*
 * Desc: set the current bg line color 
 */
{
TextLine *tl;

    tl = tb->currentline;
    tl->bgcolor = c;

    while (tl->cont && (tl->next == tl->cont))	{
	tl = tl->cont;
	tl->bgcolor = c;			}

    tl = tb->currentline;
    while (tl->prev && (tl->prev->cont == tl))	{
	tl = tl->prev;
	tl->bgcolor = c;			}

    return;
} /* tb_set_linebgcolor() */

int
tb_get_linefgcolor(TextBuf *tb)
/*
 * Desc: return the fg color for the current line
 */
{
    return(tb->currentline->fgcolor);
} /* tb_get_linefgcolor() */

int
tb_get_linebgcolor(TextBuf *tb)
/*
 * Desc: return the bg color for the current line
 */
{
    return(tb->currentline->bgcolor);
} /* tb_get_linebgcolor() */

int
tb_bufchanged(TextBuf *tb)
/*
 * Desc: return the status of the textbuffer
 */
{
    return(tb->bufchanged);
} /* tb_bufchanged() */

void
tb_set_bufchanged(TextBuf *tb, int status)
/* 
 * Desc: set the bufferchanged status
 */
{
    tb->bufchanged = status;
    return;
} /* tb_set_bufchanged() */

int
tb_find_text(TextBuf *tb, char *text, int sline, int spos, int *pos)
/*
 * Desc: search for <text> in <tb> starting from line <sline>:<spos>
 * Ret: line number of line that contains the text or -1 if text not found
 *      <pos> returns the position of the text in the line
 */
{
TextLine *tl;
int line;
char *p, *str;

 tl = tb->firstline;
 line = 0;
 while (tl)	{
  if (line < sline) {
	tl = tl->next;
	line++;
	continue;   }

  if (sline == line)
	str = tl->buf + spos;
  else
	str = tl->buf;

  if ((p = strstr(str, text)) != NULL)	{
   if (pos)
	*pos = p - tl->buf;
   return line;				}

  tl = tl->next;
  line++;
		}

 return -1;
}

void
tb_fix_line(TextLine *tl)
/*
 * Take care of all CR and LF characters in the string
 */
{
char *p;
unsigned char c;

 p = tl->buf;

 while (*p != '\0') {
   c = *(unsigned char *)p;
   switch (c)	{
    case '\010':
    case 0xa0:
	*p = '_';
    break;

    case '\n':
	if (p[1] == '\0')	{
		*p = '\0';
		tl->strlen = strlen(tl->buf);
		tl->attr[tl->strlen] = '\0';
		return;		}
	*p = '_';
    break;

    case '\r':
	 if ((p[1] == '\n') && (p[2] == '\0'))	{
		*p = '\0';
		tl->strlen = strlen(tl->buf);
		tl->attr[tl->strlen] = '\0';
		return;				}
	*p = '_';
    break;
		}
   p++;
	   	    }

 tl->attr[tl->strlen] = '\0';

 return;
}

int
tb_set_flags(TextBuf *tb, int flags)
/*
 * Desc: Returns flags value for given textbuf.
 * Ret: old flags value. If supplied flags value is -1 then it's not set
 */
{
int oflags = tb->flags;

 if (flags >= 0)
  tb->flags = flags;

 return oflags;
}

textline_callback
tb_set_callback(TextBuf *tb, textline_callback callback, int cbtype, int data)
/*
 * Desc: set callback on current line
 * Ret: old callback value
 */
{
textline_callback ocb = NULL;
TextLine *tl;

  if (tb->currentline == NULL)
	return NULL;

  tb->currentline->callback_data = data;

  switch (cbtype) {
   case FL_TEXTLINE_CLK_CALLBACK:
	ocb = tb->currentline->clk_callback;
	tb->currentline->clk_callback = callback;
   break;

   case FL_TEXTLINE_DBL_CALLBACK:
	ocb = tb->currentline->dbl_callback;
	tb->currentline->dbl_callback = callback;
   break;

   case FL_TEXTLINE_CUR_CALLBACK:
	ocb = tb->currentline->cur_callback;
	tb->currentline->cur_callback = callback;
   break;

   default:
	return NULL;
   break;
		  }

  tl = tb->currentline;
  while (tl->cont && (tl->next == tl->cont))  {
   tl = tl->cont;
   switch (cbtype) {
    case FL_TEXTLINE_CLK_CALLBACK:
	tl->clk_callback = callback;
    break;

    case FL_TEXTLINE_DBL_CALLBACK:
	tl->dbl_callback = callback;
    break;

    case FL_TEXTLINE_CUR_CALLBACK:
	tl->cur_callback = callback;
    break;
		   }
						}

  tl = tb->currentline;
  while (tl->prev && (tl->prev->cont == tl))	{
   tl = tl->prev;
   switch (cbtype) {
    case FL_TEXTLINE_CLK_CALLBACK:
	tl->clk_callback = callback;
    break;

    case FL_TEXTLINE_DBL_CALLBACK:
	tl->dbl_callback = callback;
    break;

    case FL_TEXTLINE_CUR_CALLBACK:
	tl->cur_callback = callback;
    break;
		   }
						}

  return ocb;
}

