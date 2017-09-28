/***********************************************************************
 *
 * Module: textbuf.h
 * Author: Marc van Kempen (marc@bowtie.nl)
 * Desc:   include for textbuf.c
 *	   definition of textbuffer structures
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

/*       $Id: textbuf.h,v 2.6 1998/04/13 13:24:09 gena Exp $
 */

#ifndef _TEXTBUF_H_
#define _TEXTBUF_H_

/***********************************************************************
 *
 * Textbuffer definitions
 *
 ***********************************************************************/

struct TextLine;

typedef	int (*textline_callback) (void *, struct TextLine *, int, int);

typedef struct TextLine {
    struct TextLine *prev,      /* previous line in list */
		    *next,      /* next line in list */
		    *cont;	/* continuation line */
    char	    *buf;       /* buffer to hold the data */
    char	    *attr;	/* buffer to hold the attributes */
#define	ATTR_COLOR_MASK	0x1f	/* color bits */
#define	ATTR_UNDERLINE	0x20	/* underline */
#define	ATTR_SELECT	0x40	/* select pseudo-attribute */
#define	ATTR_ISSET	0x80	/* this character has an attribute */
#define	ATTR_EMPTY	0x5f	/* this character has no attribute */
    int		    buflen;	/* length of buffer */
    int 	    strlen;	/* length of string in buffer */
    int		    fgcolor,
		    bgcolor;	/* color of the line */
    int		    flags;	/* misc flags */
#define	TLINE_MODIFIED	0x01	/* line was modified */
    textline_callback	clk_callback;	/* called on mouse click */
    textline_callback	dbl_callback;	/* called on double-click */
    textline_callback	cur_callback;	/* called on cursor movement */
#define	FL_TEXTLINE_CLK_CALLBACK	0x01
#define	FL_TEXTLINE_DBL_CALLBACK	0x02
#define	FL_TEXTLINE_CUR_CALLBACK	0x03
    int			callback_data;	/* data to pass to the callback */
} TextLine;

typedef struct TextBuf {
    TextLine    *firstline;     /* pointer to the first line */
    TextLine    *currentline;   /* pointer to the current line */
    TextLine    *lastline;      /* pointer to the last line */
    int		n;		/* the # of lines in the buffer */
    int		i;		/* the line# of the current line */
    int		bufchanged;	/* was the buffer changed? */
    int		tablen;		/* TAB length */
    int		fgcolor_def,	/* default back and foreground colors */
		bgcolor_def;
    int		attr_def;	/* default attribute */
    int		linewrap;	/* wrap line if it's longer then this # */
				/* 0 means no wrap, negative - word wrap */
    int		flags;		/* misc flags */
#define	TEXT_AUTOFORMAT	0x01	/* automatically reformat paragraphs */
    int		maxchars;	/* length of the longest line in buffer */
} TextBuf;


/***********************************************************************
 *
 * Prototypes
 *
 ***********************************************************************/

void tb_init(TextBuf *tb);
void tb_clear(TextBuf *tb);

int  tb_get_char(TextBuf *, int, char *);
void tb_get_line(TextBuf *, char **);
void tb_get_line_by_num(TextBuf *, char **, int);
char *tb_return_line(TextBuf *);
TextLine *tb_get_lineptr_by_num(TextBuf *, int);
void tb_get_block(TextBuf *, int, int, int, int, char **);
void tb_fill_region(TextBuf *, int, int, int, int);
void tb_get_paragraph(TextBuf *, int *, int *);

int tb_insert_char(TextBuf *, int, char);
void tb_insert_line(TextBuf *, char *);
void tb_insert_block(TextBuf *, int, int, char *);
void tb_insert_rect(TextBuf *, int, int, int, int, char *);
void tb_insert_cr(TextBuf *, int);
int tb_handle_tabs(TextBuf *);
int tb_reformat(TextBuf *);
int tb_wrap_line(TextBuf *);
void tb_wrap_lines(TextBuf *);
void tb_modify_lines(TextBuf *);

int  tb_del_char(TextBuf *, int);
void tb_del_line(TextBuf *);
int tb_del_block(TextBuf *, int, int, int, int);

void tb_load_file(TextBuf *, char *fname);
void tb_save_file(TextBuf *, char *fname);
void tb_insert_file(TextBuf *, int, int, char *fname);

void tb_set_text(TextBuf *, char *buf, long len);
char *tb_get_text(TextBuf *, u_long *len);
u_long tb_get_textlen(TextBuf *);

int  tb_get_nlines(TextBuf *);
int  tb_get_linelen(TextBuf *);
void tb_set_linelen(TextBuf *, int);

int  tb_bufchanged(TextBuf *);
void tb_set_bufchanged(TextBuf *, int);

int  tb_set_current_line(TextBuf *tb, int n);
int  tb_set_next_line(TextBuf *tb);
int  tb_set_prev_line(TextBuf *tb);

void tb_append_line(TextBuf *, char *);
void tb_append_buf(TextBuf *, char *, u_long);
void tb_append_to_line(TextBuf *, char *);

void tb_set_linefgcolor(TextBuf *, int);
void tb_set_linebgcolor(TextBuf *, int);
int  tb_get_linefgcolor(TextBuf *);
int  tb_get_linebgcolor(TextBuf *);

void tb_set_linewrap(TextBuf *, int);
int tb_get_linewrap(TextBuf *);
int tb_find_text(TextBuf *, char *, int , int, int *);
void tb_fix_line(TextLine *tl);
int tb_set_flags(TextBuf *, int);
void tb_set_block_attr(TextBuf *, int, int, int, int, int);
textline_callback tb_set_callback(TextBuf *, textline_callback, int, int);

#endif /* _TEXTBUF_H_ */
