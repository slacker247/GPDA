/***********************************************************************
 *
 * Module: fl_edit.h
 * Author: Marc van Kempen (marc@bowtie.nl)
 * Desc:   include file for textedit object 
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

/*       $Id: fl_edit.h,v 2.9 1998/04/13 13:24:07 gena Exp $
 */

#ifndef __FL_EDIT_H__
#define __FL_EDIT_H__

#ifdef  HAVE_CONFIG_H
#include <config.h>
#endif
#include "textbuf.h"

/***********************************************************************
 *
 * Defines
 *
 ***********************************************************************/

#define FL_TEXTEDIT		1001
#define FL_NORMAL_TEXTEDIT 	0

typedef  int (*textedit_callback) (FL_OBJECT *, char *, int, int, int);
typedef  int (*line_valid) (char *);

typedef struct {
    TextBuf tb;			/* a textbuffer */
    char name[MAXPATHLEN];	/* the name of the buffer */
    int r, c;			/* the coordinates of the cursor */
    int cpos;			/* desired x coord of cursor */
    int topline;		/* the topline to be shown */
    int leftcol;		/* the leftmost column to be shown 
				   in characters */
    int text_style;		/* style of text */
    int text_size;		/* size of text */
    int ccol;			/* color of textcursor */
    FL_OBJECT *sb, *hsb;	/* scrollbar objects */
    int v_on, h_on;		/* != 0 if scrollbar is visible */
    int vw , hh;		/* width of the scrollbars */
    int sselr, sselc;		/* row & column of selection start */
    int eselr, eselc;		/* row & column of selection end */
    int flags;			/* special flags */
#define	FL_TEXTEDIT_READONLY	0x0001	/* widget is not editable */
#define	FL_TEXTEDIT_NOTDRAWN	0x0002	/* no FL_DRAW received yet */
#define	FL_TEXTEDIT_VSBAR	0x0004	/* vertical scrollbar */
#define	FL_TEXTEDIT_VSBAR_AUTO	0x0008	/* vertical scrollbar autohide */
#define	FL_TEXTEDIT_HSBAR	0x0010	/* horizontal scrollbar */
#define	FL_TEXTEDIT_HSBAR_AUTO	0x0020	/* horizontal scrollbar autohide */
#define	FL_TEXTEDIT_SANCHOR_END	0x0040	/* selection anchor is at the end */
#define	FL_TEXTEDIT_NOCUR	0x0080	/* don't show the cursor */
#define	FL_TEXTEDIT_PASTE_CUR	0x0100	/* paste at cursor position */
    char *exp;			/* text to search for */
    int oldh, oldw;		/* old object size */
    textedit_callback	key_callback;	/* called on key press       */
    textedit_callback	clk_callback;	/* called on mouse click     */
    textedit_callback	dbl_callback;	/* called on double-click    */
    textedit_callback	cur_callback;	/* called on cursor movement */
#define	FL_TEXTEDIT_KEY_CALLBACK	0x01
#define	FL_TEXTEDIT_CLK_CALLBACK	0x02
#define	FL_TEXTEDIT_DBL_CALLBACK	0x03
#define	FL_TEXTEDIT_CUR_CALLBACK	0x04
    int wsize, csize;		/* screen size in characters */
    int charheight;		/* character height in pixels */
    int charwidth;		/* character width in pixels */
    int paster, pastec;		/* where to paste clipboard contents */
} SPEC;

/* keybindings definition */
typedef struct {
int function;		/* key function (see below) */
long key,		/* X key value              */
     keydef;		/* default X key value      */
} keybind;
 
/* digraph definition */
typedef struct _digraph {
char seq[2];
int key;
} digraph;

/***********************************************************************
 *
 * Prototypes
 *
 ***********************************************************************/

extern FL_OBJECT *fl_create_textedit(int, FL_Coord, FL_Coord, FL_Coord,FL_Coord,
				     const char *);
extern FL_OBJECT *fl_add_textedit(int, FL_Coord, FL_Coord, FL_Coord, FL_Coord,
				  const char *);
extern void	  fl_save_textedit(FL_OBJECT *ob, char *fname);
extern void	  fl_load_textedit(FL_OBJECT *ob, char *fname);
extern void	  fl_set_textedit_fontsize(FL_OBJECT *, int);
extern int	  fl_get_textedit_fontsize(FL_OBJECT *);
extern void	  fl_set_textedit_fontstyle(FL_OBJECT *, int);
extern int	  fl_get_textedit_fontstyle(FL_OBJECT *);
extern void	  fl_set_textedit_color(FL_OBJECT *, int, int, int, int);
extern void	  fl_get_textedit_color(FL_OBJECT *, int *, int *, int *);
extern void	  fl_set_textedit_bufname(FL_OBJECT *, char *);
extern char	 *fl_get_textedit_bufname(FL_OBJECT *);
extern void	  fl_set_textedit_bufchanged(FL_OBJECT *, int);
extern int	  fl_get_textedit_bufchanged(FL_OBJECT *);
extern void	  fl_set_textedit(FL_OBJECT *, char *buf, long len);
extern char	 *fl_get_textedit(FL_OBJECT *, u_long *len);
extern char	 *fl_get_textedit_seltext(FL_OBJECT *ob);
extern FL_OBJECT *fl_get_textedit_vscrollbar(FL_OBJECT *);
extern void	  fl_set_textedit_vscrollbar(FL_OBJECT *, int);
extern FL_OBJECT *fl_get_textedit_hscrollbar(FL_OBJECT *);
extern void	  fl_set_textedit_hscrollbar(FL_OBJECT *, int);
extern void	  fl_calc_cursorpos(FL_OBJECT *, u_long, int *, int *);
extern void	  fl_set_textedit_cursorpos(FL_OBJECT *, int, int, long, int);
extern void	  fl_get_textedit_cursorpos(FL_OBJECT *, int *, int *,u_long *);
extern void	  fl_clear_textedit(FL_OBJECT *);
extern int	  fl_set_textedit_wrap(FL_OBJECT *, int, int);
extern void	  fl_insert_textedit(FL_OBJECT *, char *);
extern void	  fl_insert_textedit_file(FL_OBJECT *, char *);
extern void	  fl_textedit_paste(FL_OBJECT *);
extern int	  fl_textedit_readonly(FL_OBJECT *, int);
extern int	  fl_get_textedit_topline(FL_OBJECT *);
extern void	  fl_set_textedit_topline(FL_OBJECT *, int);
extern int	  fl_get_textedit_maxline(FL_OBJECT *);
extern int	  fl_get_textedit_screenlines(FL_OBJECT *);
extern void	  fl_set_textedit_screenlines(FL_OBJECT *, int);
extern char	 *fl_get_textedit_curline(FL_OBJECT *);
extern char	 *fl_get_textedit_line(FL_OBJECT *, int);
extern void	  fl_add_textedit_line(FL_OBJECT *, char *);
extern void	  fl_addto_textedit(FL_OBJECT *, char *);
extern int	  fl_textedit_line_visible(FL_OBJECT *, int);
extern void	  fl_insert_textedit_line(FL_OBJECT *, int, char *);
extern void	  fl_delete_textedit_line(FL_OBJECT *, int);
extern void	  fl_replace_textedit_line(FL_OBJECT *, int, char *);
extern void	  fl_append_to_textedit_line(FL_OBJECT *, int, char *);
extern void	  fl_deselect_textedit(FL_OBJECT *);
extern int	  fl_isselected_textedit_line(FL_OBJECT *, int);
extern void	  fl_select_textedit_line(FL_OBJECT *, int);
extern void	  fl_deselect_textedit_line(FL_OBJECT *, int);
extern void	  fl_set_textedit_line_color(FL_OBJECT *, int, int, int);
extern void	  fl_get_textedit_line_color(FL_OBJECT *, int, int *, int *);
extern void	  fl_scroll_textedit(FL_OBJECT *, int);
extern void	  fl_textedit_replace_sel(FL_OBJECT *, char *);
extern char      *fl_textedit_get_nextword(FL_OBJECT *, line_valid);
#define	FL_TEXTEDITSCROLL_PGUP		0x01
#define	FL_TEXTEDITSCROLL_PGDOWN	0x02
#define	FL_TEXTEDITSCROLL_LINEUP	0x04
#define	FL_TEXTEDITSCROLL_LINEDOWN	0x08
#define	FL_TEXTEDITSCROLL_TOP		0x10
#define	FL_TEXTEDITSCROLL_BOTTOM	0x20
extern textedit_callback fl_textedit_set_callback(FL_OBJECT *,
						textedit_callback, int);
extern textline_callback fl_textedit_setline_callback(FL_OBJECT *, int,
						textline_callback, int, int);
extern int	  fl_set_textedit_flags(FL_OBJECT *, int, int );
extern int	  fl_set_textbuf_flags(FL_OBJECT *, int );
extern int	  fl_set_textedit_textattr(FL_OBJECT *, int);
extern void	  fl_set_textedit_blockattr(FL_OBJECT *, int, int, int, int, int);

extern void fl_textedit_map_key(int, long, int);
extern int fl_textedit_set_key(int, long *);
extern void fl_textedit_get_key(int, long *);
extern void fl_set_textedit_editkeymap(const FL_EditKeymap *);
extern void fl_textedit_set_keymap(keybind *);
extern keybind *fl_textedit_get_keymap();
extern digraph *fl_textedit_get_digraphs();
extern int fl_textedit_key_remapped(int);

/* key functions */
#define	FL_TEXTKEY_NONE		0x00	/* no key defined        */
#define	FL_TEXTKEY_HOME		0x01	/* home (text beginning) */
#define	FL_TEXTKEY_END		0x02	/* text end              */
#define	FL_TEXTKEY_LBEGIN	0x03	/* line beginning        */
#define	FL_TEXTKEY_LEND		0x04	/* line end              */
#define	FL_TEXTKEY_LEFT		0x05	/* cursor left           */
#define	FL_TEXTKEY_RIGHT	0x06	/* cursor right          */
#define	FL_TEXTKEY_UP		0x07	/* cursor up             */
#define	FL_TEXTKEY_DOWN		0x08	/* cursor down           */
#define	FL_TEXTKEY_PAGEUP	0x09	/* page up               */
#define	FL_TEXTKEY_PAGEDOWN	0x0a	/* page down             */
#define	FL_TEXTKEY_LKILL	0x0b	/* kill part or the whole line */
#define	FL_TEXTKEY_REFRESH	0x0c	/* refresh screen        */
#define	FL_TEXTKEY_LCLEAR	0x0d	/* clear line            */
#define	FL_TEXTKEY_PASTE	0x0e	/* paste                 */
#define	FL_TEXTKEY_LDELETE	0x0f	/* delete line           */
#define	FL_TEXTKEY_CLEAR	0x10	/* clear screen          */
#define	FL_TEXTKEY_FIND		0x11	/* find text             */
#define	FL_TEXTKEY_FAGAIN	0x12	/* find again            */
#define	FL_TEXTKEY_FORMAT	0x13	/* format paragraph      */
#define	FL_TEXTKEY_DEBUG	0x14	/* print debug info      */
#define	FL_TEXTKEY_DELETE	0x15	/* delete char (block)   */
#define	FL_TEXTKEY_BACKSPACE	0x16	/* backspace             */
#define	FL_TEXTKEY_WORDLEFT	0x17	/* one word left         */
#define	FL_TEXTKEY_WORDRIGHT	0x18	/* one word right        */
#define	FL_TEXTKEY_DELWORDL	0x19	/* delete prev word      */
#define	FL_TEXTKEY_DELWORDR	0x1a	/* delete next word      */
#define	FL_TEXTKEY_SLEFT	0x1b	/* select left           */
#define	FL_TEXTKEY_SRIGHT	0x1c	/* select right          */
#define	FL_TEXTKEY_SUP		0x1d	/* select up             */
#define	FL_TEXTKEY_SDOWN	0x1e	/* select down           */
#define	FL_TEXTKEY_CUT		0x1f	/* cut                   */
#define	FL_TEXTKEY_COPY		0x20	/* copy                  */
#define	FL_TEXTKEY_DIGRAPH	0x21	/* compose digraph       */

#define	FL_TEXTKEY_ENDARRAY	0x40	/* marks end of the array*/
#define	FL_TEXTKEY_MAX		0x40	/* limit on keybindings  */
#define	FL_TEXTKEY_MAXBIND	0x04	/* don't bind more then 4 keys 
						to one function  */

#define	FL_ANY_MASK	(1L<<28)	/* pseudo-mask which masks any
						modifier */

#endif /* __FL_EDIT_H__ */
