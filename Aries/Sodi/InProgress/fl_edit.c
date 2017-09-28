/***********************************************************************
 *
 * Module: fl_edit.c
 * Author: Marc van Kempen (marc@bowtie.nl)
 * Desc:   implementation of edit object 
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

/*       $Id: fl_edit.c,v 2.22 1998/04/21 13:53:55 gena Exp $
 */

#ifdef  HAVE_CONFIG_H
#include <config.h>
#include <stdlib.h>
#include <sys/param.h>

#if HAVE_UNISTD_H 
#include <sys/types.h>
#include <unistd.h> 
#endif

#if HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

#if HAVE_CTYPE_H
#include <ctype.h>
#endif

#else
#include <stdlib.h>
#include <sys/types.h>
#include <sys/param.h>
#include <unistd.h> 
#include <sys/time.h>
#include <ctype.h>
#endif

#include <forms.h>
#include "fl_edit.h"
#include "textbuf.h"
#include "fl_error.h"
#include "textform.h"

/***********************************************************************
 *
 * Some definitions 
 *
 ***********************************************************************/

/* a little macro for control characters */
#define ctrl(a) (a-'A'+1)

/***********************************************************************
 *
 * Textarea and coordinates
 * ------------------------
 *                             ob->x + ob->w - 
 *                          2 * ob->bw - sb_width
 *                                           |    +-> ob->x + ob->w
 *  ob->x                                    v    |
 *  +----------------------------------------+----+
 *  | x --+                                  |^^^^|
 *  |     |                                  |  | |
 *  .     v                                  .  +---> TEXT_SB_WIDTH
 *  .  (TEXT_X_OFFSET, TEXT_Y_OFFSET)        .    .
 *
 *  .                                        .    .
 *  |                                        |    |
 *  +----------------------------------------+----+
 *
 * Scrollbar     
 * ---------
 *             +-----> TEXT_SB_WIDTH = ob->bw + sb_box_width + ob->bw
 *             |
 *       <--------->
 *  ..--+-----------+
 *      |           |
 *      |           |
 *      .           .
 *      .           .
 *
 ***********************************************************************/

/* define the offset of the text from the coordinates of its box */
#define TEXT_X_OFFSET		2
#define TEXT_Y_OFFSET		0

//extern int fl_get_default_scrollbarsize(FL_OBJECT *);
//extern void fl_add_child(FL_OBJECT *, FL_OBJECT *);
#define fl_gc fl_state[fl_vmode].gc[0]
Window fl_cur_win/* = fl_winget()*/;
int fl_fdesc, fl_fasc, fl_fheight;
XFontStruct *fl_cur_fs;

#define	FL_ALL_MASK		(FL_ALT_MASK|FL_CONTROL_MASK|FL_SHIFT_MASK)
#define	ACCENT_UPPER_DIFF	('a'-'A')

static keybind bindings[FL_TEXTKEY_MAX] = {
{ FL_TEXTKEY_HOME,	XK_Home|FL_CONTROL_MASK,XK_Home|FL_CONTROL_MASK     },
{ FL_TEXTKEY_END,	XK_End|FL_CONTROL_MASK,	XK_End|FL_CONTROL_MASK      },
{ FL_TEXTKEY_LBEGIN,	XK_Home,	XK_Home				    },
{ FL_TEXTKEY_LBEGIN,	ctrl('A')|FL_CONTROL_MASK,ctrl('A')|FL_CONTROL_MASK },
{ FL_TEXTKEY_LEND,	XK_End,		XK_End				    },
{ FL_TEXTKEY_LEND,	ctrl('E')|FL_CONTROL_MASK,ctrl('E')|FL_CONTROL_MASK },
{ FL_TEXTKEY_LEFT,	XK_Left,	XK_Left				    },
{ FL_TEXTKEY_RIGHT,	XK_Right,	XK_Right			    },
{ FL_TEXTKEY_UP,	XK_Up,		XK_Up				    },
{ FL_TEXTKEY_DOWN,	XK_Down,	XK_Down				    },
{ FL_TEXTKEY_PAGEUP,	XK_Prior,	XK_Prior			    },
{ FL_TEXTKEY_PAGEUP,	ctrl('R')|FL_CONTROL_MASK,ctrl('R')|FL_CONTROL_MASK },
{ FL_TEXTKEY_PAGEDOWN,	XK_Next,	XK_Next				    },
{ FL_TEXTKEY_PAGEDOWN,	ctrl('T')|FL_CONTROL_MASK,ctrl('T')|FL_CONTROL_MASK },
{ FL_TEXTKEY_LKILL,	ctrl('K')|FL_CONTROL_MASK,ctrl('K')|FL_CONTROL_MASK },
{ FL_TEXTKEY_REFRESH,	ctrl('L')|FL_CONTROL_MASK,ctrl('L')|FL_CONTROL_MASK },
{ FL_TEXTKEY_LCLEAR,	ctrl('U')|FL_CONTROL_MASK,ctrl('U')|FL_CONTROL_MASK },
{ FL_TEXTKEY_PASTE,	ctrl('V')|FL_CONTROL_MASK,ctrl('V')|FL_CONTROL_MASK },
{ FL_TEXTKEY_LDELETE,	ctrl('Y')|FL_CONTROL_MASK,ctrl('Y')|FL_CONTROL_MASK },
{ FL_TEXTKEY_CLEAR,	ctrl('Z')|FL_CONTROL_MASK,ctrl('Z')|FL_CONTROL_MASK },
{ FL_TEXTKEY_FIND,	ctrl('F')|FL_CONTROL_MASK,ctrl('F')|FL_CONTROL_MASK },
{ FL_TEXTKEY_FAGAIN,	ctrl('G')|FL_CONTROL_MASK,ctrl('G')|FL_CONTROL_MASK },
{ FL_TEXTKEY_FORMAT,	ctrl('Q')|FL_CONTROL_MASK,ctrl('Q')|FL_CONTROL_MASK },
{ FL_TEXTKEY_DELETE,	127,			127			    },
{ FL_TEXTKEY_BACKSPACE,	'\b',			'\b'			    },
{ FL_TEXTKEY_WORDLEFT,	XK_Left|FL_CONTROL_MASK,XK_Left|FL_CONTROL_MASK	    },
{ FL_TEXTKEY_WORDRIGHT,	XK_Right|FL_CONTROL_MASK,XK_Right|FL_CONTROL_MASK   },
{ FL_TEXTKEY_DELWORDL,	'\b'|FL_SHIFT_MASK,	'\b'|FL_SHIFT_MASK	    },
{ FL_TEXTKEY_DELWORDL,	ctrl('W')|FL_CONTROL_MASK,ctrl('W')|FL_CONTROL_MASK },
{ FL_TEXTKEY_DELWORDR,	127|FL_SHIFT_MASK,	127|FL_SHIFT_MASK	    },
{ FL_TEXTKEY_SLEFT,	XK_Left|FL_SHIFT_MASK,	XK_Left|FL_SHIFT_MASK	    },
{ FL_TEXTKEY_SRIGHT,	XK_Right|FL_SHIFT_MASK,	XK_Right|FL_SHIFT_MASK      },
{ FL_TEXTKEY_SUP,	XK_Up|FL_SHIFT_MASK,	XK_Up|FL_SHIFT_MASK	    },
{ FL_TEXTKEY_SDOWN,	XK_Down|FL_SHIFT_MASK,	XK_Down|FL_SHIFT_MASK	    },
{ FL_TEXTKEY_CUT,	ctrl('X')|FL_CONTROL_MASK,ctrl('X')|FL_CONTROL_MASK },
{ FL_TEXTKEY_COPY,	ctrl('C')|FL_CONTROL_MASK,ctrl('C')|FL_CONTROL_MASK },
{ FL_TEXTKEY_DIGRAPH,	ctrl('D')|FL_CONTROL_MASK,ctrl('D')|FL_CONTROL_MASK },
#ifdef	DEBUG
{ FL_TEXTKEY_DEBUG,	ctrl('P')|FL_CONTROL_MASK,ctrl('P')|FL_CONTROL_MASK },
#endif
{ FL_TEXTKEY_ENDARRAY,	0,		0       			    }
};

/* list of digraphs */
static struct _digraph digraphs[] = {
{ "~!",	XK_exclamdown  },
{ "!!",	XK_exclamdown  },
{ "C$",	XK_cent        },
{ "C/",	XK_cent        },
{ "c/",	XK_cent        },
{ "L$",	XK_sterling    },
{ "L-",	XK_sterling    },
{ "l-",	XK_sterling    },
{ "X$",	XK_currency    },
{ "ox",	XK_currency    },
{ "0X",	XK_currency    },
{ "OX",	XK_currency    },
{ "0x",	XK_currency    },
{ "Y$",	XK_yen         },
{ "Y-",	XK_yen         },
{ "y-",	XK_yen         },
{ "||",	XK_brokenbar   },
{ "-a",	XK_ordfeminine },
{ "-A",	XK_ordfeminine },
{ "_O",	XK_masculine   },
{ "_o",	XK_masculine   },
{ "^-",	XK_macron      },
{ "SO",	XK_section     },
{ "so",	XK_section     },
{ "cO",	XK_copyright   },
{ "co",	XK_copyright   },
{ "CO",	XK_copyright   },
{ "rO",	XK_registered  },
{ "ro",	XK_registered  },
{ "RO",	XK_registered  },
{ "P!",	XK_paragraph   },
{ "p!",	XK_paragraph   },
{ "/u",	XK_mu          },
{ "^*",	XK_degree      },
{ "^0",	XK_degree      },
{ "^.",	XK_periodcentered },
{ "~?",	XK_questiondown},
{ "??",	XK_questiondown},
{ "+-",	XK_plusminus   },
{ "xx",	XK_multiply    },
{ "-:",	XK_division    },
{ "-|",	XK_notsign     },
{ "-,",	XK_notsign     },
{ "--",	XK_hyphen      },
{ "^1",	XK_onesuperior },
{ "^2",	XK_twosuperior },
{ "^3",	XK_threesuperior },
{ "14",	XK_onequarter  },
{ "12", XK_onehalf     },
{ "34",	XK_threequarters },
{ "<<",	XK_guillemotleft },
{ ">>",	XK_guillemotright},
{ "\"\"", XK_diaeresis },
{ "\\\\", XK_acute     },
{ ",,", XK_cedilla     },
{ "A`",	XK_Agrave      },
{ "A'",	XK_Aacute      },
{ "A^",	XK_Acircumflex },
{ "A~",	XK_Atilde      },
{ "A-",	XK_Atilde      },
{ "-A",	XK_Atilde      },
{ "A\"",XK_Adiaeresis  },
{ "A*",	XK_Aring       },
{ "AE",	XK_AE          },
{ "C,",	XK_Ccedilla    },
{ "E`",	XK_Egrave      },
{ "E'",	XK_Eacute      },
{ "E^",	XK_Ecircumflex },
{ "E\"",XK_Ediaeresis  },
{ "I`",	XK_Igrave      },
{ "I'",	XK_Iacute      },
{ "I^",	XK_Icircumflex },
{ "I\"",XK_Idiaeresis  },
{ "D-",	XK_ETH         },
{ "N~",	XK_Ntilde      },
{ "O`",	XK_Ograve      },
{ "O'",	XK_Oacute      },
{ "O^",	XK_Ocircumflex },
{ "O~",	XK_Otilde      },
{ "O\"",XK_Odiaeresis  },
{ "O/",	XK_Ooblique    },
{ "U`",	XK_Ugrave      },
{ "U'",	XK_Uacute      },
{ "U^",	XK_Ucircumflex },
{ "U\"",XK_Udiaeresis  },
{ "Y'",	XK_Yacute      },
{ "TP",	XK_THORN       },
{ "TH",	XK_THORN       },
{ "P|",	XK_THORN       },
{ "sz",	XK_ssharp      },
{ "ss",	XK_ssharp      },
{ "a`",	XK_agrave      },
{ "a'",	XK_aacute      },
{ "a^",	XK_acircumflex },
{ "a~",	XK_atilde      },
{ "a-",	XK_atilde      },
{ "-a",	XK_atilde      },
{ "a\"",XK_adiaeresis  },
{ "a*",	XK_aring       },
{ "ae",	XK_ae          },
{ "c,",	XK_ccedilla    },
{ "e`",	XK_egrave      },
{ "e'",	XK_eacute      },
{ "e^",	XK_ecircumflex },
{ "e\"",XK_ediaeresis  },
{ "i`",	XK_igrave      },
{ "i'",	XK_iacute      },
{ "i^",	XK_icircumflex },
{ "i\"",XK_idiaeresis  },
{ "d-",	XK_eth         },
{ "n~",	XK_ntilde      },
{ "o`",	XK_ograve      },
{ "o'",	XK_oacute      },
{ "o^",	XK_ocircumflex },
{ "o~",	XK_otilde      },
{ "o\"",XK_odiaeresis  },
{ "o/",	XK_oslash      },
{ "u`",	XK_ugrave      },
{ "u'",	XK_uacute      },
{ "u^",	XK_ucircumflex },
{ "u\"",XK_udiaeresis  },
{ "y'",	XK_yacute      },
{ "y\"",XK_ydiaeresis  },
{ "tp",	XK_thorn       },
{ "th",	XK_thorn       },
{ "p|",	XK_thorn       },
{ "",	0              }
};

#ifdef ALLOW_DEAD_KEYS
#ifdef XKBE_DEAD_KEYS
static unsigned char *accent_chars = " `´^~¯  ¨°  ¸    ";
#endif
static unsigned char *accents[] = {
	NULL,					/* No accent */
	"aàeèiìoòuù",		/* grave */
	"aáeéiíoóuúyý",	/* acute */
	"aâeêiîoôuû",		/* circumflex */
	"aãnñoõ",			/* tilde */
	NULL,					/* macron */
	NULL,					/* breve */
	NULL,					/* abovedot */
	"aäeëiïoöuüyÿ",	/* diaeresis */
	"aå",				/* abovering */
	NULL,					/* double acute */
	NULL,					/* caron */
	NULL,					/* cedilla */
	NULL,					/* ogonek */
	NULL,					/* iota */
	NULL,					/* voiced sound */
	NULL					/* ??? */
};
#endif

/* local prototypes */
static void fl_textedit_pageup(FL_OBJECT *ob);
static void fl_textedit_pagedown(FL_OBJECT *ob);
static int  fl_textedit_set_cursor(FL_OBJECT *ob, int mx, int my, int draw);
static void fl_textedit_remove_selection(FL_OBJECT *ob);
static int  fl_textedit_selected(FL_OBJECT *ob);
static int  fl_textedit_movecursor(FL_OBJECT *ob, int r, int c);
static int  fl_textedit_movecursor_visible(FL_OBJECT *ob, int r, int c);
static void fl_textedit_inssel(FL_OBJECT *ob, char *buf);
static void fl_textedit_extend_selection(FL_OBJECT *ob, int oldr, int oldc);
static void fl_textedit_copy(FL_OBJECT *ob);
static void fl_textedit_set_vscrollbar(FL_OBJECT *ob);
static void fl_textedit_set_hscrollbar(FL_OBJECT *ob);

FD_textform   *fd_textform;

void fl_edit_error(char *msg)
{
  printf("%s\n", msg);
}
/*
FD_textform *create_form_textform(void)
{
  FL_OBJECT *obj;
  FD_textform *fdui = (FD_textform *) fl_calloc(1, sizeof(*fdui));

  fdui->textform = fl_bgn_form(FL_NO_BOX, 801, 441);
  obj = fl_add_box(FL_UP_BOX,0,0,801,441,"");
  fdui->text = obj = fl_add_textedit(FL_UP_BOX,10,10,780,420,"");
  fl_set_object_color(obj,FL_WHITE,FL_YELLOW);
  fl_end_form();

  fdui->textform->fdui = fdui;

  return fdui;
}
*/
main(int argc, char *argv[])
{

   fl_initialize(&argc, argv, 0, 0, 0);

fd_textform = create_form_textform();

fl_load_textedit(fd_textform->text, "fl_edit.c");

fl_show_form(fd_textform->textform, FL_PLACE_CENTER,FL_FULLBORDER, "Test");

   fl_do_forms();
}
/***********************************************************************
 *
 * Text rendering routines
 * 
 ***********************************************************************/

static void
fl_textedit_get_textbb(FL_OBJECT *ob, int *x, int *y, int *w, int *h)
/*
 * Desc: return the coordinates for the textarea
 */
{
    *x = ob->x + abs(ob->bw) + TEXT_X_OFFSET;
    *y = ob->y + abs(ob->bw) + TEXT_Y_OFFSET;
    *w = ob->w - 2*abs(ob->bw) - TEXT_X_OFFSET;
    *h = ob->h - 2*abs(ob->bw) - TEXT_Y_OFFSET;

    return;
} /* fl_textedit_get_textbb() */

static void
fl_textedit_draw_cursor(FL_OBJECT *obj, FL_Coord x, FL_Coord y)
/*
 * Desc: draw cursor
 */
{
SPEC *spec = (SPEC *)obj->spec;

 fl_set_font(spec->text_style, spec->text_size);
 fl_fheight = fl_get_char_height(spec->text_style, spec->text_size, &fl_fasc, &fl_fdesc);
 fl_rectf(x, y, (FL_Coord)2, fl_fheight, spec->ccol);

 return;
}

static void
fl_textedit_underline_text(FL_OBJECT *obj,
	FL_Coord x, FL_Coord y, FL_Coord w)
/*
 * Desc: underline text
 */
{
unsigned long ul_pos, ul_thickness = 0;

 XGetFontProperty(fl_cur_fs, XA_UNDERLINE_THICKNESS, &ul_thickness);
 if ((ul_thickness == 0) || (ul_thickness > 100))
	ul_thickness = 1;

 if (!XGetFontProperty(fl_cur_fs, XA_UNDERLINE_POSITION, &ul_pos))
	ul_pos = 1;

 fl_cur_win = fl_winget();
 XFillRectangle(fl_display, fl_cur_win, fl_gc, x, (int)(y + ul_pos),
	w, ul_thickness);

 return;
}

static void
fl_textedit_draw_textline(FL_OBJECT *obj, TextLine *tl,
	int align, FL_Coord x, FL_Coord y, FL_Coord w, FL_Coord h,
	int cpos, int sels, int sele)
/*
 * Desc: Draw the line at the specified position with the alignment <align>
 *	 etc.
 *       Draw the selection if needed
 */
{
SPEC *spec = (SPEC *)obj->spec;
int seltype, currattr, currofft, currpos, attr;
char *currline, svchar;
int bgcol, fgcol, i, xc, wc, sw, lnlen;

 if (tl == NULL)	{
	fl_rectf(x - TEXT_X_OFFSET, y, w + TEXT_X_OFFSET, h, obj->col2);
	fl_textedit_draw_cursor(obj, x, y);
	return;		}

 /* check how the selection should be drawed */
 if ((sels == 0) && ((sele == -1) || (sele >= tl->strlen)))	{
	seltype = 1;  /* the whole line is selected */
	sele = tl->strlen;					}
 else
    if ((sels == -1) || ((sele >= 0) && (sele <= sels)))
	seltype = -1; /* line is not selected */
 else	{
	seltype = 0;  /* line is partially selected */
	if ((sele == -1) || (sele >= tl->strlen))
		sele = tl->strlen;
	}

 /* line is invisible */
 if ((spec->leftcol >= tl->strlen) ||
	(tl->buf == NULL) ||
	(*tl->buf == '\0'))	{
	if (seltype == 1)
		fl_rectf(x - TEXT_X_OFFSET, y, w + TEXT_X_OFFSET, h, tl->fgcolor);
	else
		fl_rectf(x - TEXT_X_OFFSET, y, w + TEXT_X_OFFSET, h, tl->bgcolor);
        /* only the cursor is visible */
        if (spec->leftcol == cpos)
	 fl_textedit_draw_cursor(obj, x, y);
	return;			}

 fl_set_font(spec->text_style, spec->text_size);

 if (cpos < 0)
  cpos = -1;
 if (cpos > tl->strlen)
  cpos = tl->strlen;

 if (sels < 0)
	sels = 0;
 if (sele < 0)
	sele = sels;
 else
 if (sele > tl->strlen)
	sele = tl->strlen;

 currpos = spec->leftcol;
 currattr = tl->attr[currpos];
 currline = tl->buf + spec->leftcol;
 currofft = 0;

 switch (seltype)	{
  case 0:
   if ((currpos >= sels) && (currpos < sele))	{
    if (currattr & ATTR_ISSET)
	currattr |= ATTR_SELECT;
    else
	currattr = ATTR_ISSET|ATTR_SELECT;	}
  break;

  case 1:
   if (currattr & ATTR_ISSET)
	currattr |= ATTR_SELECT;
   else
	currattr = ATTR_SELECT|ATTR_ISSET;
  break;
			}

 while (1)	{
   attr = tl->attr[currpos];
   switch (seltype)	{
    case 0:
     if ((currpos >= sels) && (currpos < sele))	{
      if (attr & ATTR_ISSET)
	attr |= ATTR_SELECT;
      else
	attr = ATTR_SELECT|ATTR_ISSET;		}
    break;

    case 1:
     if (attr & ATTR_ISSET)
	attr |= ATTR_SELECT;
     else
	attr = ATTR_SELECT|ATTR_ISSET;
    break;
			}

   if (attr == currattr) {
     if (++currpos < tl->strlen)
	continue;	 }

   svchar = tl->buf[currpos];
   tl->buf[currpos] = '\0';
   lnlen = strlen(currline);

   sw = wc = fl_get_string_width(spec->text_style, spec->text_size,
	currline, lnlen);

   bgcol = tl->bgcolor;
   if (currattr & ATTR_ISSET)	{
    if ((fgcol = (currattr & ATTR_COLOR_MASK)) == 0)
	fgcol = tl->fgcolor;
    if (currattr & ATTR_SELECT) {
	i = fgcol;
	fgcol = bgcol;
	bgcol = i;		}
				}
   else
	fgcol = tl->fgcolor;

   if ((currpos >= tl->strlen) || (currofft + wc >= w))	{
	wc = w - currofft;
	fl_rectf(currofft ? x + currofft : x - TEXT_X_OFFSET, y,
		currofft ? wc : w + TEXT_X_OFFSET, h, bgcol);
							}
   else
	fl_rectf(currofft ? x + currofft : x - TEXT_X_OFFSET, y,
		currofft ? wc : wc + TEXT_X_OFFSET, h, bgcol);

   xc = x + currofft;

   fl_textcolor(fgcol);
fl_cur_win = fl_winget();
fl_fheight = fl_get_char_height(spec->text_style, spec->text_size, &fl_fasc, &fl_fdesc);
   XDrawString(fl_display, fl_cur_win, /*fl_textgc*/fl_state[fl_vmode].gc[0], xc,
		y + fl_fheight - fl_fdesc, currline, lnlen);

   if ((currattr & ATTR_ISSET) && (currattr & ATTR_UNDERLINE))	{
	fl_color(fgcol);
fl_fheight = fl_get_char_height(spec->text_style, spec->text_size, &fl_fasc, &fl_fdesc);
	fl_textedit_underline_text(obj,
			xc, y + fl_fheight - fl_fdesc, sw);	}

   if ((cpos < currpos) && (cpos >= (currpos - lnlen)))	{
	i = fl_get_string_width(spec->text_style, spec->text_size,
		currline, cpos - currpos + lnlen);
	fl_textedit_draw_cursor(obj, xc + i, y);
							}

   tl->buf[currpos] = svchar;

   if ((currpos >= tl->strlen) || (currofft + wc >= w))	{
	if ((cpos == currpos) && (currofft + sw < w))
	  fl_textedit_draw_cursor(obj, xc + sw, y);
	break;						}

   currofft += wc;
   currline = tl->buf + currpos;
   currattr = tl->attr[currpos];
   switch (seltype)	{
    case 0:
     if ((currpos >= sels) && (currpos < sele))	{
      if (currattr & ATTR_ISSET)
	currattr |= ATTR_SELECT;
      else
	currattr = ATTR_SELECT|ATTR_ISSET;	}
    break;

    case 1:
     if (currattr & ATTR_ISSET)
	currattr |= ATTR_SELECT;
     else
	currattr = ATTR_SELECT|ATTR_ISSET;
    break;
			}

		}

 return;
}

static void
fl_textedit_draw_screen(FL_OBJECT *ob)
/*
 * Desc: refresh the screen
 */
{
    int i, eof, pos, lh, sels, sele, linenum;
    int tx, ty, tw, th;		/* textarea coordinates */
    SPEC *spec = (SPEC *)ob->spec;
    TextBuf *tb = &spec->tb;
    TextLine *tl;

    if (ob->form->frozen)
	return;

    fl_textedit_get_textbb(ob, &tx, &ty, &tw, &th);
    fl_drw_frame(ob->boxtype, ob->x + abs(ob->bw), ob->y + abs(ob->bw), tw + TEXT_X_OFFSET, 
		 ob->h - 2*abs(ob->bw), ob->col2, abs(ob->bw));

    /* draw the text */
    i = 0;
    tb_set_current_line(tb, spec->topline);
    eof = (tb->currentline == NULL);

    fl_set_text_clipping(tx, ty, tw, th);
    while (!eof & (i < spec->wsize)) {
	linenum = i + spec->topline;
	/* draw the cursor at the right position */
	if ((linenum == spec->r) && ob->focus &&
		!(spec->flags & FL_TEXTEDIT_NOCUR))
	    pos = spec->c;
	else
	    pos = -1;

	sels = -1;
	sele = -1;
	/* calculate selection */
	if (fl_textedit_selected(ob) &&
		(linenum >= spec->sselr) &&
		(linenum <= spec->eselr))	{
	  sels = 0;
	  if (linenum == spec->sselr)
		sels = spec->sselc;

	  if (linenum == spec->eselr)
		sele = spec->eselc;
						}

	fl_textedit_draw_textline(ob, tb->currentline, FL_ALIGN_TOP_LEFT, tx, ty + i*spec->charheight, tw, spec->charheight, pos, sels, sele);
	eof = !tb_set_next_line(tb);
	i++;
    }
    fl_unset_text_clipping();

    lh = i*spec->charheight;
    fl_rectf(tx - TEXT_X_OFFSET, ty + lh, 
	 tw+TEXT_X_OFFSET, ob->h - 2*abs(ob->bw) - lh , ob->col2);

    if ((i == 0) && ob->focus && !(spec->flags & FL_TEXTEDIT_NOCUR))
	fl_textedit_draw_cursor(ob, tx, ty);

    /* restore the current linenr */
    tb_set_current_line(tb, spec->r);

    tl = tb->firstline;
    while (tl)	{
	tl->flags &= ~TLINE_MODIFIED;
	tl = tl->next;
		}

    return;
} /* fl_textedit_draw_screen() */

void
fl_textedit_draw_line(FL_OBJECT *ob, int n)
/*
 * Desc: redraw the n-th line
 * Pre:  n<tb_get_lines(ob->spec->tb)
 */
{
    SPEC *spec = (SPEC *)ob->spec;
    TextBuf *tb = &spec->tb;
    TextLine *tl;
    int	tx, ty, tw, th;		/* bounding box of line */
    int x, y, w, h, sels, sele;

    if (ob->form->frozen)
	return;

    /* check if n is valid */
    if (n < 0 || n >= tb_get_nlines(tb))
	return;
    
    th = spec->charheight;
    if ((n < spec->topline) || (n >= spec->topline + spec->wsize))
	return;

    /* get the line */
    if ((tl = tb_get_lineptr_by_num(tb, n)) == NULL)
	return;

    /* get bounding box of line */
    fl_textedit_get_textbb(ob, &x, &y, &w, &h);
    tw = w;
    tx = x;
    ty = y + (n - spec->topline) * th;
    fl_set_text_clipping(tx, ty, tw, th);

    sels = -1;
    sele = -1;
    /* calculate selection */
    if (fl_textedit_selected(ob) &&
     (n >= spec->sselr) &&
     (n <= spec->eselr))	{
	sels = 0;
	if (n == spec->sselr)
		sels = spec->sselc;

	if (n == spec->eselr) {
		sele = spec->eselc;
		if (tl->strlen && (sels == sele)) {
		 sels = -1;
		 sele = -1;	  }
			      }
				}

    if ((spec->r == n) && ob->focus && !(spec->flags & FL_TEXTEDIT_NOCUR)) {
	if (spec->c > tl->strlen)
		spec->c = tl->strlen;
	fl_textedit_draw_textline(ob, tl, FL_ALIGN_TOP_LEFT, tx, ty, tw, th, 
				  spec->c, sels, sele);
    } else
	fl_textedit_draw_textline(ob, tl, FL_ALIGN_TOP_LEFT, tx, ty, tw, th,
				  -1, sels, sele);
    
    tl->flags &= ~TLINE_MODIFIED;
    fl_unset_text_clipping();

    return;
} /* fl_textedit_draw_line() */

void
fl_textedit_refresh_screen(FL_OBJECT *ob, int bclear)
/*
 * Desc: update screen. Draw only lines that require update
 *       bclear != 0 means that if screen is not filled with text
 *       then we should "erase" the empty bottom part
 */
{
    SPEC *spec = (SPEC *)ob->spec;
    TextBuf *tb = &spec->tb;
    TextLine *tl;
    int line;
    int x, y, w, h;

    tl = tb->firstline;
    line = 0;
    while (tl) 	{
     if (!(tl->flags & TLINE_MODIFIED))	{
	tl = tl->next;
	line++;
	continue;			}

     fl_textedit_draw_line(ob, line);
     tl->flags &= ~TLINE_MODIFIED;

     tl = tl->next;
     line++;
		}

   if (bclear)	{
    if (tb->n >= (spec->topline + spec->wsize))
	return;
    line = tb->n - spec->topline;
    if (line < 0)
	return;
    fl_textedit_get_textbb(ob, &x, &y, &w, &h);
    y += (spec->charheight * line);
    h -= (spec->charheight * line);
    fl_rectf(x - TEXT_X_OFFSET, y, w + TEXT_X_OFFSET, h, ob->col2);
    if ((line == 0) && (spec->leftcol == 0))
     fl_textedit_draw_cursor(ob, x, y);
		}

   return;
}

void
fl_textedit_set_topline(FL_OBJECT *ob, int topline, int setsb)
/*
 * Desc: redraw screen when the topline has been changed
 */
{
SPEC *spec = (SPEC *)ob->spec;
TextBuf *tb = &spec->tb;
int delta = spec->topline - topline;
int x, y, w, h, bh, line, start;
int newr = spec->r, newc = spec->c;

  if (topline >= tb->n)
	topline = tb->n - 1;

  if (topline == spec->topline)	{
   fl_textedit_refresh_screen(ob, 0);
   return;			}

  if (FL_abs(delta) > (2 * spec->wsize / 3))	{
   spec->topline = topline;
   for (line = 0; line < spec->wsize; line++)
     fl_textedit_draw_line(ob, line + spec->topline);
						}
  else
  if (spec->topline > topline)	{
   bh = (spec->wsize - delta) * spec->charheight;
   spec->topline = topline;
   fl_textedit_get_textbb(ob, &x, &y, &w, &h);
   XCopyArea(fl_display, FL_ObjWin(ob),
	FL_ObjWin(ob), fl_gc,
	x - TEXT_X_OFFSET, y, w + TEXT_X_OFFSET, bh,
	x - TEXT_X_OFFSET, y + delta * spec->charheight);
   for (line = 0; line < delta; line++)
     fl_textedit_draw_line(ob, line + spec->topline);
				}
  else
  if (spec->topline < topline)	{
   delta = topline - spec->topline;
   bh = (spec->wsize - delta) * spec->charheight;
   spec->topline = topline;
   fl_textedit_get_textbb(ob, &x, &y, &w, &h);
   XCopyArea(fl_display, FL_ObjWin(ob),
	FL_ObjWin(ob), fl_gc,
	x - TEXT_X_OFFSET, y + delta * spec->charheight, w + TEXT_X_OFFSET,
	bh, x - TEXT_X_OFFSET, y);
   start = spec->wsize - delta + spec->topline;
   for (line = 0; line < delta; line++)
     fl_textedit_draw_line(ob, line + start);
				}

  fl_textedit_refresh_screen(ob, 1);
  if (setsb)
	fl_textedit_set_vscrollbar(ob);
  if (spec->r < topline)
	newr = topline;
  else
   if (spec->r >= (topline + spec->wsize))
	newr = topline + spec->wsize - 1;

  if (newr != spec->r)	{
	tb_set_current_line(tb, newr);
	newc = spec->cpos;
	if (newc > tb_get_linelen(tb))
		newc = tb_get_linelen(tb);
	fl_textedit_movecursor(ob, newr, newc);
			}
}

void
fl_textedit_draw_selection(FL_OBJECT *ob)
/*
 * Desc: redraw the selected text
 */
{
    SPEC *spec = (SPEC *)ob->spec;
    int i;

    if ((spec->sselr < 0) ||
	(spec->eselr < 0))
	return;

    if (spec->sselr == spec->eselr)
	fl_textedit_draw_line(ob, spec->sselr);
    else {
     for (i = spec->sselr; i <= spec->eselr; i++)
	fl_textedit_draw_line(ob, i);
	 }
}

void
fl_textedit_search(FL_OBJECT *ob, int again)
/*
 * Desc: search for text
 */
{
    SPEC *spec = (SPEC *)ob->spec;
    TextBuf	 *tb = &spec->tb;
    char *p;
    int line, pos, n;

    if (again)	{
     if (!spec->exp)	{
	fl_edit_error("No previous search pattern");
	fl_winset(ob->form->window);
	return;		}
		}
    else	{
	p = (char *)fl_show_input("Search for", spec->exp ? spec->exp : "");
	fl_winset(ob->form->window);
	if (spec->exp)
		free(spec->exp);
	if (!p || !strlen(p))	{
		spec->exp = NULL;
		return;		}
	spec->exp = strdup(p);
		}

    if ((line = tb_find_text(tb, spec->exp, spec->r, spec->c + 1, &pos)) == -1){
	fl_edit_error("Text not found");
	fl_winset(ob->form->window);
	return; 							       }

    fl_textedit_remove_selection(ob);
    spec->sselr = spec->eselr = line;
    spec->sselc = pos;
    spec->eselc = pos + strlen(spec->exp);
    n = spec->r;

    if (fl_textedit_line_visible(ob, line)) {
	fl_textedit_draw_line(ob, n);
	fl_textedit_draw_selection(ob);	    }
    else
	fl_textedit_set_topline(ob, line, 1);
    fl_textedit_movecursor(ob, line, pos);
}

void
fl_textedit_hscrollbar_dim(FL_OBJECT *ob)
{
SPEC *spec = (SPEC *)ob->spec;

 spec->hsb->x = ob->x;
 spec->hsb->y = ob->y + ob->h;
 if (spec->v_on)
  spec->hsb->w = ob->w + spec->vw;
 else
  spec->hsb->w = ob->w;
 spec->hsb->h = spec->hh;
 spec->hsb->resize = FL_RESIZE_NONE;
 spec->hsb->visible = spec->h_on;

 return;
}

void
fl_textedit_vscrollbar_dim(FL_OBJECT *ob)
{
SPEC *spec = (SPEC *)ob->spec;

 spec->sb->x = ob->x + ob->w;
 spec->sb->y = ob->y;
 spec->sb->w = spec->vw;
 spec->sb->h = ob->h;
 spec->sb->resize = FL_RESIZE_NONE;
 spec->sb->visible = spec->v_on;

 return;
}

int
fl_textedit_getvscrollbar(FL_OBJECT *ob)
/*
 * Desc: return position of vertical scrollbar in lines
 */
{
SPEC *spec = (SPEC *)ob->spec;
TextBuf	 *tb = &spec->tb;
float val = fl_get_scrollbar_value(spec->sb);

 return (tb->n > spec->wsize) ? val * (tb->n - spec->wsize) + 0.01 : 0;
}

int
fl_textedit_gethscrollbar(FL_OBJECT *ob)
/*
 * Desc: return position of horizontal scrollbar in characters
 */
{
SPEC *spec = (SPEC *)ob->spec;
TextBuf	 *tb = &spec->tb;
float val = fl_get_scrollbar_value(spec->hsb);

 return val * (float)(tb->maxchars - spec->csize) + (float)0.01;
}

void
fl_textedit_reset_vscrollbar(FL_OBJECT *ob)
{
SPEC *spec = (SPEC *)ob->spec;
TextBuf	 *tb = &spec->tb;
int delta = tb->n - spec->wsize;

 if (delta > 0)	{
  fl_set_scrollbar_size(spec->sb, (float)spec->wsize/tb->n);
  fl_set_scrollbar_value(spec->sb, (float)spec->topline/delta);
  fl_set_scrollbar_increment(spec->sb, (float)(spec->wsize - 0.99)/delta,
	(float)1.01/delta);
		}
 else
  fl_set_scrollbar_size(spec->sb, 1.0);
}

void
fl_textedit_reset_hscrollbar(FL_OBJECT *ob)
{
SPEC *spec = (SPEC *)ob->spec;
TextBuf	 *tb = &spec->tb;
int delta = tb->maxchars - spec->csize;

 if (delta > 0)	{
  fl_set_scrollbar_size(spec->hsb, (float)spec->csize/(float)tb->maxchars);
  fl_set_scrollbar_value(spec->hsb, (float)spec->leftcol/(float)delta);
  fl_set_scrollbar_increment(spec->hsb, (float)(spec->csize - 0.99)/(float)delta,
	(float)1.01/delta);
		}
 else
  fl_set_scrollbar_size(spec->hsb, 1.0);
}

int
fl_textedit_switch_hscrollbar(FL_OBJECT *ob)
/*
 * Desc: turn the scrollbar on/off according to the length of the
 *       longest line on the screen and screen width
 *       Returns 1 when scrollbar does not require redrawing
 */
{
SPEC *spec = (SPEC *)ob->spec;
TextBuf	 *tb = &spec->tb;

  if (!(spec->flags & FL_TEXTEDIT_HSBAR))
	return 1;

  if (!(spec->flags & FL_TEXTEDIT_HSBAR_AUTO))	{
	if (spec->h_on)	{
		spec->hsb->visible = 1;
		fl_textedit_reset_hscrollbar(ob);
			}
	return 0;				}

  if (spec->h_on) {
	if (tb->maxchars < spec->csize) 	{
		spec->hsb->visible = 0;
		spec->h_on = 0;
		ob->h += spec->hh;
		spec->wsize = (ob->h - 2*abs(ob->bw)) / spec->charheight;
		if (spec->v_on)
		 fl_textedit_vscrollbar_dim(ob);
		fl_redraw_object(ob);
		return 1;			}
	else	{
		spec->hsb->visible = 1;
		/* fl_textedit_reset_hscrollbar(ob); */
		}
			  }
  else	{
	spec->hsb->visible = 0;
	if (tb->maxchars >= spec->csize)	{
		fl_textedit_reset_hscrollbar(ob);
		spec->hsb->visible = 1;
		spec->h_on = 1;
		ob->h -= spec->hh;
		spec->wsize = (ob->h - 2*abs(ob->bw)) / spec->charheight;
		fl_textedit_hscrollbar_dim(ob);
		fl_redraw_object(spec->hsb);
		if (spec->v_on)	{
		 fl_textedit_vscrollbar_dim(ob);
		 fl_redraw_object(spec->sb);
				}
		return 1;
						}
	return 1;
	}

  return 0;
}

int
fl_textedit_switch_vscrollbar(FL_OBJECT *ob)
/*
 * Desc: turn the scrollbar on/off according to the number of lines 
 *       and screen size
 *       Returns 1 when scrollbar does not require redrawing
 */
{
SPEC *spec = (SPEC *)ob->spec;
TextBuf	 *tb = &spec->tb;

  if (!(spec->flags & FL_TEXTEDIT_VSBAR))
	return 1;

  if (!(spec->flags & FL_TEXTEDIT_VSBAR_AUTO))	{
	if (spec->v_on) {
		spec->sb->visible = 1;
		/* fl_textedit_reset_vscrollbar(ob); */
			}
	return 0;				}	

  if (spec->v_on) {
	if (tb->n <= spec->wsize) 	{
		ob->w += spec->vw;
		spec->csize = (ob->w - 2*abs(ob->bw) - 2*TEXT_X_OFFSET) / spec->charwidth;
		spec->sb->visible = 0;
		spec->v_on = 0;
		fl_redraw_object(ob);
		return 1;
				}
	else	{
/*
		fl_textedit_reset_vscrollbar(ob);
*/
		}
			  }
  else	{
	spec->sb->visible = 0;
	if (tb->n > spec->wsize)	{
		spec->sb->visible = 1;
		spec->v_on = 1;
		ob->w -= spec->vw;
		spec->csize = (ob->w - 2*abs(ob->bw) - 2*TEXT_X_OFFSET) / spec->charwidth;
		fl_textedit_vscrollbar_dim(ob);
		fl_textedit_reset_vscrollbar(ob);
		fl_redraw_object(spec->sb);
		return 1;
				}
	return 1;
	}

  return 0;
}

static void
fl_textedit_set_vscrollbar(FL_OBJECT *ob)
/*
 * Desc: set scrollbar value
 */
{
SPEC *spec = (SPEC *)ob->spec;
TextBuf	 *tb = &spec->tb;
int delta;

  if (!(spec->flags & FL_TEXTEDIT_VSBAR))
	return;

  if (fl_textedit_switch_vscrollbar(ob) != 0)
	return;

  delta = tb->n - spec->wsize;
  if (delta > 0)	{
   fl_set_scrollbar_value(spec->sb, (float)spec->topline/delta);
   fl_set_scrollbar_increment(spec->sb, (float)(spec->wsize - 0.99)/delta,
	(float)1.01/delta);
			}
  else
   fl_set_scrollbar_value(spec->sb, 0);

  return;
}

static void
fl_textedit_set_hscrollbar(FL_OBJECT *ob)
/*
 * Desc: set hscrollbar value
 */
{
SPEC *spec = (SPEC *)ob->spec;
TextBuf	 *tb = &spec->tb;
int delta;

 if (!(spec->flags & FL_TEXTEDIT_HSBAR))
	return;

 if (fl_textedit_switch_hscrollbar(ob) != 0)
	return;

 delta = tb->maxchars - spec->csize;
 if (delta > 0)	{
  fl_set_scrollbar_value(spec->hsb, (float)spec->leftcol/(float)delta);
  fl_set_scrollbar_increment(spec->hsb, (float)(spec->csize - 0.99)/(float)delta,
	(float)1.01/(float)delta);
		}
 else
  fl_set_scrollbar_value(spec->hsb, 0);

 return;
}

void
fl_textedit_set_vscrollbar_max(FL_OBJECT *ob)
/*
 * Desc: set scrollbar max value
 */
{
SPEC *spec = (SPEC *)ob->spec;
TextBuf	 *tb = &spec->tb;
int delta;

  if (!(spec->flags & FL_TEXTEDIT_VSBAR))
	return;

  if (fl_textedit_switch_vscrollbar(ob) != 0)
	return;

  delta = tb->n - spec->wsize;
  fl_set_scrollbar_size(spec->sb, (tb->n > 0) ? (float)spec->wsize/tb->n : 1.0);
  if (delta > 0)
   fl_set_scrollbar_increment(spec->sb, (float)(spec->wsize - 0.99)/delta,
	(float)1.01/delta);

  return;
}

void
fl_textedit_set_hscrollbar_max(FL_OBJECT *ob)
/*
 * Desc: set hscrollbar max value
 */
{
SPEC *spec = (SPEC *)ob->spec;
TextBuf	 *tb = &spec->tb;
int delta;

 if (!(spec->flags & FL_TEXTEDIT_HSBAR))
	return;

 if (fl_textedit_switch_hscrollbar(ob) != 0)
	return;

 delta = tb->maxchars - spec->csize;
 fl_set_scrollbar_size(spec->hsb, (tb->maxchars > 0) ? (float)spec->csize/(float)tb->maxchars : 1.0);
 if (delta > 0)
  fl_set_scrollbar_increment(spec->hsb, (float)(spec->csize - 0.99)/(float)delta,
	(float)1.01/(float)delta);

 return;
}

void
fl_textedit_set_vscrollbar_wsize(FL_OBJECT *ob)
/*
 * Desc: set scrollbar wsize value
 */
{
  fl_textedit_set_vscrollbar_max(ob);
  return;
}

void
fl_textedit_set_hscrollbar_wsize(FL_OBJECT *ob)
/*
 * Desc: set hscrollbar wsize value
 */
{
  fl_textedit_set_hscrollbar_max(ob);
  return;
}

/***********************************************************************
 *
 * Event handling
 *
 ***********************************************************************/

void
fl_textedit_scroll_with_mouse(FL_OBJECT *ob, int key)
/*
 * Desc: scroll the text while the mouse button <key> is being
 *	 held down
 */
{
    int		 mx, my, oldmx, oldmy, lastmy;
    unsigned int keymask;
    int 	 button, nlines, maxlines;
    SPEC	 *spec = (SPEC *)ob->spec;
    TextBuf	 *tb = &spec->tb;
    int 	 oldtopline = spec->topline;
    int		 lineskip = 4;
    int		 newtopline;

    button = Button1Mask;
    switch (key) {
    case 1: button = Button1Mask; break;
    case 2: button = Button2Mask; break;
    case 3: button = Button3Mask; break;
    }

    /* Query the pointer and move the text around */
    fl_get_mouse(&oldmx, &oldmy, &keymask);
    lastmy = oldmy;
    while (keymask & button) {
	fl_get_mouse(&mx, &my, &keymask);
	if (abs(lastmy-my) >= lineskip) {
	    nlines = (oldmy-my) / lineskip;
	    newtopline = oldtopline + nlines;
	    maxlines = tb_get_nlines(&spec->tb);
	    if (newtopline < 0)
		newtopline = 0;
	    if (newtopline >= maxlines)
		newtopline = maxlines - 1;
	    spec->r = spec->topline;
	    tb_set_current_line(tb, spec->r);
	    if (spec->c > tb_get_linelen(tb)) {
		spec->c = tb_get_linelen(tb);
	    }
	    fl_textedit_set_topline(ob, newtopline, 1);
	    fl_textedit_set_cursor(ob, mx, my, 1);
	    lastmy = my;
	}
	else
	    fl_textedit_set_cursor(ob, mx, my, 1);
    }

    return;
} /* fl_textedit_scroll_with_mouse() */

void
fl_textedit_cleft(FL_OBJECT *ob)
/*
 * Desc: move the whole page one character left
 */
{
    SPEC *spec = (SPEC *)ob->spec;

    if (spec->leftcol == 0)
	return;

    spec->leftcol--;
    spec->c = spec->leftcol;
    fl_textedit_draw_screen(ob);
    fl_textedit_set_hscrollbar(ob);
}

void
fl_textedit_cright(FL_OBJECT *ob)
/*
 * Desc: move the whole page one character right
 */
{
    SPEC *spec = (SPEC *)ob->spec;
    TextBuf *tb = &spec->tb;

    if (spec->leftcol >= tb->maxchars)
	return;

    spec->leftcol++;
    spec->c = spec->leftcol + spec->csize;
    fl_textedit_draw_screen(ob);
    fl_textedit_set_hscrollbar(ob);
}

static void
fl_textedit_pagedown(FL_OBJECT *ob)
/*
 * Desc: move the cursor one page down
 */
{
    SPEC *spec = (SPEC *)ob->spec;
    TextBuf *tb = &spec->tb;
    int newtopline, newr, newc;

    if (tb_get_nlines(tb) <= (spec->wsize + spec->topline)) 	{
	tb_set_current_line(tb, tb_get_nlines(tb) - 1);
	if (spec->cpos > tb_get_linelen(tb))
		spec->cpos = tb_get_linelen(tb);
	fl_textedit_movecursor(ob, tb_get_nlines(tb) - 1, spec->cpos);
	return;						}

    newc = spec->cpos;
    newr = spec->r + spec->wsize;
    newtopline = spec->topline + spec->wsize;

    if (tb_get_nlines(tb) <= (spec->wsize + newtopline))
	newtopline = tb_get_nlines(tb) - spec->wsize;

    if (newr >= tb_get_nlines(tb))
	newr = tb_get_nlines(tb) - 1;
    tb_set_current_line(tb, newr);

    if (newc > tb_get_linelen(tb))
	newc = tb_get_linelen(tb);

    fl_textedit_set_topline(ob, newtopline, 1);
    fl_textedit_movecursor(ob, newr, newc);
    tb_set_current_line(tb, spec->r);

    if (spec->cur_callback) (spec->cur_callback)(ob, tb_return_line(tb), 0, spec->r, spec->c);
    return;
} /* fl_textedit_pagedown() */

static void
fl_textedit_pageup(FL_OBJECT *ob)
/*
 * Desc; move the cursor one page up
 */
{
    SPEC *spec = (SPEC *)ob->spec;
    TextBuf *tb = &spec->tb;
    int newtopline = spec->topline, newr = 0, newc = spec->cpos;

    if (spec->topline == 0) 	{
	tb_set_current_line(tb, 0);
	if (spec->cpos > tb_get_linelen(tb))
		spec->cpos = tb_get_linelen(tb);
	fl_textedit_movecursor(ob, 0, spec->cpos);
	return;
				}

    if (spec->r > spec->wsize - 1)
	newr = spec->r - spec->wsize;
    tb_set_current_line(tb, newr);

    if (spec->topline > newr)
	newtopline = newr;

    if (newc > tb_get_linelen(tb))
	newc = tb_get_linelen(tb);

    fl_textedit_set_topline(ob, newtopline, 1);
    fl_textedit_movecursor(ob, newr, newc);
    tb_set_current_line(tb, spec->r);

    if (spec->cur_callback) (spec->cur_callback)(ob, tb_return_line(tb), 0, spec->r, spec->c);

    return;
} /* fl_textedit_pageup() */

void 
fl_textedit_linedown(FL_OBJECT *ob)
/*
 * Desc: move the cursor one line down
 */
{
    SPEC *spec = (SPEC *)ob->spec;
    TextBuf *tb = &spec->tb;
    int newc;

    if ((spec->r < (spec->topline + spec->wsize - 1)) &&
	(spec->r < (tb_get_nlines(tb) - 1))) {
	tb_set_next_line(tb);
	fl_textedit_movecursor(ob, spec->r + 1,
		spec->cpos >= tb_get_linelen(tb) ? tb_get_linelen(tb) : spec->cpos);
    } else {
	/* scroll the text down by one line */
	if (spec->r < tb_get_nlines(tb) - 1) {
	    tb_set_next_line(tb);
	    newc = spec->cpos;
	    if (newc >= tb_get_linelen(tb))
		newc = tb_get_linelen(tb);
	    fl_textedit_set_topline(ob, spec->topline + 1, 1);
	    fl_textedit_movecursor(ob, spec->r + 1, newc);
	    if (spec->cur_callback) (spec->cur_callback)(ob, tb_return_line(tb), 0, spec->r, spec->c);
					   }
	else
		fl_textedit_movecursor(ob, spec->r, tb_get_linelen(tb));
	   }
    
    return;
} /* fl_textedit_linedown() */

void
fl_textedit_lineup(FL_OBJECT *ob)
/*
 * Desc: move the cursor one line up
 */
{
    SPEC *spec = (SPEC *)ob->spec;
    TextBuf *tb = &spec->tb;
    int newc = spec->cpos;

    if (spec->r > spec->topline) {
	tb_set_prev_line(tb);
	fl_textedit_movecursor(ob, spec->r - 1,
		spec->cpos >= tb_get_linelen(tb) ? tb_get_linelen(tb) : spec->cpos);
				 }
    else	{
	/* scroll the text up by one line */
	if (spec->topline > 0) 	{
	    tb_set_prev_line(tb);
	    newc = spec->cpos;
	    if (newc >= tb_get_linelen(tb))
		newc = tb_get_linelen(tb);
	    fl_textedit_set_topline(ob, spec->topline - 1, 1);
	    fl_textedit_movecursor(ob, spec->r - 1, newc);
				}
		}

    return;
} /* fl_textedit_lineup() */

void
fl_textedit_wordleft(FL_OBJECT *ob)
/*
 * Desc: move the cursor one word left
 */
{
SPEC *spec = (SPEC *)ob->spec;
TextBuf *tb = &spec->tb;
char *p, *line;
int oldr;

    line = tb_return_line(tb);
    oldr = spec->r;

    if ((spec->c > 0) && line && *line) {
     /* one world left */
     if (spec->c >= strlen(line))
	p = line + strlen(line) - 1;
     else
	p = line + spec->c;

     p--;
     while ((p > line) && (*p == ' '))
	p--;
     if ((p >= line) && (*p != ' ')) {
      while ((p > line) && (*p != ' '))
	p--;
      if (*p == ' ')
	p++;
      fl_textedit_movecursor_visible(ob, spec->r, p - line);
      return;	    		     }
					}

   /* one line up */
   fl_textedit_lineup(ob);
   line = tb_return_line(tb);
   if ((oldr != spec->r) && line && *line) {
     p = line + strlen(line) - 1;
     while ((p > line) && (*p == ' '))
	p--;
     while ((p > line) && (*p != ' '))
	p--;
     if (*p == ' ')
	p++;
     fl_textedit_movecursor_visible(ob, spec->r, p - line);
					   }

   return;
}

void
fl_textedit_wordright(FL_OBJECT *ob)
/*
 * Desc: move the cursor one word right
 */
{
SPEC *spec = (SPEC *)ob->spec;
TextBuf *tb = &spec->tb;
char *p, *line;
int oldr;

    oldr = spec->r;
    line = tb_return_line(tb);
    if (line && *line && (spec->c <= strlen(line))) {
     /* one word right */
     if ((p = strchr(line + spec->c, ' ')) != NULL) 	{
      while (*p && (*p == ' '))
	p++;
      if (*p)	{
	fl_textedit_movecursor_visible(ob, spec->r, p - line);
	return; }
					  		}
						    }

   /* line down */
   fl_textedit_linedown(ob);
   line = tb_return_line(tb);
   if ((oldr != spec->r) && line) {
      p = line;
      while (*p && (*p == ' '))
	p++;
     fl_textedit_movecursor_visible(ob, spec->r, p - line);
				  }

   return;
}

void
fl_textedit_delwordleft(FL_OBJECT *ob)
/*
 * Desc: delete one word at the left of the cursor
 */
{
SPEC *spec = (SPEC *)ob->spec;
TextBuf *tb = &spec->tb;
char *p, *line;
int oldnum, lpos;

    if ((line = tb_return_line(tb)) == NULL)
	return;

    oldnum = tb->n;

    if (spec->c > strlen(line))
	p = line + strlen(line) - 1;
    else
	p = line + spec->c;

    if (p > line)
	p--;
    while ((p > line) && (*p == ' '))
	p--;
    while ((p > line) && (*p != ' '))
	p--;
    lpos = p - line;
    tb_del_block(tb, spec->r, lpos, spec->r, spec->c);
    if (oldnum == tb->n) {
	fl_textedit_draw_line(ob, spec->r);
	fl_textedit_movecursor_visible(ob, spec->r, lpos);
			 }
    else	{
	fl_textedit_refresh_screen(ob, 1);
	fl_textedit_lineup(ob);
	line = tb_return_line(tb);
	fl_textedit_movecursor_visible(ob, spec->r, line ? strlen(line) : 0);
		}

    return;
}

void
fl_textedit_delwordright(FL_OBJECT *ob)
/*
 * Desc: delete one word at the right of the cursor
 */
{
SPEC *spec = (SPEC *)ob->spec;
TextBuf *tb = &spec->tb;
char *p, *line;
int oldnum, lpos;

    if ((line = tb_return_line(tb)) == NULL)
	return;

    oldnum = tb->n;

    if ((p = strchr(line + spec->c, ' ')) != NULL) 	{
      while (*p && (*p == ' '))
	p++;						}
    else
      p = line + strlen(line);

    lpos = p - line;
    tb_del_block(tb, spec->r, spec->c, spec->r, lpos);
    if (oldnum == tb->n)
	fl_textedit_draw_line(ob, spec->r);
    else	{
	fl_textedit_refresh_screen(ob, 1);
	if (fl_textedit_movecursor_visible(ob, spec->r, 0) < 0)
		fl_textedit_lineup(ob);
		}

    return;
}

void
fl_textedit_handle_keyboard(FL_OBJECT *ob, int key, void *xev)
/*
 * Desc: handle the keyboard input 
 */
{
static int digraph_mode = 0, digraph_key = 0;
#ifdef ALLOW_DEAD_KEYS
static int accent_state = 0;
unsigned char *ab = NULL, *ap;
#endif

SPEC *spec = (SPEC *)ob->spec;
TextBuf *tb = &spec->tb;
int n, func, oldc = spec->c, oldr = spec->r, oldmc = tb->maxchars;
long mod;
struct _digraph *dg;
XKeyEvent *xkev = (XKeyEvent *)xev;

#ifdef ALLOW_DEAD_KEYS
    if (accent_state)	{
      ab = accents[accent_state];
      if (ab && (ap = strchr(ab, tolower(key)))) {
	if ((ab - ap) % 2 == 0)
	  key = isupper(key) ? (ap[1] - ACCENT_UPPER_DIFF) : ap[1];
						 }
#if defined(XKBE_DEAD_KEYS) && defined(ALLOW_DEAD_KEYS)
      else
	key = accent_chars[accent_state];
#endif

      accent_state = 0;
			}
     else {
      switch(key)	{
#ifdef	XK_dead_acute
       case XK_dead_acute:
#endif
#ifdef	XK_dead_grave
       case XK_dead_grave:
#endif
#ifdef	XK_dead_tilde
       case XK_dead_tilde:
#endif
#ifdef	XK_dead_abovering
       case XK_dead_abovering:
#endif
#ifdef	XK_dead_doubleacute
       case XK_dead_doubleacute:
#endif
#ifdef	XK_dead_circumflex
       case XK_dead_circumflex:
#endif
#ifdef	XK_dead_macron
       case XK_dead_macron:
#endif
#ifdef	XK_dead_abovedot
       case XK_dead_abovedot:
#endif
#ifdef	XK_dead_diaeresis
       case XK_dead_diaeresis:
#endif
#ifdef	XK_dead_caron
       case XK_dead_caron:
#endif
#ifdef	XK_dead_cedilla
       case XK_dead_cedilla:
#endif
       case XK_VoidSymbol:
	accent_state = (key & 15) + 1;
	return;
       break;
#if defined(XKBE_DEAD_KEYS) && defined(ALLOW_DEAD_KEYS)
       default:
	ab = strchr(accent_chars, key);
	if (ab)	{
	  accent_state = (ab - accent_chars);
	  if (accent_chars[accent_state] == ' ')
		accent_state = 0;
	  else
		return;
		}
       break;
#endif
			}
    }
#endif

    n = 0;
    func = FL_TEXTKEY_NONE;
    while ((bindings[n].function != FL_TEXTKEY_ENDARRAY) &&
		(n < FL_TEXTKEY_MAX))	{
      if (key == (bindings[n].key & 0xffff)) {
	if (bindings[n].key & FL_ANY_MASK) {
	 func = bindings[n].function;
	 break;
					   }
	mod = 0;
	if (xkev->state & ControlMask)
	 mod |= FL_CONTROL_MASK;
	if (xkev->state & Mod1Mask)
	 mod |= FL_ALT_MASK;
	if (xkev->state & ShiftMask)
	 mod |= FL_SHIFT_MASK;

	if (mod != (bindings[n].key & FL_ALL_MASK)) 	{
		n++;
		continue;				}

	func = bindings[n].function;
	break;
					    }
      n++;
					}

    if (func == FL_TEXTKEY_DIGRAPH)	{
	digraph_mode = 1;
	digraph_key = 0;
	return;
					}
    else	{
     if (digraph_mode)	{
	if (func) {
	 digraph_mode = 0;
	 digraph_key = 0;
	 return;  }

	if (digraph_mode == 1)	{
	 digraph_mode = 0;
	 digraph_key = 0;
	 for (dg = &digraphs[0],n = 0;digraphs[n].key != 0;dg = &digraphs[++n])
	  if (dg->seq[0] == key)
		break;

	 if (digraphs[n].key == 0) {
	  for (dg = &digraphs[0],n = 0;digraphs[n].key != 0;dg = &digraphs[++n])
	   if (dg->seq[1] == key)
		break;		   }

	 if (digraphs[n].key == 0) {
		fl_ringbell(100);
		return;		   }

	 digraph_mode = 2;
	 digraph_key = key;
	 return;
				}
	else
	if (digraph_mode == 2)  {
	 digraph_mode = 0;
	 for (dg = &digraphs[0],n = 0;digraphs[n].key != 0;dg = &digraphs[++n])
	  if ((dg->seq[0] == digraph_key) &&
		(dg->seq[1] == key))
		break;

	 if (digraphs[n].key == 0) {
	  for (dg = &digraphs[0],n = 0;digraphs[n].key != 0;dg = &digraphs[++n])
	   if ((dg->seq[1] == digraph_key) &&
		(dg->seq[0] == key))
		break;
				   }

	 digraph_key = 0;
	 if (digraphs[n].key == 0) {
		fl_ringbell(100);
		return;		   }

	 key = digraphs[n].key;
				}
			}
		}

#ifdef FLTEXT_DEBUG
    printf("key character = %c (%d)\n", key, key);
    printf("ctrl is pressed = %d, function = 0x%02x\n",
		ControlMask & xkev->state, func);
#endif

    /* check if read-only */
    if (spec->flags & FL_TEXTEDIT_READONLY)	{
	if ((key > 26) && (key < 256))
		return;

	switch (key)	{
	 case '\n':
	 case '\r':
	 case '\t':
		return;
	 break;
			}

	if (func == FL_TEXTKEY_BACKSPACE)
		return;
						}

    /* check selection */
    if (fl_textedit_selected(ob))	{
	switch (func)	{
	 case FL_TEXTKEY_CUT:
			fl_textedit_copy(ob);
			if (spec->flags & FL_TEXTEDIT_READONLY)
				return;
	 case FL_TEXTKEY_BACKSPACE:	/* delete selected block */
	 case FL_TEXTKEY_DELETE:
	 case FL_TEXTKEY_DELWORDL:
	 case FL_TEXTKEY_DELWORDR:
		if (tb_del_block(tb, spec->sselr, spec->sselc,
			spec->eselr, spec->eselc)) {
			while (spec->sselr &&
				!tb_set_current_line(tb, spec->sselr))
				spec->sselr--;

			spec->r = spec->sselr;
			spec->c = spec->sselc;
			if (spec->c > tb_get_linelen(tb))
				spec->c = tb_get_linelen(tb);
			spec->sselr = -1;
			spec->eselr = -1;
			n = tb_get_nlines(tb);
			if (spec->topline > spec->r)
				spec->topline = spec->r;
			fl_textedit_refresh_screen(ob, 1);
			fl_textedit_set_vscrollbar(ob);
			fl_textedit_set_vscrollbar_max(ob);
			fl_textedit_set_hscrollbar_max(ob);
						   }
		else	{
			spec->c = spec->sselc;
			if (spec->c > tb_get_linelen(tb))
				spec->c = tb_get_linelen(tb);
			fl_textedit_remove_selection(ob);
			}

		if (spec->cur_callback) (spec->cur_callback)(ob, tb_return_line(tb), 0, spec->r, spec->c);
		return;
	 break;

	 /* leave the selection alone */
	 case FL_TEXTKEY_REFRESH:
	 case FL_TEXTKEY_FORMAT:
	 case FL_TEXTKEY_SLEFT:
	 case FL_TEXTKEY_SRIGHT:
	 case FL_TEXTKEY_SUP:
	 case FL_TEXTKEY_SDOWN:
	 case FL_TEXTKEY_COPY:
	 case FL_TEXTKEY_DEBUG:
	 break;

	 /* deselect everything unless we try to type something */
	 default:
		if ((key == '\t') || (key == '\r') ||
			((key > 26) && (key < 256))) {
			if (tb_del_block(tb, spec->sselr, spec->sselc,
				spec->eselr, spec->eselc)) {
				while (spec->sselr &&
					!tb_set_current_line(tb, spec->sselr))
					spec->sselr--;

				spec->r = spec->sselr;
				spec->c = spec->sselc;
				if (spec->c > tb_get_linelen(tb))
					spec->c = tb_get_linelen(tb);
				spec->sselr = -1;
				spec->eselr = -1;
				n = tb_get_nlines(tb);
				if (spec->topline > spec->r)
					spec->topline = spec->r;
				fl_textedit_set_vscrollbar(ob);
				fl_textedit_set_vscrollbar_max(ob);
				fl_textedit_set_hscrollbar_max(ob);
				fl_textedit_refresh_screen(ob, 1);
							   }
			else	{
				spec->c = spec->sselc;
				if (spec->c > tb_get_linelen(tb))
					spec->c = tb_get_linelen(tb);
				fl_textedit_remove_selection(ob);
				}
			if (spec->cur_callback) (spec->cur_callback)(ob, tb_return_line(tb), 0, spec->r, spec->c);
								  }
		else
			fl_textedit_remove_selection(ob);
	 break;
			}
					}
	else	{
	 switch (func)	{
	  /* start new selection */
	  case FL_TEXTKEY_SLEFT:
	  case FL_TEXTKEY_SRIGHT:
	  case FL_TEXTKEY_SUP:
	  case FL_TEXTKEY_SDOWN:
	    spec->sselr = spec->eselr = spec->r;
	    spec->sselc = spec->eselc = spec->c;
	  break;
			}
		}

    /* handle ctrl keys */
    if (key == '\n' || key == '\r')	{
	tb_insert_cr(tb, spec->c);	/* insert a newline at 
					   position spec->c */
	n = spec->r;
	tb_set_next_line(tb);
	spec->cpos = 0;
	if (spec->leftcol)			{
	   spec->r++;
	   spec->c = 0;
	   spec->leftcol = 0;
	   if ((spec->r + 1) >= (spec->topline + spec->wsize))
		spec->topline++;
	   fl_textedit_draw_screen(ob);		}
	else
	if ((spec->r + 1) >= (spec->topline + spec->wsize))	{
	    fl_textedit_set_topline(ob, spec->topline + 1, 1);
	    fl_textedit_movecursor(ob, n + 1, 0);		}
	else						{
	    fl_textedit_refresh_screen(ob, 0);
	    fl_textedit_movecursor(ob, n + 1, 0);	}
	fl_textedit_set_vscrollbar(ob);
	fl_textedit_set_vscrollbar_max(ob);
	if (spec->cur_callback) (spec->cur_callback)(ob, tb_return_line(tb), 0, spec->r, spec->c);
	return;
					}

    if (func == FL_TEXTKEY_BACKSPACE) {
	/* backspace */
	if (spec->c > 0) {
	    tb_del_char(tb, spec->c - 1);
	    fl_textedit_movecursor_visible(ob, spec->r, spec->c - 1);
	    fl_textedit_refresh_screen(ob, 1);
			 }
	else	{
	    if (spec->r > 0)	{
		tb_set_prev_line(tb);
		n = tb_get_linelen(tb);
		tb_del_char(tb, n);
		if (n > tb_get_linelen(tb))
			n = tb_get_linelen(tb);
		fl_textedit_movecursor_visible(ob, spec->r - 1, n);
		fl_textedit_set_vscrollbar_max(ob);
		fl_textedit_set_hscrollbar_max(ob);
		fl_textedit_refresh_screen(ob, 1);
				}
		}
	spec->cpos = spec->c;
	if (spec->cur_callback) (spec->cur_callback)(ob, tb_return_line(tb), 0, spec->r, spec->c);
	return;
    }
    if (func == FL_TEXTKEY_DELETE) {
	/* del key */
	n = tb_get_nlines(tb);
	if (tb_del_char(tb, spec->c)) {
	    if (spec->c > tb_get_linelen(tb))
		spec->c = tb_get_linelen(tb);
	    fl_textedit_refresh_screen(ob, 1);
	    /* check if a line got deleted */
	    if (n != tb_get_nlines(tb))
		fl_textedit_set_vscrollbar_max(ob);
	    fl_textedit_set_hscrollbar_max(ob);
	} else
	    fl_textedit_draw_line(ob, spec->r);
	return;
    }

    if (func != FL_TEXTKEY_NONE)
	goto kfuncs;

    /* handle character inserts */
    if ((key == '\t') || ((key > 26) && (key < 256) && (key != 127))) {
        /* one space will be removed from the line if line is wrapped */
	/* and word wrap is on and line had spaces */
	int mspace = (key == ' ') || ((tb->linewrap < 0) &&
		tb->currentline && strrchr(tb->currentline->buf, ' ')) ? 1 : 0;

	n = tb_insert_char(tb, spec->c, key);
	/* line was wrapped */
	if (n < 0)   {
	 /* cursor is beyond the end of the line */
	 n *= -1;
	 if ((spec->c + n) >= tb->currentline->strlen)	{
	  spec->cpos = spec->c + n - tb->currentline->strlen - mspace;
	  if (spec->cpos < 0)
		spec->cpos = 0;
	  fl_textedit_linedown(ob);			}
	 else
	  spec->c += n;

	 fl_textedit_set_vscrollbar_max(ob);
	 spec->cpos = spec->c;
	 if (spec->c < spec->leftcol)	{
	   spec->leftcol = 0;
	   fl_textedit_set_hscrollbar(ob);
	   if (spec->r >= spec->topline + spec->wsize)
		spec->topline++;
	   fl_textedit_draw_screen(ob);	}
	 else	{
	  if (spec->r >= spec->topline + spec->wsize)
	    fl_textedit_set_topline(ob, spec->topline + 1, 1);
		}
	 fl_textedit_refresh_screen(ob, 0);
						}
	else 	{
	 /* advance the cursor */
	 spec->cpos = spec->c;
	 fl_textedit_draw_line(ob, spec->r);
	 if (oldmc != tb->maxchars)
	   fl_textedit_set_hscrollbar_max(ob);
	 fl_textedit_movecursor_visible(ob, spec->r, spec->c + n);
		}
	if (spec->cur_callback) (spec->cur_callback)(ob, tb_return_line(tb), 0, spec->r, spec->c);
	return;
    }

kfuncs:
    /* handle function keys */
    switch(func) {
    case FL_TEXTKEY_HOME:
	    /* Go-Home was pressed, go to the beginning of the buffer */
	    if (fl_textedit_movecursor(ob, 0, 0) < 0)	{
	     if (spec->leftcol)		     {
		spec->topline = 0;
		spec->leftcol = 0;
		spec->r = 0;
		spec->c = 0;
		fl_textedit_set_vscrollbar(ob);
		fl_textedit_set_hscrollbar(ob);
		fl_textedit_draw_screen(ob); }
	     else	{
		fl_textedit_set_topline(ob, 0, 1);
	        fl_textedit_movecursor(ob, 0, 0);
			}
							}

	    spec->cpos = 0;
	return;
	break;
    case FL_TEXTKEY_LBEGIN:
	if (spec->leftcol) {
		spec->leftcol = 0;
		fl_textedit_draw_screen(ob);
		fl_textedit_set_hscrollbar(ob);
			   }

        fl_textedit_movecursor(ob, spec->r, 0);
       	spec->cpos = 0;

	return;
	break;
    case FL_TEXTKEY_END:
	    /* Goto-End was pressed, go to the end of the buffer */
	    /* check if the last line is visible */
	    if (tb_get_nlines(tb) <= (spec->wsize + spec->topline)) 	{
		fl_textedit_movecursor(ob, tb_get_nlines(tb) - 1, spec->c);
		return;						}
	    if (spec->leftcol)	{
	     spec->r = tb_get_nlines(tb) - 1;
	     if (spec->r < 0)
		spec->r = 0;
	     spec->c = 0;
	     spec->topline = tb_get_nlines(tb) - spec->wsize;
	     spec->leftcol = 0;
	     fl_textedit_set_vscrollbar(ob);
	     fl_textedit_set_hscrollbar(ob);
	     fl_textedit_draw_screen(ob);
				}
	    else	{
	     fl_textedit_set_topline(ob, tb_get_nlines(tb) - spec->wsize, 1);
	     fl_textedit_movecursor(ob, tb_get_nlines(tb) ? tb_get_nlines(tb) - 1 : 0, 0);
			}
	    spec->cpos = 0;
	return;
	break;
    case FL_TEXTKEY_LEND:
	if (fl_textedit_movecursor(ob, spec->r, tb_get_linelen(tb)) == -1) {
		spec->leftcol = tb_get_linelen(tb) - spec->csize + 1;
		fl_textedit_set_hscrollbar(ob);
		fl_textedit_draw_screen(ob);	}
	spec->cpos = spec->c;
	return;
	break;
    case FL_TEXTKEY_SLEFT:
    case FL_TEXTKEY_LEFT:
	if (spec->flags & FL_TEXTEDIT_READONLY) {
		fl_textedit_cleft(ob);
		return;				}

	if (spec->c > 0)
	    fl_textedit_movecursor_visible(ob, spec->r, spec->c - 1);
	else	{
	    if (spec->r > 0)	{
		fl_textedit_lineup(ob);
		fl_textedit_movecursor_visible(ob, spec->r, tb_get_linelen(tb));
				}
		}
	spec->cpos = spec->c;
	if (func == FL_TEXTKEY_SLEFT)
		fl_textedit_extend_selection(ob, oldr, oldc);
	return;
	break;
    case FL_TEXTKEY_SRIGHT:
    case FL_TEXTKEY_RIGHT:
	if (spec->flags & FL_TEXTEDIT_READONLY) {
		fl_textedit_cright(ob);
		return;				}

	if (spec->c < tb_get_linelen(tb)) {
	    fl_textedit_movecursor_visible(ob, spec->r, spec->c + 1);
	} else {
	    if (spec->r < tb_get_nlines(tb)-1) {
		fl_textedit_linedown(ob);
		fl_textedit_movecursor_visible(ob, spec->r, 0);
	    }
	}
	spec->cpos = spec->c;
	if (func == FL_TEXTKEY_SRIGHT)
		fl_textedit_extend_selection(ob, oldr, oldc);
	return;
	break;
    case FL_TEXTKEY_SUP:
    case FL_TEXTKEY_UP:
	/* move line up */
	if (spec->flags & FL_TEXTEDIT_READONLY) {
	 if (spec->topline > 0) 	{
	    fl_textedit_set_topline(ob, spec->topline - 1, 1);
	    fl_textedit_movecursor(ob, spec->topline, 0);
	    tb_set_current_line(tb, spec->topline);
					}

	 return;				}

	fl_textedit_lineup(ob);
	if (func == FL_TEXTKEY_SUP)
		fl_textedit_extend_selection(ob, oldr, oldc);
	return;
	break;
    case FL_TEXTKEY_SDOWN:
    case FL_TEXTKEY_DOWN:
	/* move line down */
	if (spec->flags & FL_TEXTEDIT_READONLY) {
	 if (spec->topline < tb_get_nlines(tb)-1) 	{
	    fl_textedit_set_topline(ob, spec->topline + 1, 1);
	    fl_textedit_movecursor(ob, spec->topline, 0);
	    tb_set_current_line(tb, spec->topline);	}
	 return;				}

	fl_textedit_linedown(ob);
	if (func == FL_TEXTKEY_SDOWN)
		fl_textedit_extend_selection(ob, oldr, oldc);
	return;
	break;
    case FL_TEXTKEY_PAGEDOWN:
	/* pagedown */
	fl_textedit_pagedown(ob);
	return;
	break;
    case FL_TEXTKEY_PAGEUP:
	/* pageup */
	fl_textedit_pageup(ob);
	return;
	break;
    case FL_TEXTKEY_LKILL:
	if (spec->flags & FL_TEXTEDIT_READONLY)
		return;
	/* delete to end of line */
	if (spec->c < tb_get_linelen(tb)) {
		tb_set_linelen(tb, spec->c);
		fl_textedit_refresh_screen(ob, 0);
		return;			  }
	else	{
	 n = tb_get_nlines(tb);
	 tb_del_char(tb, spec->c);
	 if (spec->c > tb_get_linelen(tb))
		spec->c = tb_get_linelen(tb);
	 fl_textedit_refresh_screen(ob, 1);
	 /* check if a line got deleted */
	 if (n != tb_get_nlines(tb))
		fl_textedit_set_vscrollbar_max(ob);
	 fl_textedit_set_hscrollbar_max(ob);
		}
	break;
    case FL_TEXTKEY_REFRESH:
	/* refresh screen */
	fl_textedit_switch_vscrollbar(ob);
	fl_textedit_switch_hscrollbar(ob);
	fl_textedit_draw_screen(ob);
	fl_redraw_object(spec->sb);
	fl_redraw_object(spec->hsb);
	return;
	break;
    case FL_TEXTKEY_LCLEAR:
	if (spec->flags & FL_TEXTEDIT_READONLY)
		return;
	/* clear line */
	tb_set_linelen(tb, 0);
	spec->c = 0;
	spec->cpos = 0;
	fl_textedit_refresh_screen(ob, 0);
	return;
	break;
    case FL_TEXTKEY_CUT:
	if (spec->flags & FL_TEXTEDIT_READONLY)
		return;
	return;
	break;
    case FL_TEXTKEY_COPY:
	fl_textedit_copy(ob);
	return;
	break;
    case FL_TEXTKEY_PASTE:
	if (spec->flags & FL_TEXTEDIT_READONLY)
		return;
	spec->paster = spec->r;
	spec->pastec = spec->c;
	fl_textedit_paste(ob);
	return;
	break;
    case FL_TEXTKEY_LDELETE:
	if (spec->flags & FL_TEXTEDIT_READONLY)
		return;
	/* delete line */
	tb_del_line(tb);
	n = tb_get_nlines(tb);
	if (n == 0)
		spec->r = 0;
	else
	if (spec->r >= n)
		spec->r = n - 1;
	if (spec->c > tb_get_linelen(tb))
		spec->c = tb_get_linelen(tb);
	fl_textedit_set_vscrollbar_max(ob);
	spec->cpos = spec->c;
	fl_textedit_refresh_screen(ob, 1);
	if (spec->cur_callback) (spec->cur_callback)(ob, tb_return_line(tb), 0, spec->r, spec->c);
	return;
	break;
   case FL_TEXTKEY_CLEAR:
	if (spec->flags & FL_TEXTEDIT_READONLY)
		return;
	/* clear screen */
	tb_clear(tb);
	tb->bufchanged = TRUE;
	spec->c = 0;
	spec->r = 0;
	spec->sselr = spec->sselc = spec->eselr = spec->eselc = -1;
	spec->cpos = spec->topline = 0;
	spec->leftcol = 0;
	fl_textedit_set_hscrollbar_max(ob);
	fl_textedit_set_vscrollbar_max(ob);
	fl_textedit_draw_screen(ob);
	if (spec->cur_callback) (spec->cur_callback)(ob, tb_return_line(tb), 0, spec->r, spec->c);
	return;
	break;
  case FL_TEXTKEY_FIND:
	fl_textedit_search(ob, 0);
	return;
	break;
  case FL_TEXTKEY_FAGAIN:
	fl_textedit_search(ob, 1);
	return;

  case FL_TEXTKEY_WORDLEFT:
	if (spec->flags & FL_TEXTEDIT_READONLY)
		return;
	fl_textedit_wordleft(ob);
	return;

  case FL_TEXTKEY_WORDRIGHT:
	if (spec->flags & FL_TEXTEDIT_READONLY)
		return;
	fl_textedit_wordright(ob);
	return;

  case FL_TEXTKEY_DELWORDL:
	if (spec->flags & FL_TEXTEDIT_READONLY)
		return;
	fl_textedit_delwordleft(ob);
	return;

  case FL_TEXTKEY_DELWORDR:
	if (spec->flags & FL_TEXTEDIT_READONLY)
		return;
	fl_textedit_delwordright(ob);
	return;

  case FL_TEXTKEY_FORMAT:
	if (spec->flags & FL_TEXTEDIT_READONLY)
		return;
	if (fl_textedit_selected(ob))	{
		tb_fill_region(tb, spec->sselr, spec->sselc, spec->eselr, spec->eselc);
		if (fl_textedit_movecursor(ob, spec->r, spec->c) == -1) {
		 fl_textedit_set_topline(ob, spec->r, 1);
		 fl_textedit_movecursor(ob, spec->sselr, 0);
		 spec->cpos = 0;
		 tb_set_current_line(tb, spec->topline);		}
		else
		 tb_set_current_line(tb, spec->r);

		fl_textedit_remove_selection(ob);
					}
	else	{
	 int pstart, pend;

		tb_get_paragraph(tb, &pstart, &pend);
		tb_fill_region(tb, pstart, 0, pend, -1);

		if (fl_textedit_movecursor(ob, spec->r, spec->c) == -1) {
		 fl_textedit_set_topline(ob, pstart, 1);
		 fl_textedit_movecursor(ob, pstart, 0);
		 spec->cpos = 0;
		 tb_set_current_line(tb, spec->topline);		}
		else
		 tb_set_current_line(tb, spec->r);
		}

	tb->bufchanged = TRUE;
	fl_textedit_set_hscrollbar_max(ob);
	fl_textedit_refresh_screen(ob, 1);
	return;
#ifdef	DEBUG
  case FL_TEXTKEY_DEBUG:
	fprintf(stderr, "Geometry: %dx%d +%d+%d\n", ob->x, ob->y, ob->w, ob->h);
	fprintf(stderr, "Topline: %d Left column: %d\n", spec->topline, spec->leftcol);
	fprintf(stderr, "Screen size: %dx%d\n", spec->wsize, spec->csize);
	fprintf(stderr, "Character size: %dx%d\n", spec->charheight, spec->charwidth);
	fprintf(stderr, "Cursor - R: %d C: %d CPOS: %d\n", spec->r, spec->c, spec->cpos);
	fprintf(stderr, "Selection - %dx%d-%dx%d\n", spec->sselr, spec->sselc,
						     spec->eselr, spec->eselc);
	fprintf(stderr, "Text - Style: %d Size: %d\n", spec->text_style, spec->text_size);
	fprintf(stderr, "Colors - Background: %lu Foreground: %lu Cursor: %d\n",
		ob->col1, ob->col2, spec->ccol);
	fprintf(stderr, "Options -");
	if (spec->flags & FL_TEXTEDIT_READONLY)
	 fprintf(stderr, " READONLY");
	if (spec->flags & FL_TEXTEDIT_NOTDRAWN)
	 fprintf(stderr, " NOTDRAWN");
	if (spec->flags & FL_TEXTEDIT_VSBAR)
	 fprintf(stderr, " VERTICAL_SCROLLBAR");
	if (spec->flags & FL_TEXTEDIT_VSBAR_AUTO)
	 fprintf(stderr, " VERTICAL_SCROLLBAR_AUTOHIDE");
	if (spec->flags & FL_TEXTEDIT_HSBAR)
	 fprintf(stderr, " HORIZONTAL_SCROLLBAR");
	if (spec->flags & FL_TEXTEDIT_HSBAR_AUTO)
	 fprintf(stderr, " HORIZONTAL_SCROLLBAR_AUTOHIDE");
	fprintf(stderr, "\n");
	if (spec->exp)
		fprintf(stderr, "Search expression - %s\n", spec->exp);
	fprintf(stderr, "Textbuffer - Lines: %d Current: %d Maxlength: %d\n", tb->n, tb->i, tb->maxchars);
	fprintf(stderr, "Textbuffer - Changed: %d Tab: %d Wrap: %d\n", tb->bufchanged, tb->tablen, tb->linewrap);
	fprintf(stderr, "Textbuffer - Foregound color: %d Background color: %d\n", tb->fgcolor_def, tb->bgcolor_def);
	fprintf(stderr, "Textbuffer - Default attribute: %02X\n", tb->attr_def);
	if (tb->firstline)
	 fprintf(stderr, "Textbuffer - First line:\n[%s]\n[%s]\n", tb->firstline->buf, tb->firstline->attr);
	if (tb->lastline)
	 fprintf(stderr, "Textbuffer - Last line:\n[%s]\n[%s]\n", tb->lastline->buf, tb->lastline->attr);
	if (tb->currentline)
	 fprintf(stderr, "Textbuffer - Current line:\n[%s]\n[%s]\n", tb->currentline->buf, tb->currentline->attr);
	fprintf(stderr, "Vertical scrollbar - Geometry: %dx%d +%d+%d\n", spec->sb->x, spec->sb->y, spec->sb->w, spec->sb->h);
	fprintf(stderr, "Vertical scrollbar - Value: %.3f\n", fl_get_scrollbar_value(spec->sb));
	fprintf(stderr, "Vertical scrollbar - On: %d Visible: %d Default width: %d\n", spec->v_on, spec->sb->visible, spec->vw);
	fprintf(stderr, "Horizontal scrollbar - Geometry: %dx%d +%d+%d\n", spec->hsb->x, spec->hsb->y, spec->hsb->w, spec->hsb->h);
	fprintf(stderr, "Horizontal scrollbar - Value: %.3f\n", fl_get_scrollbar_value(spec->hsb));
	fprintf(stderr, "Horizontal scrollbar - On: %d Visible: %d Default width: %d\n", spec->h_on, spec->hsb->visible, spec->hh);
	break;
#endif
	break;
    }
    
    return;
} /* fl_textedit_handle_keyboard() */

int
fl_textedit_sel_call(FL_OBJECT *ob, long type, const void *data, long size)
/*
 * callback that receives clipboard contents
 */
{
Window wn;
char *buf;

 if ((buf = (char *)malloc(size + 1)) == NULL)
	return 0;

 strncpy(buf, (char *)data, size);
 buf[size] = '\0';

 wn = fl_winget();
 fl_winset(ob->form->window);
 fl_textedit_inssel(ob, buf);
 fl_winset(wn);

 free(buf);

 return 0;
}

void
fl_textedit_paste(FL_OBJECT *ob)
/*
 * Paste selection into the textarea
 */
{
 fl_request_clipboard(ob, 0, fl_textedit_sel_call);
 return;
}

static void
fl_textedit_inssel(FL_OBJECT *ob, char *buf)
/*
 * Desc: insert buf into text area
 */
{
SPEC *spec = (SPEC *)ob->spec;
TextBuf *tb = &spec->tb;
int lines, newr = spec->r, newc = spec->c;
u_long offt, buflen;


 if ((spec->paster == -1) ||
	(spec->pastec == -1))
	return;

 fl_get_textedit_cursorpos(ob, NULL, NULL, &offt);
 lines = tb_get_nlines(tb);
 buflen = tb_get_textlen(tb);
 tb_insert_block(tb, spec->paster, spec->pastec, buf);
 buflen = tb_get_textlen(tb) -  buflen;
 if ((spec->flags & FL_TEXTEDIT_PASTE_CUR) &&
	(buflen > 0)) {
   fl_calc_cursorpos(ob, offt + buflen, &newc, &newr);
   fl_textedit_movecursor_visible(ob, newr, newc);
		      }

 if (lines != tb_get_nlines(tb))
	fl_textedit_set_vscrollbar_max(ob);

 fl_textedit_remove_selection(ob);
 fl_textedit_refresh_screen(ob, 0);
 fl_textedit_set_hscrollbar_max(ob);
 if (spec->cur_callback) (spec->cur_callback)(ob, tb_return_line(tb), 0, spec->r, spec->c);

 spec->paster = spec->pastec = -1;
}

char *
fl_get_textedit_seltext(FL_OBJECT *ob)
/*
 * Desc: returns pointer to selected text
 *       The text should be freed after using
 */
{
TextBuf *tb;
SPEC    *spec;
char *buf;

 spec = (SPEC *)ob->spec;
 tb = &spec->tb;

 /* check if we have any text selected */
 if ((spec->sselr < 0) || (spec->eselr < 0) ||
	((spec->sselr == spec->eselr) && (spec->sselc == spec->eselc)))
	return NULL;

 tb_get_block(tb, spec->sselr, spec->sselc, spec->eselr, spec->eselc, &buf);

 return buf;
}

int
fl_textedit_copy_call(FL_OBJECT *ob, long type)
{
Window wn;

 wn = fl_winget();
 fl_winset(ob->form->window);
 fl_textedit_remove_selection(ob);
 fl_winset(wn);
 return 0;
}

static void
fl_textedit_copy(FL_OBJECT *ob)
/*
 * Copy selected text from textarea to X-servers buffer
 */
{
char *buf;

 if (fl_textedit_selected(ob))	{
  if ((buf = fl_get_textedit_seltext(ob)) != NULL)	{
	fl_stuff_clipboard(ob, 0, buf, strlen(buf), fl_textedit_copy_call);
	free(buf);					}
				}
}

static int
fl_textedit_set_cursor(FL_OBJECT *ob, int mx, int my, int draw)
/*
 * Desc: set the textcursor to the textcoordinates 
 *	  corresponding with (mx,my)
 * Returns: -1 if cursor is above the text area
 *          -2 at the left
 *           0 in the text area
 *           2 at the right
 *           1 below the text area
 */
{
    TextBuf *tb;
    SPEC    *spec;
    char    *line, ch[2];
    int	    i, lh, x, oldline, r, c;

    spec = (SPEC *)ob->spec;
    tb = &spec->tb;

    /* correct the mx and my offset to be relative to the 
       start of the text */
    mx = mx - ob->x - TEXT_X_OFFSET;
    my = my - ob->y - TEXT_Y_OFFSET;

    if (my < 0)	   {
	if (fl_textedit_movecursor(ob, 0, spec->c) == -1)	   {
		tb_set_current_line(tb, 0);
		fl_textedit_movecursor(ob, 0, tb_get_linelen(tb)); }
	return -1; }

    if (mx < 0)    {
	fl_textedit_movecursor(ob, spec->r, 0);
	spec->cpos = 0;
	return -2; }

    if (mx > (ob->w - 2*abs(ob->bw) - 2* TEXT_X_OFFSET))
	return 2;

    /* save the old line with the cursor on it */
    oldline = spec->r;

    /* set the current line to the right one, but check if it's not
       out of range */
    i = tb_get_nlines(tb);
    if (i == 0)
	i = 1;
    if (spec->topline + (my/spec->charheight) >= i)	{
	r = tb_get_nlines(tb) - 1;
	if (r < 0)
		r = 0;
	if (fl_textedit_movecursor(ob, r, spec->c) == -1)		{
		tb_set_current_line(tb, r);
		fl_textedit_movecursor(ob, r, tb_get_linelen(tb));	}
	return 1;					}

    if ((my/spec->charheight) >= spec->wsize)	{
	r = spec->topline + spec->wsize - 1;
	if (fl_textedit_movecursor(ob, r, spec->c) == -1)		{
		tb_set_current_line(tb, r);
		fl_textedit_movecursor(ob, r, tb_get_linelen(tb));	}
	return 1;		}

    r = spec->topline + (my/spec->charheight);

    /* now calculate which character the x position points to */
    /* first get the current line */
    tb_set_current_line(tb, r);
    tb_get_line(tb, &line);
    if (!line || !*line)   {
	c = 0;
	goto reallysetcur; }

    lh = strlen(line);
    x = 0;
    i = spec->leftcol;
    if (i > lh)
	i = lh;
    ch[1] = '\0';
    while ((x < mx) && (i < lh) && line[i]) {
	/* get the width of the i-th character */
	ch[0] = line[i];
	x = x + fl_get_string_width(spec->text_style, spec->text_size, ch, 1);
	i++;
    } /* x>=mx || line[i]==NULL */
    if (x>=mx && i>0) {
	i--;
    }

    c = i;

reallysetcur:
    if ((r == spec->r) && (c == spec->c))
	return 0;

    fl_textedit_movecursor(ob, r, c);
    spec->cpos = spec->c;

    return 0;
} /* fl_textedit_set_cursor() */

static int
fl_textedit_selected(FL_OBJECT *ob)
{
    SPEC *spec = (SPEC *)ob->spec;

    /* check if there is any text selected */
    if ((spec->sselr < 0) ||
	(spec->eselr < 0))
	return 0;

    /* check if length of the selected part is bigger then zero */
    if ((spec->sselr == spec->eselr) &&
	(spec->sselc == spec->eselc))
	return 0;

    return 1;
}

static void
fl_textedit_remove_selection(FL_OBJECT *ob)
/*
 * Desc: remove text selection
 */
{
    SPEC *spec = (SPEC *)ob->spec;
    int i, ssel, esel;

    if ((spec->sselr < 0) ||
        (spec->eselr < 0))
        return;

    ssel = spec->sselr;
    esel = spec->eselr;

    spec->sselr = -1;
    spec->eselr = -1;
    spec->flags &= ~FL_TEXTEDIT_SANCHOR_END;

    for (i = ssel; i <= esel; i++)
	fl_textedit_draw_line(ob, i);
}

void
fl_textedit_extend_selection(FL_OBJECT *ob, int oldr, int oldc)
/*
 * Desc: extend selection up to cursor location
 */
{
    SPEC *spec = (SPEC *)ob->spec;
    int sselr, eselr, sselc, eselc, i;

 /* no selection */
 if ((spec->sselr < 0) ||
	(spec->eselr < 0))
	return;

 if (spec->flags & FL_TEXTEDIT_SANCHOR_END) {
   if ((spec->sselr == spec->r) &&
	(spec->sselc == spec->c))
	return;
					    }
 else	{
   if ((spec->eselr == spec->r) &&
	(spec->eselc == spec->c))
	return;
	}

 eselr = spec->eselr;
 sselr = spec->sselr;
 eselc = spec->eselc;
 sselc = spec->sselc;

 /* the selection now starts/ends at cursor position */
 if (spec->sselr > spec->r)	{
	if (!(spec->flags & FL_TEXTEDIT_SANCHOR_END)) 	{
		spec->eselr = spec->sselr;
		spec->eselc = spec->sselc;		}
	spec->sselr = spec->r;
	spec->sselc = spec->c;
	spec->flags |= FL_TEXTEDIT_SANCHOR_END;
				}
 else
 if (spec->sselr == spec->r)	{
	if (spec->c < spec->sselc) {
		if (!(spec->flags & FL_TEXTEDIT_SANCHOR_END)) 	{
			spec->eselr = spec->sselr;
			spec->eselc = spec->sselc;		}
		spec->sselc = spec->c;
		spec->flags |= FL_TEXTEDIT_SANCHOR_END;
				   }
	else
	if (spec->c > spec->sselc) {
		if (spec->flags & FL_TEXTEDIT_SANCHOR_END) {
			if ((spec->c >= spec->eselc) &&
				(spec->r >= spec->eselr))	{
	 		 spec->flags &= ~FL_TEXTEDIT_SANCHOR_END;
	 		 spec->sselr = spec->eselr;
	 		 spec->sselc = spec->eselc;
	 		 spec->eselr = spec->r;
	 		 spec->eselc = spec->c;			}
			else
	 		 spec->sselc = spec->c;
							   }
		else	{
			spec->eselr = spec->r;
			spec->eselc = spec->c;
			}
				   }	
	else	{
		if (oldr != spec->r)	{
			fl_textedit_draw_line(ob, oldr);
			fl_textedit_draw_line(ob, spec->r);
					}
		return;
		}
				}
 else				{
	if (spec->flags & FL_TEXTEDIT_SANCHOR_END) {
	 if (spec->r >= spec->eselr) {
	  spec->flags &= ~FL_TEXTEDIT_SANCHOR_END;
	  spec->sselr = spec->eselr;
	  spec->sselc = spec->eselc;
	  spec->eselr = spec->r;
	  spec->eselc = spec->c;     }
	 else	{
	  spec->sselr = spec->r;
	  spec->sselc = spec->c;
		}
						   }
	else	{
	 spec->eselr = spec->r;
	 spec->eselc = spec->c;
		}	
				}

 if (eselr < spec->eselr)	{
   for (i = eselr; i <= spec->eselr; i++)
	fl_textedit_draw_line(ob, i);
				}
 else
 if (eselr > spec->eselr)	{
   for (i = spec->eselr; i <= eselr; i++)
	fl_textedit_draw_line(ob, i);
				}
 else
 if (sselr < spec->sselr)	{
   for (i = sselr; i <= spec->sselr; i++)
	fl_textedit_draw_line(ob, i);
				}
 else
 if (sselr > spec->sselr)	{
   for (i = spec->sselr; i <= sselr; i++)
	fl_textedit_draw_line(ob, i);
				}
 else {
  if ((eselc != spec->eselc) || (sselc != spec->sselc) || (oldr != spec->r))
        fl_textedit_draw_line(ob, spec->r);
      }

 if (oldr != spec->r)
	fl_textedit_draw_line(ob, oldr);

 return;
}

static int
fl_textedit_handle_dblclick(FL_OBJECT *ob, int key)
/*
 * Desc: handle double-click events
 */
{
SPEC *spec = (SPEC *)ob->spec;
TextBuf *tb = &spec->tb;
char *p, *line;

 /* mark the current word */
 tb_get_line(tb, &line);
 if ((line == NULL) || (*line == '\0') ||
	(spec->c > strlen(line)))
	return -1;

 fl_textedit_remove_selection(ob);
 spec->sselr = spec->r;
 spec->eselr = spec->r;

 spec->sselc = spec->c;
 spec->eselc = spec->c;

 while ((spec->sselc > 0) && (line[spec->sselc - 1] != ' '))
	spec->sselc--;

 while ((line[spec->eselc] != '\0') && (line[spec->eselc] != ' '))
	spec->eselc++;

 if (spec->dbl_callback &&
	(spec->eselc > spec->sselc)) {
  p = strdup(line + spec->sselc);
  p[spec->eselc - spec->sselc] = '\0';
  if ((spec->dbl_callback)(ob, p, 0, spec->r, spec->c)) {
	spec->sselr = spec->eselr = -1;
	free(p);
	fl_winset(ob->form->window);
	return 1;				        }
  fl_winset(ob->form->window);
  free(p);
				    }

 if (key == 0)	{
  if (tb->currentline && tb->currentline->dbl_callback)		{
	(tb->currentline->dbl_callback)(ob, tb->currentline, 0,
		tb->currentline->callback_data);
	fl_winset(ob->form->window);				}
		}
 else	{
  if (tb->currentline && tb->currentline->clk_callback)		{
	(tb->currentline->clk_callback)(ob, tb->currentline, key,
		tb->currentline->callback_data);
	fl_winset(ob->form->window);				}
	}

 fl_textedit_draw_selection(ob);
 fl_textedit_copy(ob);

 return 0;
}

/***********************************************************************
 *
 * Main event handling 
 *
 ***********************************************************************/

static int
handle_textedit(FL_OBJECT *ob, int event, FL_Coord mx, FL_Coord my, 
		int key, void *xev)
/*
 * Desc: the event handler for the textedit object 
 */
{
    SPEC *spec = (SPEC *)ob->spec;
    TextBuf *tb = &spec->tb;
    int oldc, oldr;
    char *line;
    XEvent *txev;
     
    txev = (XEvent *)xev;

#ifdef FLTEXT_DEBUG
    if (xev && ((XEvent *)xev)->type == KeyPress) {
	XKeyEvent *xkev = (XKeyEvent *)xev;
	printf("keycode = %d, state = %d\n", xkev->keycode, xkev->state);
	printf("Control = %d, Shift = %d, Alt = %d\n",
	       xkev->state & ControlMask, xkev->state & ShiftMask, 
	       xkev->state & Mod1Mask);
    }
#endif

    switch(event) {
    case FL_DRAW:
	spec->sb->boxtype = ob->boxtype;
	spec->hsb->boxtype = ob->boxtype;
	spec->sb->bw = spec->hsb->bw = ob->bw;
	spec->wsize = (ob->h - 2*abs(ob->bw)) / spec->charheight;
	spec->csize = (ob->w - 2*abs(ob->bw) - 2*TEXT_X_OFFSET) / spec->charwidth;
	fl_textedit_vscrollbar_dim(ob);
	fl_textedit_hscrollbar_dim(ob);

	if (spec->flags & FL_TEXTEDIT_NOTDRAWN)	{
		spec->flags &= ~FL_TEXTEDIT_NOTDRAWN;
		fl_textedit_switch_vscrollbar(ob);
		fl_textedit_switch_hscrollbar(ob);
		  				}
	else	{
		if (spec->oldh != ob->h)		   {
			fl_textedit_reset_vscrollbar(ob);
			fl_textedit_switch_vscrollbar(ob); }
		if (spec->oldw != ob->w)		   {
			fl_textedit_reset_hscrollbar(ob);
			fl_textedit_switch_hscrollbar(ob); }
		}

	fl_textedit_draw_screen(ob);
	spec->oldw = ob->w;
	spec->oldh = ob->h;
    case FL_DRAWLABEL:
	fl_draw_object_label(ob);
	break;
    case FL_PUSH:
	switch (key)  {
	 case 1:
	    /* set the cursor at the correct position */
	    fl_textedit_set_cursor(ob, mx, my, 1);

	    /* start new selection */
	    fl_textedit_remove_selection(ob);
	    spec->sselr = spec->eselr = spec->r;
	    spec->sselc = spec->eselc = spec->c;
	 break;

	 case 2:
	    if (spec->flags & FL_TEXTEDIT_READONLY)	{
		fl_textedit_set_cursor(ob, mx, my, 1);
		fl_textedit_handle_dblclick(ob, key);
		return 0;				}

	    /* paste selection into the text area */
	    if (spec->flags & FL_TEXTEDIT_PASTE_CUR) {
		spec->paster = spec->r;
		spec->pastec = spec->c;
		fl_textedit_paste(ob);		     }
	    else	{
	     oldr = spec->r;
	     oldc = spec->c;
	     if (fl_textedit_set_cursor(ob, mx, my, 1) == 0)	{
		spec->paster = spec->r;
		spec->pastec = spec->c;
		fl_textedit_paste(ob);				}
	     fl_textedit_movecursor(ob, oldr, oldc);
			}
	 break;

	 case 3:
		fl_textedit_set_cursor(ob, mx, my, 1);
	 break;
			}

	 if (tb->currentline && tb->currentline->clk_callback)
		if ((tb->currentline->clk_callback)(ob, tb->currentline, key,
			tb->currentline->callback_data) == 0)
			return 0;

	 if (spec->clk_callback) {
		tb_get_line(tb, &line);
		if ((spec->clk_callback)(ob, line, key, spec->r, spec->c) == 0)
			return 0;
				 }

	 if (key == 3)
		return 1;
	break;
    case FL_MOUSE:
	if (key != 1)
		break;
	/* store the old cursor position */
	oldc = spec->c;
	oldr = spec->r;

	/* set the cursor at the correct position */
	switch (fl_textedit_set_cursor(ob, mx, my, 0))	{
	 case -1:
	    fl_textedit_lineup(ob);
	 break;

	 case 1:
	    fl_textedit_linedown(ob);
	 break;

	 case -2:
	    if (spec->h_on)
		fl_textedit_cleft(ob);
	 break;

	 case 2:
	    if (spec->h_on)
		fl_textedit_cright(ob);
	 break;
							}

	/* select everything up to the cursor position */
	fl_textedit_extend_selection(ob, oldr, oldc);
	break;
    case FL_RELEASE:
	/* copy selected text */
	if ((key == 1) || (key == 0))
		fl_textedit_copy(ob);
	break;
    case FL_KEYBOARD:
	 if (spec->key_callback) {
	   tb_get_line(tb, &line);
	   if ((spec->key_callback)(ob, line, key, spec->r, spec->c)) 	{
		fl_winset(ob->form->window);
		break;							}
	   fl_winset(ob->form->window);
				 }
	fl_textedit_handle_keyboard(ob, key, xev);
	break;
    case FL_SHORTCUT:
	return 1;
	break;
    case FL_UNFOCUS:
	/* deselect everything
	fl_textedit_remove_selection(ob); */
	/* hide cursor */
	spec->flags |= FL_TEXTEDIT_NOCUR;
	if ((ob->active == 1) &&
		ob->form->visible &&
		(fl_winget() == ob->form->window))
		fl_textedit_movecursor(ob, spec->r, spec->c);
	break;
    case FL_FOCUS:
	/* show cursor */
	if (!(spec->flags & FL_TEXTEDIT_READONLY)) {
	 spec->flags &= ~FL_TEXTEDIT_NOCUR;
	 if ((ob->active == 1) &&
		ob->form->visible &&
		(fl_winget() == ob->form->window))
		fl_textedit_movecursor(ob, spec->r, spec->c);
						   }
	break;
    case FL_FREEMEM:
	/* free the memory used by the spec member of ob */
	{
	    tb_clear(tb);
	    if (spec->exp)
		free(spec->exp);
	    free(spec);
	    ob->spec = NULL;
	}

	break;
    case FL_DBLCLICK:
	fl_textedit_handle_dblclick(ob, 0);
	break;
    case FL_TRPLCLICK:
	/* a triple click event was received, the timeout must be 
	   set in ob->click_timeout */
	/* mark the current line */
	fl_textedit_remove_selection(ob);
	spec->sselr = spec->r;
	spec->eselr = spec->r;

	spec->sselc = 0;
	spec->eselc = -1;
	fl_textedit_draw_selection(ob);
	fl_textedit_copy(ob);
	break;
    }
    
    return 0;
} /* handle_textedit() */

void
fl_textedit_sb_cb(FL_OBJECT *ob, long data)
/*
 * Desc: the callback for the scrollbar object to call to redraw
 *	  the screen
 */
{
    FL_OBJECT *obj = (FL_OBJECT *)data;
    SPEC *spec = (SPEC *)obj->spec;
    TextBuf *tb = &spec->tb;
    Window wn = fl_winget();
    int spos = fl_textedit_getvscrollbar(obj);

    if (spos == spec->topline)
	return;

    fl_winset(ob->form->window);
    fl_textedit_set_topline(obj, spos, 0);
    fl_winset(wn);
    if (spec->cur_callback) (spec->cur_callback)(obj, tb_return_line(tb), 0, spec->r, spec->c);
    return;
} /* fl_textedit_sb_cb() */

void
fl_textedit_hsb_cb(FL_OBJECT *ob, long data)
/*
 * Desc: the callback for the horizontal scrollbar object to call to redraw
 *	  the screen
 */
{
    FL_OBJECT *obj = (FL_OBJECT *)data;
    SPEC *spec = (SPEC *)obj->spec;
    Window wn = fl_winget();
    int spos = fl_textedit_gethscrollbar(obj);

    if (spec->leftcol == spos)
	return;

    spec->leftcol = spos;
    fl_winset(ob->form->window);
    fl_textedit_draw_screen(obj);
    fl_winset(wn);

    return;
}


FL_OBJECT *
fl_create_textedit(int type, FL_Coord x, FL_Coord y, 
		   FL_Coord w, FL_Coord h, const char *label)
/*
 * Desc: create the textedit object
 */
{
    FL_OBJECT *ob;
    SPEC *spec;

    ob = fl_make_object(FL_TEXTEDIT, type, x, y, w, h,
			label, handle_textedit);

    ob->col1 = FL_BLACK;
    ob->col2 = FL_MCOL;
    ob->boxtype = FL_DOWN_BOX;
    ob->wantkey = FL_KEY_ALL;
    ob->click_timeout = 300;
    ob->input = TRUE;

    /* initialize the object specific user data */
    ob->spec = (SPEC *) fl_calloc(1, sizeof(SPEC));
    spec = (SPEC *)ob->spec;
    if (!ob->spec) {
	fl_edit_error("Could not malloc space for SPEC structure in object:%s");//, 
//		 label);
	exit(1);
    }
    tb_init(&spec->tb);
    spec->tb.fgcolor_def = ob->col1;
    spec->tb.bgcolor_def = ob->col2;
    spec->r = 0;
    spec->c = 0;
    spec->cpos = 0;
    spec->topline = 0;
    spec->leftcol = 0;
    spec->name[0] = '\0';
    spec->text_style = FL_FIXED_STYLE;
    spec->text_size = FL_NORMAL_SIZE;
    spec->ccol = FL_YELLOW;
    spec->sselr = -1;
    spec->sselc = 0;
    spec->eselr = -1;
    spec->eselc = 0;
    spec->flags = FL_TEXTEDIT_NOTDRAWN | FL_TEXTEDIT_VSBAR | FL_TEXTEDIT_VSBAR_AUTO | FL_TEXTEDIT_HSBAR | FL_TEXTEDIT_HSBAR_AUTO;
    spec->exp = NULL;
    spec->oldw = ob->w;
    spec->oldh = ob->h;
    spec->paster = spec->pastec = -1;
    spec->key_callback = NULL;
    spec->clk_callback = NULL;
    spec->dbl_callback = NULL;
    spec->cur_callback = NULL;
    fl_get_string_dimension(spec->text_style, spec->text_size, "W", 1,
		&spec->charwidth, &spec->charheight);
    spec->wsize = (ob->h - 2*abs(ob->bw)) / spec->charheight;
    spec->csize = (ob->w - 2*abs(ob->bw) - 2*TEXT_X_OFFSET) / spec->charwidth;

    /* scrollbar section */
    spec->v_on = spec->h_on = 0;
    spec->vw = spec->hh = fl_get_default_scrollbarsize(ob);
    spec->sb = fl_create_scrollbar(FL_VERT_THIN_SCROLLBAR, x + w - spec->vw,
				y, spec->vw, h, "");
    fl_set_object_callback(spec->sb, fl_textedit_sb_cb, (long) ob);
    spec->sb->visible = 0;
    spec->sb->resize = FL_RESIZE_NONE;
    fl_set_scrollbar_value(spec->sb, 0.0);
    fl_textedit_vscrollbar_dim(ob);

    spec->hsb = fl_create_scrollbar(FL_HOR_THIN_SCROLLBAR, x, y + h - spec->hh,
				w, spec->hh, "");
    fl_set_object_callback(spec->hsb, fl_textedit_hsb_cb, (long) ob);
    spec->hsb->visible = 0;
    spec->hsb->resize = FL_RESIZE_NONE;
    fl_set_scrollbar_value(spec->hsb, 0.0);
    fl_textedit_hscrollbar_dim(ob);

    return(ob);
} /* fl_create_textedit() */

/***********************************************************************
 *
 * Exported API
 *
 ***********************************************************************/

FL_OBJECT *
fl_add_textedit(int type, FL_Coord x, FL_Coord y, 
		   FL_Coord w, FL_Coord h, const char *label)
{
    FL_OBJECT *ob = fl_create_textedit(type, x, y, w, h, label);
    SPEC *spec = (SPEC *)ob->spec;

    fl_add_child(ob, spec->sb);
    fl_add_child(ob, spec->hsb);
    fl_add_object(fl_current_form, ob);

    return(ob);
} /* fl_add_textedit() */

void
fl_set_textedit(FL_OBJECT *ob, char *buf, long len)
/*
 * Desc: insert text from <buf> with length <len> into the textedit object <ob>
 * Pre:  The textedit object has been created
 *
 */
{
    SPEC *spec = (SPEC *)ob->spec;
    TextBuf *tb = &spec->tb;
    int n;

    tb_set_text(tb, buf, len);
    strcpy(spec->name, "noname");
    spec->r = 0;
    spec->c = 0;
    spec->cpos = 0;
    spec->topline = 0;
    spec->leftcol = 0;
    spec->sselr = spec->eselr = -1;
    n = tb_get_nlines(tb);

    fl_textedit_set_vscrollbar_wsize(ob);
    fl_textedit_set_hscrollbar_wsize(ob);
    if (spec->cur_callback) (spec->cur_callback)(ob, tb_return_line(tb), 0, spec->r, spec->c);

    fl_redraw_object(ob);

    return;
} /* fl_set_textedit() */

void
fl_load_textedit(FL_OBJECT *ob, char *fname)
/*
 * Desc: load a file into the textedit object <ob>
 * Pre:  The textedit object has been created
 *
 */
{
    SPEC *spec = (SPEC *)ob->spec;
    TextBuf *tb = &spec->tb;

    tb_load_file(tb, fname);
    strcpy(spec->name, fname);
    spec->r = 0;
    spec->c = 0;
    spec->cpos = 0;
    spec->topline = 0;
    spec->leftcol = 0;
    spec->sselr = spec->eselr = -1;
 
    fl_textedit_set_vscrollbar_wsize(ob);
    fl_textedit_set_hscrollbar_wsize(ob);
    if (spec->cur_callback) (spec->cur_callback)(ob, tb_return_line(tb), 0, spec->r, spec->c);

    fl_redraw_object(ob);

    return;
} /* fl_load_textedit() */

void
fl_save_textedit(FL_OBJECT *ob, char *fname)
/*
 * Desc: Save the textedit file under the name <fname>
 */
{
    SPEC *spec = (SPEC *)ob->spec;
    TextBuf *tb = &spec->tb;

    tb_save_file(tb, fname);
    strcpy(spec->name, fname);

    return;  
} /* fl_save_textedit() */

char *
fl_get_textedit(FL_OBJECT *ob, u_long *len)
/*
 * Desc: return pointer to buffer with text of object <ob>
 * length of the buffer will be returned in <len>
 */
{
    SPEC *spec = (SPEC *)ob->spec;
    TextBuf *tb = &spec->tb;

    return tb_get_text(tb, len);

} /* fl_get_textedit() */

void 
fl_set_textedit_fontsize(FL_OBJECT *ob, int size)
/*
 * Desc: set the font size of the object 
 */
{
    SPEC *spec = (SPEC *)ob->spec;

    if (spec->text_size != size)		{
	spec->text_size = size;
	fl_get_string_dimension(spec->text_style, spec->text_size, "W", 1,
		&spec->charwidth, &spec->charheight);
	spec->wsize = (ob->h - 2*abs(ob->bw)) / spec->charheight;
	spec->csize = (ob->w - 2*abs(ob->bw) - 2*TEXT_X_OFFSET) / spec->charwidth;
	fl_redraw_object(ob);
	fl_textedit_set_vscrollbar_wsize(ob);
	fl_textedit_set_hscrollbar_wsize(ob);	}

    spec->text_size = size;
    
    return;
} /* fl_set_textedit_fontsize() */

int
fl_get_textedit_fontsize(FL_OBJECT *ob)
/*
 * Desc: get the font size of the object
 */
{
    SPEC *spec = (SPEC *)ob->spec;
    
    return(spec->text_size);
} /* fl_get_textedit_fontsize() */

void
fl_set_textedit_fontstyle(FL_OBJECT *ob, int style)
/*
 * Desc: set the font style of the object 
 */
{
    SPEC *spec = (SPEC *) ob->spec;
    
    if (style != spec->text_style) 		{
	spec->text_style = style;
	fl_get_string_dimension(spec->text_style, spec->text_size, "W", 1,
		&spec->charwidth, &spec->charheight);
	spec->wsize = (ob->h - 2*abs(ob->bw)) / spec->charheight;
	spec->csize = (ob->w - 2*abs(ob->bw) - 2*TEXT_X_OFFSET) / spec->charwidth;
	fl_redraw_object(ob);
	fl_textedit_set_vscrollbar_wsize(ob);
	fl_textedit_set_hscrollbar_wsize(ob);   }

    spec->text_style = style;

    return;
} /* fl_set_textedit_fontstyle() */

int
fl_get_textedit_fontstyle(FL_OBJECT *ob)
/*
 * Desc: get the font style of the object
 */
{
    SPEC *spec = (SPEC *)ob->spec;

    return(spec->text_style);
} /* fl_get_textedit_fontstyle() */

void
fl_set_textedit_color(FL_OBJECT *ob, int bgcol, int fgcol, int ccol, int all)
/*
 * Desc: set the color of the object 
 * <bgcol>, <fgcol> and <ccol> are background , foreground and cursort colors
 *	respectively
 * if <all> is not 0 then color of every line is set to given colors
 */
{
int redraw;
SPEC *spec = (SPEC *)ob->spec;
TextBuf *tb = &spec->tb;
TextLine *tl = tb->firstline;

    redraw = all;
    if (ccol >= 0)	{
     if (spec->ccol != ccol)
	redraw = 1;
     spec->ccol = ccol;	}

    if (fgcol >= 0)
     ob->col1 = fgcol;
    if (bgcol >= 0)
     ob->col2 = bgcol;

    if (all && tl)	{
     while (tl)	{
      if ((fgcol >= 0) && (tl->fgcolor == tb->fgcolor_def))
	tl->fgcolor = fgcol;
      if ((bgcol >= 0) && (tl->bgcolor == tb->bgcolor_def))
	tl->bgcolor = bgcol;
      tl = tl->next;
		}
			}

    tb->fgcolor_def = ob->col1;
    tb->bgcolor_def = ob->col2;

    if (redraw)
	fl_redraw_object(ob);

    return;
}

void
fl_get_textedit_color(FL_OBJECT *ob, int *bgcol, int *fgcol, int *ccol)
/*
 * Desc: get the color of the object 
 */
{
    SPEC *spec = (SPEC *)ob->spec;
    if (ccol)
	*ccol = spec->ccol;
    if (bgcol)
	*bgcol = ob->col2;
    if (fgcol)
	*fgcol = ob->col1;
}

char *
fl_get_textedit_bufname(FL_OBJECT *ob)
/* 
 * Desc: return the name of the buffer 
 */
{
    SPEC *spec = (SPEC *)ob->spec;
    
    return(spec->name);
} /* fl_get_textedit_bufname() */

void
fl_set_textedit_bufname(FL_OBJECT *ob, char *fname)
/*
 * Desc: set the name of the buffer
 */
{
    SPEC *spec = (SPEC *)ob->spec;

    strcpy(spec->name, fname);
    
    return;
} /* fl_set_textedit_bufname() */

int
fl_get_textedit_bufchanged(FL_OBJECT *ob)
/*
 * Desc: return the change status of the textbuffer 
 *	  TRUE if it needs saving, FALSE if not
 */
{
    SPEC *spec = (SPEC *)ob->spec;
    TextBuf *tb = &spec->tb;

    return(tb_bufchanged(tb));
} /* fl_textedit_bufchanged() */

void
fl_set_textedit_bufchanged(FL_OBJECT *ob, int status)
/*
 * Desc: set the bufferchanged status 
 */
{
    SPEC *spec = (SPEC *)ob->spec;
    TextBuf *tb = &spec->tb;
    
    tb_set_bufchanged(tb, status);
    return;
} /* fl_set_textedit_bufchanged() */

FL_OBJECT *
fl_get_textedit_vscrollbar(FL_OBJECT *ob)
/*
 * return pointer to vertical scrollbar associated with textedit 
 */
{
    SPEC *spec = (SPEC *)ob->spec;

    return spec->v_on ? spec->sb : NULL;
}

FL_OBJECT *
fl_get_textedit_hscrollbar(FL_OBJECT *ob)
/*
 * return pointer to horizontal scrollbar associated with textedit 
 */
{
    SPEC *spec = (SPEC *)ob->spec;

    return spec->h_on ? spec->hsb : NULL;
}

void
fl_set_textedit_vscrollbar(FL_OBJECT *ob, int set)
/*
 * toggle textedit's vertical scrollbar
 * set = 0 - scrollbar off
 * set = 1 - scrollbar on
 * set = 2 - scrollbar auto
 */
{
SPEC *spec = (SPEC *)ob->spec;

  switch (set) {
   case 0:
    /* disable the scrollbar */
    if (spec->v_on)		{
	spec->v_on = 0;
	spec->sb->visible = 0;
	fl_hide_object(spec->sb);
	ob->w += spec->vw;
	fl_redraw_object(ob); 	}
    spec->flags &= ~FL_TEXTEDIT_VSBAR;
    spec->flags &= ~FL_TEXTEDIT_VSBAR_AUTO;
   break;

   case 1:
   /* enable the scrollbar */
   if (!spec->v_on) {
    spec->v_on = 1;
    spec->sb->visible = 1;
    fl_textedit_reset_vscrollbar(ob);
    ob->w -= spec->vw;
    fl_redraw_object(ob);
    fl_textedit_vscrollbar_dim(ob);
    fl_show_object(spec->sb);
			   }
    spec->flags |= FL_TEXTEDIT_VSBAR;
    spec->flags &= ~FL_TEXTEDIT_VSBAR_AUTO;
   break;

   case 2:
   /* auto switch scrollbar */
    spec->flags |= FL_TEXTEDIT_VSBAR;
    spec->flags |= FL_TEXTEDIT_VSBAR_AUTO;
    fl_textedit_switch_vscrollbar(ob);
   break;
				}

   return;
}

void
fl_set_textedit_hscrollbar(FL_OBJECT *ob, int set)
/*
 * toggle textedit's horizontal scrollbar
 * set = 0 - scrollbar off
 * set = 1 - scrollbar on
 * set = 2 - scrollbar auto
 */
{
SPEC *spec = (SPEC *)ob->spec;

  switch (set) {
   case 0:
    /* disable the scrollbar */
    if (spec->h_on) 	{
	spec->h_on = 0;
	spec->hsb->visible = 0;
	fl_hide_object(spec->hsb);
	ob->h += spec->hh;
	fl_redraw_object(ob); 	}
    spec->flags &= ~FL_TEXTEDIT_HSBAR;
    spec->flags &= ~FL_TEXTEDIT_HSBAR_AUTO;
   break;

   case 1:
   /* enable the scrollbar */
   if (!spec->h_on) {
    spec->h_on = 0;
    spec->hsb->visible = 0;
    fl_textedit_reset_hscrollbar(ob);
    ob->h -= spec->hh;
    fl_redraw_object(ob);
    fl_textedit_hscrollbar_dim(ob);
    fl_show_object(spec->hsb);
    if (spec->sb->visible) {
	 fl_textedit_vscrollbar_dim(ob);
	 fl_redraw_object(spec->sb); }
			   }
    spec->flags |= FL_TEXTEDIT_HSBAR;
    spec->flags &= ~FL_TEXTEDIT_HSBAR_AUTO;
   break;

   case 2:
   /* auto switch scrollbar */
    spec->flags |= FL_TEXTEDIT_HSBAR;
    spec->flags |= FL_TEXTEDIT_HSBAR_AUTO;
    fl_textedit_switch_hscrollbar(ob);
   break;
				}

   return;
}

void
fl_clear_textedit(FL_OBJECT *ob)
/*
 * clear text area
 */
{
SPEC *spec = (SPEC *)ob->spec;
TextBuf *tb = &spec->tb;
 
    tb_clear(&spec->tb);
    spec->r = 0;
    spec->c = 0;
    spec->sselr = -1;
    spec->eselr = -1;
    spec->cpos = 0;
    spec->topline = 0;
    spec->leftcol = 0;
    if (spec->exp)	  {
	free(spec->exp);
	spec->exp = NULL; }

    fl_freeze_form(ob->form);
    fl_textedit_reset_vscrollbar(ob);
    fl_textedit_switch_vscrollbar(ob);
    fl_textedit_reset_hscrollbar(ob);
    fl_textedit_switch_hscrollbar(ob);
    if (spec->cur_callback) (spec->cur_callback)(ob, tb_return_line(tb), 0, spec->r, spec->c);
    fl_redraw_object(ob);
    fl_unfreeze_form(ob->form);
}

int
fl_textedit_issel_pos(FL_OBJECT *ob, TextLine *tl, int r, int c)
/*
 * check if specific position in textline is selected
 */
{
SPEC    *spec = (SPEC *)ob->spec;

    if ((r > spec->sselr) && (r < spec->eselr))
	return 1;
    else
    if ((r == spec->sselr) && (r == spec->eselr)) {
	if ((spec->sselc == 0) && (spec->eselc > 0) &&
	 ((spec->eselc == -1) || (spec->eselc >= tl->strlen)))
		return 1;
	else
	if ((c >= spec->sselc) && (c < spec->eselc))
		return 1;
						  }
    else
    if (r == spec->sselr) {
	if ((c >= spec->sselc) &&
		((c < tl->strlen) || (spec->sselc == 0)))
		return 1;
			  }
    else
    if (r == spec->eselr) {
	if ((c < spec->eselc) || (spec->eselc == -1) ||
		(spec->eselc >= tl->strlen))
		return 1;
			  }

    return 0;
}

static int
fl_textedit_movecursor_visible(FL_OBJECT *ob, int r, int c)
/*
 * move cursor to specified position and make it visible
 */
{
SPEC    *spec = (SPEC *)ob->spec;
TextBuf *tb = &spec->tb;
int cdraw = 0, newtopline = -1;

  if (fl_textedit_movecursor(ob, r, c) == 0)
	return 0;

  if ((r > 0) && (r < spec->topline))	{
	newtopline = r;
	cdraw = 1;			}
  else
  if ((r >= (spec->topline + spec->wsize)) &&
	(r < (tb_get_nlines(tb) - 1)))	{
	newtopline = r;
	cdraw = 1;			}

  if ((c > 0) && (c < spec->leftcol))	{
	spec->leftcol = c - spec->csize;
	if (spec->leftcol < 0)
		spec->leftcol = 0;
	cdraw = 2;			}
  else
  if (c >= (spec->leftcol + spec->csize))	{
	spec->leftcol = c - 1;
	cdraw = 2;				}

  switch (cdraw) {
   case 1:
	fl_textedit_set_topline(ob, newtopline, 1);
   break;

   case 2:
	fl_textedit_draw_screen(ob);
	fl_textedit_set_hscrollbar(ob);
   break;
		 }

  return fl_textedit_movecursor(ob, r, c);
}

static int
fl_textedit_movecursor(FL_OBJECT *ob, int r, int c)
/*
 * move cursor to specified position
 */
{
SPEC    *spec = (SPEC *)ob->spec;
TextBuf *tb = &spec->tb;
int	tx, ty, tw;		/* bounding box of line */
int x, y, w, h, swidth, rc, attr, bgcol, fgcol, i;
char onechar[2];
TextLine *tl;

    fl_textedit_get_textbb(ob, &x, &y, &w, &h);

    tl = tb_get_lineptr_by_num(tb, spec->r);
    if (!tl) 		{
      if (!spec->r && !spec->c) 	      {
	swidth = fl_get_string_width(spec->text_style, spec->text_size, " ", 1);
	fl_rectf(x, y, swidth, spec->charheight, ob->col2); }
	goto showcur;   }
    
    /* line is out of visible range */
    if ((spec->r < spec->topline) || (spec->r >= spec->topline + spec->wsize))
	goto showcur;

    if (spec->c > tl->strlen)
	spec->c = tl->strlen;
    rc = spec->c - spec->leftcol;

    /* cursor is beyond the left margin */
    if (rc < 0)
	goto showcur;

    onechar[0] = (spec->c == tl->strlen) ? ' ' : tl->buf[spec->c];
    onechar[1] = '\0';

    attr = (spec->c == tl->strlen) ? ATTR_EMPTY : tl->attr[spec->c];
    if (fl_textedit_issel_pos(ob, tl, spec->r, spec->c))	{
     if (attr & ATTR_ISSET)
	attr |= ATTR_SELECT;
     else
	attr = ATTR_ISSET|ATTR_SELECT;				}

    bgcol = tl->bgcolor;
    if (attr & ATTR_ISSET)	{
     if ((fgcol = (attr & ATTR_COLOR_MASK)) == 0)
	fgcol = tl->fgcolor;
     if (attr & ATTR_SELECT) {
	i = fgcol;
	fgcol = bgcol;
	bgcol = i;	     }
				}
    else
	fgcol = tl->fgcolor;

    /* get string width up to cursor position */
    swidth = fl_get_string_width(spec->text_style, spec->text_size, tl->buf + spec->leftcol, rc);
    tw = fl_get_string_width(spec->text_style, spec->text_size,onechar, 1);

    /* cursor is beyond the right margin */
    if ((swidth + tw) > (ob->w - 2*abs(ob->bw) - 2*TEXT_X_OFFSET))
	goto showcur;

    tx = x + swidth;
    ty = y + (spec->r - spec->topline) * spec->charheight;

    /* erase the current line */
    fl_rectf(tx, ty, tw, spec->charheight, bgcol);

    fl_drw_text_cursor(FL_ALIGN_TOP_LEFT, tx, ty, tw, spec->charheight, fgcol, spec->text_style, spec->text_size, onechar, spec->ccol, -1);

    if ((attr & ATTR_ISSET) && (attr & ATTR_UNDERLINE))		{
	fl_color(fgcol);
fl_fheight = fl_get_char_height(spec->text_style, spec->text_size, &fl_fasc, &fl_fdesc);
	fl_textedit_underline_text(ob,
			tx, ty + fl_fheight - fl_fdesc, tw);	}

showcur:
    tl = tb_get_lineptr_by_num(tb, r);
    if (!tl) {
	if (!r && !c) {
	 spec->r = spec->c = 0;

	 if (!(spec->flags & FL_TEXTEDIT_NOCUR))
		fl_textedit_draw_cursor(ob, x, y);
	 return 0;
				  }
	return -1;
	     }
    
    /* line is out of visible range */
    if ((r < spec->topline) || (r >= spec->topline + spec->wsize))
	return -1;

    if (c > tl->strlen)
	c = tl->strlen;
    rc = c - spec->leftcol;

    /* cursor is beyond the left margin */
    if (rc < 0)    {
	spec->r = r;
	spec->c = c;
	return -1; }

    onechar[0] = (c >= tl->strlen) ? ' ' : tl->buf[c];
    onechar[1] = '\0';

    attr = (c >= tl->strlen) ? ATTR_EMPTY : tl->attr[c];
    if (fl_textedit_issel_pos(ob, tl, r, c))	{
     if (attr & ATTR_ISSET)
	attr |= ATTR_SELECT;
     else
	attr = ATTR_ISSET|ATTR_SELECT;		}

    bgcol = tl->bgcolor;
    if (attr & ATTR_ISSET)	{
     if ((fgcol = (attr & ATTR_COLOR_MASK)) == 0)
	fgcol = tl->fgcolor;
     if (attr & ATTR_SELECT) {
	i = fgcol;
	fgcol = bgcol;
	bgcol = i;	     }
				}
    else
	fgcol = tl->fgcolor;

    /* get string width up to cursor position */
    swidth = fl_get_string_width(spec->text_style, spec->text_size, tl->buf + spec->leftcol, rc);
    tw = fl_get_string_width(spec->text_style, spec->text_size,onechar, 1);

    /* cursor is beyond the right margin */
    if ((swidth + tw) > (ob->w - 2*abs(ob->bw) - 2*TEXT_X_OFFSET)) {
	spec->r = r;
	spec->c = c;
	return -1;					    	   }

    tx = x + swidth;
    ty = y + (r - spec->topline) * spec->charheight;

    /* draw the cursor */
    if (!(spec->flags & FL_TEXTEDIT_NOCUR))
	fl_textedit_draw_cursor(ob, tx, ty);

    spec->r = r;
    spec->c = c;
    tb_set_current_line(tb, spec->r);

    if (spec->cur_callback)
	(spec->cur_callback)(ob, tl->buf, 0, spec->r, spec->c);

    return 0;
}

void
fl_calc_cursorpos(FL_OBJECT *ob, u_long cofft, int *cx, int *cy)
/*
 * Calculate cursor position from offset
 */
{
SPEC    *spec = (SPEC *)ob->spec;
TextBuf *tb;
int i, len;
char *line;

 tb = &spec->tb;
 *cx = 0;
 *cy = 0;
 /* scan all lines */
 for (i = 0; i < tb_get_nlines(tb);i++)	{
	tb_get_line_by_num(tb, &line, i);
	if (!line)
		break;
	len = strlen(line) + 1;
	if (len < cofft) {
		cofft -= len;
		(*cy)++; }
	 else		 {
		*cx = cofft;
		break;	 }
					}
}

void
fl_set_textedit_cursorpos(FL_OBJECT *ob, int cx, int cy, long cofft, int screen)
/*
 * set position of the cursor
 */
{
 SPEC    *spec = (SPEC *)ob->spec;
 TextBuf *tb;
 int c;
 char *line;
 Window wn;

    tb = &spec->tb;

    /* if offset specified then calculate the cursor position from it */
    if (cofft >= 0)
	fl_calc_cursorpos(ob, cofft, &cx, &cy);

    /* screen (and not text) coordinates are provided */
    if (screen) {
	cy += spec->topline;
	cx += spec->leftcol;
		}

    /* set the current line to the right one, but check if it's not
       out of range */
    if (spec->topline + cy >= tb_get_nlines(tb))
	return;

    tb_set_current_line(tb, spec->r);

    /* now calculate which character the cx position points to */
    /* first get the current line */
    tb_get_line(tb, &line);

    if (!line || !*line)
	c = 0;
    else
	c = (cx > strlen(line)) ? strlen(line) : cx;
    spec->cpos = c;

    wn = fl_winget();
    fl_winset(ob->form->window);
    if ((cy < spec->topline) ||
	(cy >= spec->topline + fl_get_textedit_screenlines(ob))) {
	spec->topline = spec->r = cy;
	spec->c = c;
	fl_textedit_set_vscrollbar(ob);
	fl_redraw_object(ob);
								 }
    else
	fl_textedit_movecursor(ob, cy, c);
    fl_winset(wn);

    return;
}

void
fl_get_textedit_cursorpos(FL_OBJECT *ob, int *cx, int *cy, u_long *cofft)
/*
 * get position of the cursor
 */
{
 SPEC    *spec = (SPEC *)ob->spec;
 char *line;
 int i;

 if (cx)
	*cx = spec->c;

 if (cy)
	*cy = spec->r;

 if (cofft) 	{
  *cofft = 0;
  if (spec->r == 0) {
	*cofft = spec->c;
	return;     }

  for (i = 0; i < spec->r; i++) {
    tb_get_line_by_num(&spec->tb, &line, i);
    if (!line)
	break;

    *cofft += strlen(line) + 1; }

  *cofft += spec->c;
		}
}

int
fl_set_textedit_wrap(FL_OBJECT *ob, int wrap, int all)
/*
 * set word/line wrap
 */
{
SPEC    *spec = (SPEC *)ob->spec;
TextBuf *tb;
int owrap;

 tb = &spec->tb;
 owrap = tb_get_linewrap(tb);
 tb_set_linewrap(tb, wrap);

  if (all && (tb_get_nlines(tb) > 0)) {
        tb_set_current_line(tb, 0);
        tb_wrap_line(tb);
        while (tb_set_next_line(tb))
	 tb_wrap_line(tb);
	fl_redraw_object(ob);
    }

 return owrap;
}

void
fl_insert_textedit(FL_OBJECT *ob, char *buf)
/*
 * insert <buf> at cursor position
 */
{
SPEC    *spec = (SPEC *)ob->spec;
TextBuf *tb = &spec->tb;
u_long offt, buflen;
int lines, newr = spec->r, newc = spec->c;
Window wn;

 if (!buf || !*buf)
	return;

 fl_get_textedit_cursorpos(ob, NULL, NULL, &offt);
 lines = tb_get_nlines(tb);
 buflen = tb_get_textlen(tb);
 tb_insert_block(tb, spec->r, spec->c, buf);
 buflen = tb_get_textlen(tb) -  buflen;
 if (buflen > 0)
  fl_calc_cursorpos(ob, offt + buflen, &newc, &newr);
 wn = fl_winget();
 fl_winset(ob->form->window);
 fl_textedit_movecursor_visible(ob, newr, newc);
 fl_textedit_refresh_screen(ob, 0);
 if (lines != tb_get_nlines(tb))
  fl_textedit_set_vscrollbar_max(ob);
 fl_textedit_set_hscrollbar_max(ob);
 fl_winset(wn);

 if (spec->cur_callback) (spec->cur_callback)(ob, tb_return_line(tb), 0, spec->r, spec->c);
}

void
fl_insert_textedit_file(FL_OBJECT *ob, char *fname)
/*
 * insert contents of file <fname> at cursor position
 */
{
SPEC    *spec = (SPEC *)ob->spec;
TextBuf *tb;

 tb = &spec->tb;
 if (!fname)
	return;

 tb_insert_file(tb, spec->r, spec->c, fname);
 if (spec->c > tb_get_linelen(tb))
	spec->c = tb_get_linelen(tb);
 fl_redraw_object(ob);

 fl_textedit_set_vscrollbar_wsize(ob);
 fl_textedit_set_hscrollbar_wsize(ob);
}

int
fl_textedit_readonly(FL_OBJECT *ob, int readonly)
/*
 * set/remove read-only flag
 */
{
SPEC    *spec = (SPEC *)ob->spec;
int oronly;
Window wn;

 oronly = spec->flags & FL_TEXTEDIT_READONLY;

 if (readonly)	{
	spec->flags |= FL_TEXTEDIT_READONLY;
	spec->flags |= FL_TEXTEDIT_NOCUR;
		}
 else	{
	spec->flags &= ~FL_TEXTEDIT_READONLY;
	spec->flags &= ~FL_TEXTEDIT_NOCUR;
	}

 if (ob->form->visible && !ob->form->frozen) {
  wn = fl_winget();
  fl_winset(ob->form->window);
  fl_textedit_movecursor(ob, spec->r, spec->c);
  fl_winset(wn);
				             }

 return oronly;
}

int
fl_get_textedit_topline(FL_OBJECT *ob)
/*
 * Desc: return number of the first line displayed
 */
{
SPEC    *spec = (SPEC *)ob->spec;

 return spec->topline;
}

int
fl_get_textedit_maxline(FL_OBJECT *ob)
/*
 * Desc: return number of lines in textbuffer
 */
{
SPEC    *spec = (SPEC *)ob->spec;
TextBuf *tb;

 tb = &spec->tb;

 return tb->n;
}

int
fl_get_textedit_screenlines(FL_OBJECT *ob)
/*
 * Desc: return number of lines in textbuffer
 */
{
SPEC    *spec = (SPEC *)ob->spec;

    return spec->wsize;
}

char *
fl_get_textedit_curline(FL_OBJECT *ob)
/*
 * Desc: returns text of the line with cursor
 */
{
SPEC    *spec = (SPEC *)ob->spec;
TextBuf *tb;

 tb = &spec->tb;

 if (tb->currentline)
	return tb->currentline->buf;
 else
	return NULL;
}

char *
fl_get_textedit_line(FL_OBJECT *ob, int n)
/*
 * Desc: returns text of the line number <n>
 */
{
SPEC    *spec = (SPEC *)ob->spec;
TextBuf *tb;
char *line;

 tb = &spec->tb;

 tb_get_line_by_num(tb, &line, n);

 return line;
}

void
fl_add_textedit_line(FL_OBJECT *ob, char *line)
/*
 * Desc: append line to text buffer
 */
{
SPEC    *spec = (SPEC *)ob->spec;
int maxc;
TextBuf *tb;
Window wn;

 tb = &spec->tb;
 maxc = tb->maxchars;
 tb_append_line(tb, line);

 wn = fl_winget();
 fl_winset(ob->form->window);
 fl_textedit_refresh_screen(ob, 0);
 fl_textedit_set_vscrollbar(ob);
 fl_textedit_set_vscrollbar_max(ob);
 if (maxc != tb->maxchars)
	fl_textedit_set_hscrollbar_max(ob);
 fl_winset(wn);

 return;
}

void
fl_set_textedit_topline(FL_OBJECT *ob, int n)
/*
 * Desc: set topline to <n>
 */
{
Window wn;

 if ((n < 0) || (n > fl_get_textedit_maxline(ob)))
	return;

 wn = fl_winget();
 fl_winset(ob->form->window);
 fl_textedit_set_topline(ob, n, 1);
 fl_winset(wn);

 return;
}

void
fl_addto_textedit(FL_OBJECT *ob, char *line)
/*
 * Desc: append line to text buffer and make it visible
 */
{
SPEC    *spec = (SPEC *)ob->spec;
TextBuf *tb;
int slines, maxc;
Window wn;

 tb = &spec->tb;
 maxc = tb->maxchars;
 tb_append_line(tb, line);

 wn = fl_winget();
 fl_winset(ob->form->window);
 slines = fl_get_textedit_screenlines(ob);
 if (tb->n > (spec->topline + slines))
	fl_set_textedit_topline(ob, tb->n - slines);
 else
	fl_textedit_refresh_screen(ob, 1);

 fl_textedit_set_vscrollbar_max(ob);
 if (maxc != tb->maxchars)
	fl_textedit_set_hscrollbar_max(ob);
 fl_winset(wn);

 return;
}

int
fl_textedit_line_visible(FL_OBJECT *ob, int line)
/*
 * Desc: check if <line> is visible
 */
{
SPEC    *spec = (SPEC *)ob->spec;

 if ((line >= spec->topline) &&
	(line < (spec->topline + fl_get_textedit_screenlines(ob))))
	return 1;

 return 0;
}

void
fl_insert_textedit_line(FL_OBJECT *ob, int where, char *line)
/*
 * Desc: insert new line after line number <where>
 */
{
SPEC    *spec = (SPEC *)ob->spec;
TextBuf *tb;
TextLine *tl;
int maxc;
Window wn;

 tb = &spec->tb;
 maxc = tb->maxchars;
 if ((where < 0) || (where > tb->n))
	return;

 tl = tb->currentline;

 if (!tb_set_current_line(tb, where))
	return;

 tb_insert_line(tb, line);
 tb->currentline = tl;

 wn = fl_winget();
 fl_winset(ob->form->window);
 if (fl_textedit_line_visible(ob, where) || (where < spec->topline))
	fl_textedit_refresh_screen(ob, 1);

 fl_textedit_set_vscrollbar_max(ob);
 if (maxc != tb->maxchars)
	fl_textedit_set_hscrollbar_max(ob);
 fl_winset(wn);
}

void
fl_delete_textedit_line(FL_OBJECT *ob, int line)
/*
 * Desc: delete line with number <line>
 */
{
SPEC    *spec = (SPEC *)ob->spec;
TextBuf *tb;
TextLine *tl;
Window wn;

 tb = &spec->tb;
 if ((line < 0) || (line > tb->n))
	return;

 tl = tb->currentline;

 if (!tb_set_current_line(tb, line))
	return;

 tb_del_line(tb);
 tb->currentline = tl;

 if (spec->r > tb->n) 	{
	spec->r = tb->n;
	if (spec->cur_callback) (spec->cur_callback)(ob, tb_return_line(tb), 0, spec->r, spec->c);
			}

 if (spec->topline > tb->n)
	spec->topline = tb->n;

 wn = fl_winget();
 fl_winset(ob->form->window);
 if (fl_textedit_line_visible(ob, line) || (line < spec->topline))
	fl_textedit_refresh_screen(ob, 1);

 fl_textedit_set_vscrollbar_max(ob);
 fl_winset(wn);
}

void
fl_append_to_textedit_line(FL_OBJECT *ob, int line, char *buf)
/*
 * Desc: appends contents of <buf> to line <line>
 */
{
SPEC    *spec = (SPEC *)ob->spec;
Window wn;
TextBuf *tb;
TextLine *tl;
int n, maxc;

 tb = &spec->tb;
 maxc = tb->maxchars;
 if ((line < -1) || (line > tb->n))
	return;

 tl = tb->currentline;
 n = tb->n;

 if (!tb_set_current_line(tb, line))
	return;

 tb_append_to_line(tb, buf);

 wn = fl_winget();
 fl_winset(ob->form->window);
 if (n != tb->n) {
  tb_modify_lines(tb);
  fl_textedit_refresh_screen(ob, 0);
  fl_textedit_set_vscrollbar_max(ob);
		 }
 else
  fl_textedit_draw_line(ob, tb->i);

 if (maxc != tb->maxchars)
	fl_textedit_set_hscrollbar_max(ob);

 fl_winset(wn);
 tb->currentline = tl;

 return;
}

void
fl_replace_textedit_line(FL_OBJECT *ob, int line, char *nline)
/*
 * Desc: replace contents of line <line> with <nline>
 */
{
SPEC    *spec = (SPEC *)ob->spec;
Window wn;
TextBuf *tb;
TextLine *tl;
int n, maxc;

 tb = &spec->tb;
 maxc = tb->maxchars;
 if ((line < -1) || (line > tb->n))
	return;

 tl = tb->currentline;

 if (!tb_set_current_line(tb, line))
	return;

 n = tb->n;
 tb_del_line(tb);
 tb_insert_line(tb, nline);

 tb->currentline = tl;

 wn = fl_winget();
 fl_winset(ob->form->window);
 if (n != tb->n) {
  if (fl_textedit_line_visible(ob, line) || (line < spec->topline))
	fl_textedit_refresh_screen(ob, 1);

  fl_textedit_set_vscrollbar_max(ob);
		 }
 else
  fl_textedit_draw_line(ob, line);

 if (maxc != tb->maxchars)
	fl_textedit_set_hscrollbar_max(ob);
 fl_winset(wn);
}

void
fl_deselect_textedit(FL_OBJECT *ob)
/*
 * Desc: deselect any selected text
 */
{
Window wn;

  wn = fl_winget();
  fl_winset(ob->form->window);
  fl_textedit_remove_selection(ob);
  fl_winset(wn);
}

int
fl_isselected_textedit_line(FL_OBJECT *ob, int line)
/*
 * Desc: check if <line> is selected (at least part of it)
 */
{
SPEC    *spec = (SPEC *)ob->spec;

 if ((line >= spec->sselr) && (line <= spec->eselr)) 	{
  if ((line == spec->sselr) && (line == spec->eselr))	{
	if (spec->sselc != spec->eselc)
		return 1;
	else
		return 0;				}

	return 1;					}

 return 0;
}

void
fl_select_textedit_line(FL_OBJECT *ob, int line)
/*
 * Select <line>
 */
{
SPEC    *spec = (SPEC *)ob->spec;
Window wn;

 wn = fl_winget();
 fl_winset(ob->form->window);
 fl_textedit_remove_selection(ob);

 spec->sselr = spec->eselr = line;
 spec->sselc = 0;
 spec->eselc = -1;

 fl_textedit_draw_selection(ob);
 fl_winset(wn);
}

void
fl_deselect_textedit_line(FL_OBJECT *ob, int line)
/*
 * Deselect <line>
 */
{
 fl_deselect_textedit(ob);

 return;
}

void
fl_set_textedit_line_color(FL_OBJECT *ob, int line, int bgcol, int fgcol)
/*
 * Desc: set back and foreground colors of <line>
 */
{
SPEC    *spec = (SPEC *)ob->spec;
TextBuf *tb;
Window wn;

 tb = &spec->tb;

 if (!tb_set_current_line(tb, line))
	return;

 if (bgcol > 0)
	tb_set_linebgcolor(tb, bgcol);

 if (fgcol > 0)
	tb_set_linefgcolor(tb, fgcol);

 wn = fl_winget();
 fl_winset(ob->form->window);
 fl_textedit_draw_line(ob, line);
 fl_winset(wn);
}

void
fl_get_textedit_line_color(FL_OBJECT *ob, int line, int *bgcol, int *fgcol)
/*
 * Desc: get back and foreground colors of <line>
 */
{
SPEC    *spec = (SPEC *)ob->spec;
TextBuf *tb;
TextLine *tl;

 tb = &spec->tb;

 if ((tl = tb_get_lineptr_by_num(tb, line)) == NULL)
	return;

 if (bgcol)
	*bgcol = tl->bgcolor;

 if (fgcol)
	*fgcol = tl->fgcolor;
}

void
fl_scroll_textedit(FL_OBJECT *ob, int type)
/*
 * Desc: scroll textedit according to type
 */
{
SPEC    *spec = (SPEC *)ob->spec;
TextBuf *tb = &spec->tb;
Window wn;

 wn = fl_winget();
 fl_winset(ob->form->window);

 switch (type) 	{
  case FL_TEXTEDITSCROLL_PGUP:
	fl_textedit_pageup(ob);
  break;

  case FL_TEXTEDITSCROLL_PGDOWN:
	fl_textedit_pagedown(ob);
  break;

  case FL_TEXTEDITSCROLL_LINEUP:
	fl_textedit_lineup(ob);
  break;

  case FL_TEXTEDITSCROLL_LINEDOWN:
	fl_textedit_linedown(ob);
  break;

  case FL_TEXTEDITSCROLL_TOP:
	fl_textedit_set_topline(ob, 0, 1);
  break;

  case FL_TEXTEDITSCROLL_BOTTOM:
	if (tb->n > spec->wsize)
		fl_textedit_set_topline(ob, tb->n - 1, 1);
  break;
		}

 fl_winset(wn);
}

textedit_callback
fl_textedit_set_callback(FL_OBJECT *ob, textedit_callback callback, int type)
{
SPEC    *spec = (SPEC *)ob->spec;
textedit_callback ocallback = NULL;

 switch (type)	{
  case FL_TEXTEDIT_KEY_CALLBACK:
   ocallback = spec->key_callback;
   spec->key_callback = callback;
  break;

  case FL_TEXTEDIT_CLK_CALLBACK:
   ocallback = spec->clk_callback;
   spec->clk_callback = callback;
  break;

  case FL_TEXTEDIT_DBL_CALLBACK:
   ocallback = spec->dbl_callback;
   spec->dbl_callback = callback;
  break;

  case FL_TEXTEDIT_CUR_CALLBACK:
   ocallback = spec->cur_callback;
   spec->cur_callback = callback;
  break;
		}

 return ocallback;
}

void
fl_textedit_map_key(int function, long key, int add)
/*
 * Desc: bind key to specified function
 * if key < 0 then restore default for this function
 * if key = 0 delete binding
 * if add != 0 then key will work in addition to all previously defined
 */
{
int i = 0, b = 0;
long keydef = key;

 while ((bindings[i].function != FL_TEXTKEY_ENDARRAY) &&
	(i < FL_TEXTKEY_MAX)) {

  if (bindings[i].function == function) {
   if (add)
	keydef = bindings[i].keydef;
   else {
    if (b == 0)  {
     if (key < 0)
	bindings[i].key = bindings[i].keydef;
     else
	bindings[i].key = key;
	 	 }
    else
	bindings[i].key = 0;
	}
   b++;
					}

  i++;
			      }

 if (add) {
   if (b > FL_TEXTKEY_MAXBIND)
	return;

   i = 0;
   while ((bindings[i].function != FL_TEXTKEY_ENDARRAY) &&
	(i < FL_TEXTKEY_MAX)) 	{
    if ((bindings[i].function == FL_TEXTKEY_NONE) ||
	((bindings[i].function == function) &&
		(bindings[i].key == 0)))	 {
     if (bindings[i].function == FL_TEXTKEY_NONE)
	bindings[i].keydef = keydef;
     bindings[i].function = function;
     bindings[i].key = key;
     return;					 }

    i++;
				}

   if (i >= FL_TEXTKEY_MAX)
	return;

   bindings[i].function = function;
   bindings[i].key = key;
   bindings[i++].keydef = keydef;

   bindings[i].function = FL_TEXTKEY_ENDARRAY;
	 }

 return;
}

int
fl_textedit_set_key(int function, long *key)
/*
 * Desc: set key mappings for specified function
 * key points to array of long with legth of FL_TEXTKEY_MAXBIND
 * uninitialized elements of the array are set to -1
 */
{
int b, i = 0;

 if (!key)
	return -1;

 fl_textedit_map_key(function, 0, 0);
 for (b = 0; b < FL_TEXTKEY_MAXBIND; b++)		{
   if (key[b] > 0)	{
	fl_textedit_map_key(function, key[b], 1);
	i++;		}
							}

 return i ? 0 : -1;
}

void
fl_textedit_get_key(int function, long *key)
/*
 * Desc: returns all keys bound to specified function
 * key points to array of long with legth of FL_TEXTKEY_MAXBIND
 * uninitialized elements of the array are set to -1
 */
{
int i, b;

 for (i = 0; i < FL_TEXTKEY_MAXBIND; i++)
	key[i] = -1;

 b = i = 0;

 while ((bindings[i].function != FL_TEXTKEY_ENDARRAY) &&
	(i < FL_TEXTKEY_MAX)) 	{

  if (bindings[i].function == function) {
   key[b++] = bindings[i].key;
   if (b >= FL_TEXTKEY_MAXBIND)
	return;
					}

  i++;
				}

 return;
}

int
fl_textedit_key_remapped(int function)
/*
 * Desc: check if specified key has been remapped
 */
{
int i = 0;

 while ((bindings[i].function != FL_TEXTKEY_ENDARRAY) &&
	(i < FL_TEXTKEY_MAX)) 	{
  if ((bindings[i].function == function) &&
	(bindings[i].key != 0) &&
	(bindings[i].key != bindings[i].keydef))
	return 1;

  i++;				}

 return 0;
}

digraph *
fl_textedit_get_digraphs()
/* 
 * Desc: returns pointer to array of digraph mappings
 */
{
 return digraphs;
}

keybind *
fl_textedit_get_keymap()
/* 
 * Desc: returns pointer to array of key mappings
 */
{
 return bindings;
}

void
fl_textedit_set_keymap(keybind *keymap)
/*
 * Desc: sets internal keymap
 * if keymap is NULL restores the default key map
 */
{
int i;

 if (!keymap) 	{
   i = 0;
   while ((bindings[i].function != FL_TEXTKEY_ENDARRAY) &&
	(i < FL_TEXTKEY_MAX)) 	{
     bindings[i].key = bindings[i].keydef;
     i++;			}

   return;
		}

 i = 0;
 while ((keymap[i].function != FL_TEXTKEY_ENDARRAY) &&
	(i < FL_TEXTKEY_MAX))
	fl_textedit_map_key(keymap[i].function, 0, 0);

 i = 0;
 while ((keymap[i].function != FL_TEXTKEY_ENDARRAY) &&
	(i < FL_TEXTKEY_MAX))
	fl_textedit_map_key(keymap[i].function, keymap[i].key, 1);

 return;
}

void
fl_set_textedit_editkeymap(const FL_EditKeymap *keymap)
/*
 * Desc: sets internal keymap according to supplied structure
 * if keymap is NULL restores the default key map
 */
{
int i;

 if (!keymap) 	{
   i = 0;
   while ((bindings[i].function != FL_TEXTKEY_ENDARRAY) &&
	(i < FL_TEXTKEY_MAX)) 	{
     bindings[i].key = bindings[i].keydef;
     i++;			}

   return;
		}

 if (keymap->del_prev_char != 0)
	fl_textedit_map_key(FL_TEXTKEY_BACKSPACE, keymap->del_prev_char, 0);

 if (keymap->del_next_char != 0)
	fl_textedit_map_key(FL_TEXTKEY_DELETE, keymap->del_next_char, 0);

 if (keymap->moveto_prev_line != 0)
	fl_textedit_map_key(FL_TEXTKEY_UP, keymap->moveto_prev_line, 0);

 if (keymap->moveto_next_line != 0)
	fl_textedit_map_key(FL_TEXTKEY_DOWN, keymap->moveto_next_line, 0);

 if (keymap->moveto_prev_char != 0)
	fl_textedit_map_key(FL_TEXTKEY_LEFT, keymap->moveto_prev_char, 0);

 if (keymap->moveto_next_char != 0)
	fl_textedit_map_key(FL_TEXTKEY_RIGHT, keymap->moveto_next_char, 0);

 if (keymap->moveto_prev_page != 0)
	fl_textedit_map_key(FL_TEXTKEY_PAGEUP, keymap->moveto_prev_page, 0);

 if (keymap->moveto_next_page != 0)
	fl_textedit_map_key(FL_TEXTKEY_PAGEDOWN, keymap->moveto_next_page, 0);

 if (keymap->moveto_bol != 0)
	fl_textedit_map_key(FL_TEXTKEY_LBEGIN, keymap->moveto_bol, 0);

 if (keymap->moveto_eol != 0)
	fl_textedit_map_key(FL_TEXTKEY_LEND, keymap->moveto_eol, 0);

 if (keymap->moveto_bof != 0)
	fl_textedit_map_key(FL_TEXTKEY_HOME, keymap->moveto_bof, 0);

 if (keymap->moveto_eof != 0)
	fl_textedit_map_key(FL_TEXTKEY_END, keymap->moveto_eof, 0);

 if (keymap->paste != 0)
	fl_textedit_map_key(FL_TEXTKEY_PASTE, keymap->paste, 0);

 if (keymap->backspace != 0)
	fl_textedit_map_key(FL_TEXTKEY_BACKSPACE, keymap->backspace, 0);

 if (keymap->del_to_eol != 0)
	fl_textedit_map_key(FL_TEXTKEY_LKILL, keymap->del_to_eol, 0);

 if (keymap->clear_field != 0)
	fl_textedit_map_key(FL_TEXTKEY_CLEAR, keymap->clear_field, 0);

 if (keymap->moveto_prev_word != 0)
	fl_textedit_map_key(FL_TEXTKEY_WORDLEFT, keymap->moveto_prev_word, 0);

 if (keymap->moveto_next_word != 0)
	fl_textedit_map_key(FL_TEXTKEY_WORDRIGHT, keymap->moveto_next_word, 0);

 if (keymap->del_prev_word != 0)
	fl_textedit_map_key(FL_TEXTKEY_DELWORDL, keymap->del_prev_word, 0);

 if (keymap->del_next_word != 0)
	fl_textedit_map_key(FL_TEXTKEY_DELWORDR, keymap->del_next_word, 0);

 return;
}

void 
fl_textedit_replace_sel(FL_OBJECT *ob, char *replace)
/*
 * Desc: replaces selected text with new text
 */
{
  SPEC *spec = (SPEC *)ob->spec;
  TextBuf *tb = &spec->tb;
  Window wn;

  wn = fl_winget();
  fl_winset(ob->form->window);

  if (tb_del_block(tb, spec->sselr, spec->sselc, spec->eselr, spec->eselc)) {
    while (spec->sselr && !tb_set_current_line(tb, spec->sselr))
      spec->sselr--;
    if (spec->topline > spec->sselr)
	fl_textedit_set_topline(ob, spec->sselr, 1);
    fl_textedit_movecursor(ob, spec->sselr, spec->sselc);
    spec->sselr = -1;
    spec->eselr = -1;
    fl_textedit_refresh_screen(ob, 1);
    fl_textedit_set_vscrollbar_max(ob);
    fl_textedit_set_hscrollbar_max(ob);
  									    }
  else {
    spec->c = spec->sselc;
    if (spec->c > tb_get_linelen(tb))
      spec->c = tb_get_linelen(tb);
    fl_textedit_remove_selection(ob);
       }

  fl_insert_textedit(ob, replace);
  fl_winset(wn);

  return;
}

static const char *punctuation = ",.;:!\"?<>()[]{}@ ";

char *
fl_textedit_get_nextword(FL_OBJECT *ob, line_valid valid)
/*
 * Desc: selects and returns next word (after the cursor)
 * Each line is checked for validity if valis is not NULL
 */
{
  SPEC *spec = (SPEC *)ob->spec;
  TextBuf *tb = &spec->tb;
  char *p, *ret = NULL, *line;
  int oldr, len;
  Window wn;

  wn = fl_winget();
  fl_winset(ob->form->window);
again:  
  fl_textedit_remove_selection(ob);

  oldr = spec->r;
  line = tb_return_line(tb);

  while (line && valid && (valid)(line)) {
    oldr = spec->r;
    fl_textedit_linedown(ob);
    fl_textedit_movecursor(ob, spec->r, 0);
    if (oldr == spec->r) {
      fl_winset(wn);
      return NULL;       }
    line = tb_return_line(tb);
  					  }

  p = line + spec->c;
  if (p && *p) {
    len = strspn(p,punctuation);
    spec->c+=len;
    p+=len;			}

  spec->sselr=spec->r;
  spec->sselc=spec->c;

  if (line && *line && (spec->c <= strlen(line))) {
    /* one word right */
    len = strcspn(line+spec->c,punctuation);
    if (len) {
      p = line+spec->c+len;
      p += strspn(p,punctuation);
      if (*p) {
        ret = (char*)calloc(1,len+1);
        strncpy(ret,line+spec->c,len);
        spec->eselr = spec->r;
        spec->eselc = spec->c+len;
        fl_textedit_movecursor(ob, spec->r, p - line);
        fl_textedit_draw_selection(ob);
        fl_winset(wn);
        return ret; 
             }
    						   }
						  }

  if (!line || (strlen(line+spec->c) == 0)) {
    ret = NULL;
    len = 0;				    }
  else  {
    ret = strdup(line+spec->c);
    len = strcspn(ret,punctuation);
    ret[len] = '\0';
	}

  spec->eselr = spec->r;
  spec->eselc = spec->c+len;

  /* line down */
  fl_textedit_linedown(ob);
  line = tb_return_line(tb);
  if ((oldr != spec->r) && line) {
     p = line;
     p += strspn(p,punctuation);
     fl_textedit_movecursor(ob, spec->r, p - line);
     if (!ret)
	goto again;
     fl_textedit_draw_selection(ob);
     fl_winset(wn);
   				 }
  else
     fl_textedit_movecursor(ob, spec->r, line?strlen(line):0);

  return ret;
}

int
fl_set_textbuf_flags(FL_OBJECT *ob, int flags)
/*
 * Desc: set textbuf specific flags
 * Ret: old flags value. flags are not set if supplied value < 0
 */
{
SPEC *spec = (SPEC *)ob->spec;
TextBuf *tb = &spec->tb;

  return tb_set_flags(tb, flags);
}

int
fl_set_textedit_flags(FL_OBJECT *ob, int setflags, int unsetflags)
/*
 * Desc: set/unset textedit flags
 * Ret: old flags value. flags are not set if both supplied values < 0
 */
{
SPEC *spec = (SPEC *)ob->spec;
int oflags = spec->flags;

 if (setflags > 0)	{
  if (setflags & FL_TEXTEDIT_READONLY)
	spec->flags |= FL_TEXTEDIT_READONLY;
  if (setflags & FL_TEXTEDIT_NOCUR)
	spec->flags |= FL_TEXTEDIT_NOCUR;
  if (setflags & FL_TEXTEDIT_PASTE_CUR)
	spec->flags |= FL_TEXTEDIT_PASTE_CUR;
			}

 if (unsetflags > 0)	{
  if (setflags & FL_TEXTEDIT_READONLY)
	spec->flags &= ~FL_TEXTEDIT_READONLY;
  if (setflags & FL_TEXTEDIT_NOCUR)
	spec->flags &= ~FL_TEXTEDIT_NOCUR;
  if (setflags & FL_TEXTEDIT_PASTE_CUR)
	spec->flags &= ~FL_TEXTEDIT_PASTE_CUR;
			}

 return oflags;
}

int
fl_set_textedit_textattr(FL_OBJECT *ob, int attr)
/*
 * set new text attribute
 */
{
SPEC *spec = (SPEC *)ob->spec;
TextBuf *tb = &spec->tb;
int oldattr = tb->attr_def;

 if (attr != 0)
	tb->attr_def = (attr&0xff) | ATTR_ISSET;
 else
	tb->attr_def = ATTR_EMPTY;

 return oldattr & ~ATTR_ISSET;
}

void
fl_set_textedit_blockattr(FL_OBJECT *ob, int r0, int r1, int c0, int c1, int attr)
/*
 * set new block attribute
 */
{
SPEC *spec = (SPEC *)ob->spec;
TextBuf *tb = &spec->tb;
Window wn;

 wn = fl_winget();
 fl_winset(ob->form->window);
 if (r0 == -1)
  tb_set_block_attr(tb, spec->sselr, spec->sselc,
	spec->eselr, spec->eselc, (attr&0xff)|ATTR_ISSET);
 else
  tb_set_block_attr(tb, r0, c0, r1, c1, (attr&0xff)|ATTR_ISSET);
 fl_textedit_refresh_screen(ob, 1);
 fl_winset(wn);

 return;
}

textline_callback
fl_textedit_setline_callback(FL_OBJECT *ob, int line,
	textline_callback callback, int cbtype, int data)
/*
 * Desc: set callback on specific line
 */
{
SPEC *spec = (SPEC *)ob->spec;
TextBuf *tb = &spec->tb;
int cline = tb->i;
textline_callback ocb;

 if (!tb_set_current_line(tb, line))
	return NULL;
 ocb = tb_set_callback(tb, callback, cbtype, data);
 tb_set_current_line(tb, cline);
 return ocb;
}

