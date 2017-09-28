/********************************** xedit.h **********************************/
/*                                                                           */
/*****************************************************************************/
/*                                                                           */
/*        Authors:  BeBe Ly - NASA/Johnson Space Center                      */
/*                  Daniel J. McCoy - University of Houston - Downtown       */
/*                                                                           */
/*****************************************************************************/

String bindings[] =
  {
  "Ctrl-a   Begining Of Line",
  "Ctrl-b   Search for matching parenthesis",
  "Crtl-d   Delete Next Character",
  "Crtl-e   End Of Line",
  "Crtl-f   Forward Character",
  "Crtl-g   Multiply Reset",
  "Crtl-h   Delete Previous Character",
  "Crtl-j   Newline And Indent",
  "Crtl-k   Kill To End Of Line",
  "Crtl-l   Redraw Display",
  "Crtl-m   Newline",
  "Crtl-n   Next Line",
  "Crtl-o   Newline And Backup",
  "Crtl-p   Previous Line",
  "Crtl-r   Search/Replace Backward",
  "Crtl-s   Search/Replace Forward",
  "Crtl-t   Transpose Characters",
  "Crtl-u   Multiply by 4",
  "Crtl-v   Next Page",
  "Crtl-w   Kill Selection",
  "Crtl-y   Unkill",
  "Crtl-z   Scroll One Line Up",
  "Meta-b   Backward Word",
  "Meta-D   Kill Word",
  "Meta-d   Delete Next Word",
  "Meta-f   Forward Word",
  "Meta-H   Backward Kill Word",
  "Meta-h   Delete Previous Word",
  "Meta-i   Insert File",
  "Meta-k   Kill To End Of Paragraph",
  "Meta-q   Form Paragraph",
  "Meta-v   Previous Page",
  "Meta-y   Insert Current Selection",
  "Meta-z   Scroll One Line Down",
  "Meta-<   Begining Of File",
  "Meta->   End Of File",
  "Meta-]   Forward Paragraph",
  "Meta-[   Backward Paragraph",
  "Meta-Delete    Delete Previous Word",
  "Meta-Shift Delete    Kill Previous Word",
  "Meta-Backspace Delete Previous Word",
  "Meta-Shift Backspace Kill Previous Word",
  NULL,
  };

char *xclips_translation3 =
"\
Ctrl<Key>B:     balance() \n\
Ctrl<Key>C:     complete-construct-editor()\n\
";


