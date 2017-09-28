/* Form definition file generated with fdesign. */

#include "forms.h"
#include <stdlib.h>
#include "TCPforms.h"

FD_tcpscan *create_form_tcpscan(void)
{
  FL_OBJECT *obj;
  FD_tcpscan *fdui = (FD_tcpscan *) fl_calloc(1, sizeof(*fdui));

  fdui->tcpscan = fl_bgn_form(FL_NO_BOX, 851, 461);
  obj = fl_add_box(FL_FRAME_BOX,0,0,851,461,"");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
  obj = fl_add_labelframe(FL_ENGRAVED_FRAME,5,45,430,410,"Port Scan");
    fl_set_object_lcolor(obj,FL_CYAN);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
  obj = fl_add_labelframe(FL_ENGRAVED_FRAME,445,285,400,170,"Trace Route");
    fl_set_object_lcolor(obj,FL_CYAN);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
  obj = fl_add_button(FL_NORMAL_BUTTON,750,10,90,30,"Exit");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,TCPexitCB,0);
  fdui->tcp_suspicious = obj = fl_add_browser(FL_NORMAL_BROWSER,220,120,210,315,"TCP Ports Suspicious");
    fl_set_object_boxtype(obj,FL_FRAME_BOX);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,TCPnoneCB,0);
  fdui->tcp_sport = obj = fl_add_input(FL_NORMAL_INPUT,260,70,60,25,"Starting\nPort");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,TCPnoneCB,0);
  fdui->tcp_eport = obj = fl_add_input(FL_NORMAL_INPUT,370,70,60,25,"Ending\nPort");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,TCPnoneCB,0);
  fdui->tcp_scan = obj = fl_add_button(FL_NORMAL_BUTTON,10,70,90,25,"Scan!");
    fl_set_object_lcolor(obj,FL_RED);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,tcpscanCB,0);
  fdui->tcp_portsopen = obj = fl_add_browser(FL_NORMAL_BROWSER,10,120,210,315,"TCP Ports Open");
    fl_set_object_boxtype(obj,FL_FRAME_BOX);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,TCPnoneCB,0);
  fdui->tcp_oportitle = obj = fl_add_text(FL_NORMAL_TEXT,10,100,210,20,"");
    fl_set_object_boxtype(obj,FL_FRAME_BOX);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_FIXEDBOLD_STYLE);
    fl_set_object_callback(obj,TCPnoneCB,0);
  fdui->tcp_sportitle = obj = fl_add_text(FL_NORMAL_TEXT,220,100,210,20,"");
    fl_set_object_boxtype(obj,FL_FRAME_BOX);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_FIXEDBOLD_STYLE);
    fl_set_object_callback(obj,TCPnoneCB,0);
  fdui->tcp_hostname = obj = fl_add_input(FL_NORMAL_INPUT,220,10,215,25,"Host\nName");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,TCPnoneCB,0);
  fdui->tcp_periodic = obj = fl_add_lightbutton(FL_PUSH_BUTTON,115,50,75,20,"Periodic");
    fl_set_object_color(obj,FL_RED,FL_GREEN);
    fl_set_object_callback(obj,TCPnoneCB,0);
  fdui->tcp_busy = obj = fl_add_pixmap(FL_NORMAL_PIXMAP,165,75,20,20,"Scanning");
    fl_set_object_lalign(obj,FL_ALIGN_LEFT);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,TCPnoneCB,0);
  obj = fl_add_button(FL_NORMAL_BUTTON,450,305,90,25,"Trace!");
    fl_set_object_lcolor(obj,FL_RED);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,tcptraceCB,0);
  fdui->tcp_trace = obj = fl_add_browser(FL_NORMAL_BROWSER,450,335,385,115,"");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,TCPnoneCB,0);
  fdui->tcp_busy2 = obj = fl_add_pixmap(FL_NORMAL_PIXMAP,605,310,20,20,"Tracing");
    fl_set_object_lalign(obj,FL_ALIGN_LEFT);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,TCPnoneCB,0);
  obj = fl_add_labelframe(FL_ENGRAVED_FRAME,445,225,400,45,"Ping");
    fl_set_object_lcolor(obj,FL_CYAN);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,TCPnoneCB,0);
  obj = fl_add_button(FL_NORMAL_BUTTON,450,240,90,25,"Ping!");
    fl_set_object_lcolor(obj,FL_RED);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,tcppingCB,0);
  fdui->tcp_busy3 = obj = fl_add_pixmap(FL_NORMAL_PIXMAP,605,245,20,20,"Pinging");
    fl_set_object_lalign(obj,FL_ALIGN_LEFT);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,TCPnoneCB,0);
  fdui->tcp_ping = obj = fl_add_text(FL_NORMAL_TEXT,705,235,125,30,"Success!");
    fl_set_object_boxtype(obj,FL_EMBOSSED_BOX);
    fl_set_object_lsize(obj,FL_MEDIUM_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_FIXEDBOLDITALIC_STYLE+FL_EMBOSSED_STYLE);
    fl_set_object_callback(obj,TCPnoneCB,0);
  obj = fl_add_labelframe(FL_ENGRAVED_FRAME,445,45,400,170,"DNS (Host) Lookup");
    fl_set_object_lcolor(obj,FL_CYAN);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,TCPnoneCB,0);
  obj = fl_add_button(FL_NORMAL_BUTTON,450,65,90,25,"Lookup!");
    fl_set_object_lcolor(obj,FL_RED);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,tcphostCB,0);
  fdui->tcp_busy4 = obj = fl_add_pixmap(FL_NORMAL_PIXMAP,605,70,20,20,"Looking");
    fl_set_object_lalign(obj,FL_ALIGN_LEFT);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,TCPnoneCB,0);
  fdui->tcp_host = obj = fl_add_browser(FL_NORMAL_BROWSER,450,95,385,115,"");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,TCPnoneCB,0);
  fl_end_form();

  fdui->tcpscan->fdui = fdui;

  return fdui;
}
/*---------------------------------------*/

