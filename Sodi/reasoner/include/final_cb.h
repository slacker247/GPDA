#include "forms.h"
#include "final.h"

/* callbacks for form final */

//-----------------cancel call back---------------------------

//ends the program

void cancel_finalCB(FL_OBJECT *ob, long data)
{
   fl_hide_form(fd_final->final);
   fl_finish();
   exit(0);
}

//-----------------accept call back----------------------------

//User accepts the case so the graph form is set up
//to show how the cases compare to each other and
//which one was chosen

void acceptCB(FL_OBJECT *ob, long data)
{
   int color1, color2, color3;
   float highest = 0;
   float second = 100;
   float third = 100;
   int i = 0;          //loop control variable
   char chtemp[128];   //temp array to output characters to X-Form form

/***clears all graphs on graphs form*/
   fl_clear_chart(fd_graphs->graph1);
   fl_clear_chart(fd_graphs->graph2);
   fl_clear_chart(fd_graphs->graph3);

 
/***sets labels to what I want them to say*/

   sprintf(chtemp, "%s", "Total case 
points ordered
as read in from 
case base.  
Higher means
a better route.");

   fl_set_object_label(fd_graphs->text1, chtemp);

   sprintf(chtemp, "%s", "Total points 
that match
user criteria.  
Higher is a 
better match."); 

   fl_set_object_label(fd_graphs->text2, chtemp);
   
   sprintf(chtemp, "%s", "Of the three 
best matching
cases, total 
matching points 
to given
criteria.  
Highest is 
best case.");

   fl_set_object_label(fd_graphs->text3, chtemp);

/*****
      Saves cases:  Writes all original cases to a file
                    and best case too if it is not an exact
                    copy of a previous case
*****/

   COAcases.save_cases();


/***gets points to plot on graphs*/

   point_return plotme;

   plotme = COAcases.plotme();

/*plot points on graph on X-Forms form graphs*/

   while ((plotme.total_points[i] > 0) && (plotme.total_points[i] < 20))
   {       
   
   fl_add_chart_value(fd_graphs->graph1, plotme.total_points[i], " ", 1+i);
 
   if (plotme.total_points[i] > highest)
   {
       highest = plotme.total_points[i];
       color1 = i+1;  
   }

   else if ((plotme.total_points[i] > second) && (plotme.total_points[i] < highest))
   {
      second = plotme.total_points[i];
      color2 = i+1;
   }

   else if((plotme.total_points[i] > third) && (plotme.total_points[i] < second)
       && (plotme.total_points[i] < highest))
   {
      third = plotme.total_points[i];
      color3 = i+1;
   }

 
   fl_add_chart_value(fd_graphs->graph2, plotme.match_points[i], " " , 1+i);


   i++;
   }

   plotme = COAcases.plotmetoo();


         fl_add_chart_value(fd_graphs->graph3, plotme.match_points[0], " ", 
                            color1);

         fl_add_chart_value(fd_graphs->graph3, plotme.match_points[1], " ", 
                            color1);

         fl_add_chart_value(fd_graphs->graph3, plotme.match_points[2], " ", 
                            color2);

   fl_show_form(fd_graphs->graphs, FL_PLACE_CENTER, FL_FULLBORDER, " ");
}

//----------------------start over call back-----------------------

//If the user wants to start over this will clear all forms from the
//screen, sets data to original, and goes back to the first form

void start_overCB(FL_OBJECT *ob, long data)
{
 if (fl_form_is_visible(fd_test->test))
   fl_hide_form(fd_test->test);

   if (fl_form_is_visible(fd_changer->changer))
   fl_hide_form(fd_changer->changer);

   if (fl_form_is_visible(fd_time_changer->time_changer))
   fl_hide_form(fd_time_changer->time_changer);

   if (fl_form_is_visible(fd_terrain_changer->terrain_changer))
   fl_hide_form(fd_terrain_changer->terrain_changer);

   if (fl_form_is_visible(fd_float_changer->float_changer))
   fl_hide_form(fd_float_changer->float_changer);

   if (fl_form_is_visible(fd_char_changer->char_changer))
   fl_hide_form(fd_char_changer->char_changer);

   if (fl_form_is_visible(fd_add->add))
   fl_hide_form(fd_add->add);
   
   if (fl_form_is_visible(fd_final->final))
   fl_hide_form(fd_final->final);


   fl_clear_browser(fd_test->coa1);
   fl_clear_browser(fd_test->coa2);
   fl_clear_browser(fd_test->coa3);
   
   fl_clear_browser(fd_test->coa1info);
   fl_clear_browser(fd_test->coa2info);
   fl_clear_browser(fd_test->coa3info);
   
   fl_show_form(fd_criteria->criteria, FL_PLACE_CENTER, FL_FULLBORDER, "");

}



