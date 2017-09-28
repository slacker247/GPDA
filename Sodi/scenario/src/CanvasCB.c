/*Yet another file of call backs because the other got too long to look at*/

/*This file contains callbacks that deal with interaction between the opengl
  screen and gathering of data from placement of objects
*/

//<------------------------expose-------------------------------------->

//Run when the opengl screen needs to be refreshed 'cause it is "expose"

int exposeCB(FL_OBJECT *ob, Window win, int w, int h, XEvent *xev, void *ud)
{
   draw();
}


//<---------------------button press-------------------------------->

//when the user clicks in the opengl canvas window...the program goes here

int buttonpressCB(FL_OBJECT *ob, Window win, int w, int h, XEvent *xev, void *ud)
{
void ConvertToLatLon(int xval, int yval);
void DefineData();

FL_Coord        width, height;
float           xpos, ypos, closest, d;
float           slat, slon, salt, shead;
int             i, xval, yval;

   xval = xev->xbutton.x;
   yval = xev->xbutton.y;


   fl_get_winsize(win, &width, &height);
   xpos = (-180.0*RADDEG) + ((float)xval/(float)width)*(360.0*RADDEG);
   ypos = (90.0*RADDEG) - ((float)yval/(float)height)*(180.0*RADDEG);
   fprintf(stderr, "User clicked in canvas at %f %f\n", xpos*DEGRAD, ypos*DEGRAD);

   ConvertToLatLon(xval, yval);

   xpos = (xpos*DEGRAD) + 180.0;
   ypos = (ypos*DEGRAD) + 90.0;
   closest = 99999.9;
   for (i=0; i<n_assets; i++) {
     AssetGetLLAH(i, &slat, &slon, &salt, &shead);
     slon = slon + 180.0;
     slat = slat + 90.0;
     d = sqrt( (xpos-slon)*(xpos-slon) + (ypos-slat)*(ypos-slat) );
     if (d < closest) {
       closest = d;
       asset_index = i;
     }
   }
   fprintf(stderr, "We seem to have a match at %s\n", AssetGetName(asset_index));

   if (Clicked == 1)
      DefineData();

}

//someday will convert x and y coordinates to latitude and longintude

void ConvertToLatLon(int xval, int yval)
{


}

//<------------------define data-------------------------------->

//if an asset has been selected and a user clicked on the screen
//this sets up variables and forms and turns the asset selected
//button off

void DefineData()
{
   Clicked = 0;

   Changing.sensor = 0;
   Changing.weapons = 0;
   Changing.coverage = 0;
   Changing.unit = 0;
   Changing.area_defense = 0;

   fl_set_input(fd_select_change->assetname, "");
   fl_set_input(fd_select_change->assetid, "");
 
   fl_set_button(fd_select_change->weapons, 0);
   fl_set_button(fd_select_change->coverage, 0);
   fl_set_button(fd_select_change->sensor, 0);
   fl_set_button(fd_select_change->defense, 0);
   fl_set_button(fd_select_change->unit, 0);

   fl_set_object_color(fd_select_change->colorasset, FL_COL1, FL_COL1);

   fl_set_button(object, 0);

cout << "reading" << endl;
   Read_File_Asset(asset);
cout << "done reading" << endl;
   Set_Change_Screen();
}

