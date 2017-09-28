//#include "forms.h"
//#include "Map.h"

/**** MAP callbacks for form Map ****/

/* Upon click of any asset button, the name
of that asset is saved for search in a file,
Clicked flag is set to allow placement on map,
and the name of the button object is stored
so its button can be turned off after use.
*/


void gbrCB(FL_OBJECT *ob, long data)
{
   fl_set_button(object, 0);

   fl_set_object_label(fd_select_change->instructions,
      "GBR includes only location and asset information");
   strcpy(asset, "GBR");

   Clicked = 1;

   object = fd_Map->gbr;
}

void crcCB(FL_OBJECT *ob, long data)
{
   fl_set_button(object, 0);
   
   fl_set_object_label(fd_select_change->instructions,
      "CRC is treated as a sensor");
   strcpy(asset, "CRC");

   Clicked = 1;
 
   object = fd_Map->crc;
}

void hawkCB(FL_OBJECT *ob, long data)
{
   fl_set_button(object, 0);
   
   fl_set_object_label(fd_select_change->instructions,
      "HAWK includes all below ");
   strcpy(asset, "HAWK");

   Clicked = 1;
 
   object = fd_Map->hawk;
}

void aegisCB(FL_OBJECT *ob, long data)
{
   fl_set_button(object, 0);
   
   fl_set_object_label(fd_select_change->instructions,
      "AEGIS includes all below");
   strcpy(asset, "AEGIS");

   Clicked = 1;
 
   object = fd_Map->aegis;
}

void jtagsCB(FL_OBJECT *ob, long data)
{
   fl_set_button(object, 0);
   
   fl_set_object_label(fd_select_change->instructions,
      "JTAG is treated as a sensor");
   strcpy(asset, "JTAGS");

   Clicked = 1;
 
   object = fd_Map->jtags;
}

void patriotCB(FL_OBJECT *ob, long data)
{
   fl_set_button(object, 0);
   
   fl_set_object_label(fd_select_change->instructions,
      "PATRIOT includes all below");
   strcpy(asset, "PATRIOT");

   Clicked = 1;
 
   object = fd_Map->patriot;
}

void gbiCB(FL_OBJECT *ob, long data)
{
   fl_set_button(object, 0);
   
   fl_set_object_label(fd_select_change->instructions,
      "GBI is a NMD farm");
   strcpy(asset, "GBI");

   Clicked = 1;
 
   object = fd_Map->gbi;
}

void dspCB(FL_OBJECT *ob, long data)
{
   fl_set_button(object, 0);
   
   fl_set_object_label(fd_select_change->instructions,
      "DSP is treated as a moving space based sensor");
   strcpy(asset, "DSP");

   Clicked = 1;
 
   object = fd_Map->dsp;
}

void sbirsCB(FL_OBJECT *ob, long data)
{
   fl_set_button(object, 0);
   
   fl_set_object_label(fd_select_change->instructions,
      "SBIRS is treated as a moving space based sensor");
   strcpy(asset, "SBIRS");

   Clicked = 1;
 
   object = fd_Map->sbirs;
}

void thaadCB(FL_OBJECT *ob, long data)
{
   fl_set_button(object, 0);
   
   fl_set_object_label(fd_select_change->instructions,
      "THAAD includes all below fields");
   strcpy(asset, "THAAD");

   Clicked = 1;
 
   object = fd_Map->thaad;
}

void awacsCB(FL_OBJECT *ob, long data)
{
   fl_set_button(object, 0);
   
   fl_set_object_label(fd_select_change->instructions,
      "AWACS are treated as moving air breathing sensors");
   strcpy(asset, "AWACS");

   Clicked = 1;
 
   object = fd_Map->awacs;
}

void tacticalCB(FL_OBJECT *ob, long data)
{
   fl_set_button(object, 0);
   
   fl_set_object_label(fd_select_change->instructions,
      "Tactical includes location, area defended, and asset information");
   strcpy(asset, "Tactical");

   Clicked = 1;
 
   object = fd_Map->tactical;
}

void cruiseCB(FL_OBJECT *ob, long data)
{
   fl_set_button(object, 0);
   
   //fl_set_object_label(fd_select_change->instructions,
    //  "GBR includes only location and defining information");
   strcpy(asset, "Cruise");

   Clicked = 1;
 
   object = fd_Map->cruise;
}

void armynavyCB(FL_OBJECT *ob, long data)
{
   fl_set_button(object, 0);
   
   fl_set_object_label(fd_select_change->instructions,
      "Army and Navy have location, area defended, and asset information");
   strcpy(asset, "ArmyNavy");

   Clicked = 1;
 
   object = fd_Map->armynavy;
}

void maindoneCB(FL_OBJECT *ob, long data)
{
static int AssetsSaved = FALSE;
char filename[128];

   switch (data) {
      case 0:
        if (AssetsSaved) {
           fl_hide_form(fd_Map->Map);
           exit(0);
	} else {
	   if (fl_show_question("Scenario not saved! Quit anyway?", 0)) {
              fl_hide_form(fd_Map->Map);
              exit(0);
	   }
	}
        break;

      case 1:
        strcpy(filename, fl_show_fselector("Save Scenario As...", "./",
                                     "*.par", "newassets.par"));
        array_place = array_place - 1;
        Save_Data_To_File(filename);
	AssetsSaved = TRUE;
        break;

      case 2:
        strcpy(filename, fl_show_fselector("Load Scenario From...", "./",
                                     "*.par", "assets.par"));
        Load_Data_From_File(filename); 
        break;
   }
}

void evtbrowserCB(FL_OBJECT *ob, long data)
{

}

void canvasCB(FL_OBJECT *ob, long data)
{
Window          win;
FL_Coord        xloc, yloc;
unsigned int    keymask;

   fprintf(stderr, "User clicked in canvas\n");

   win = fl_get_canvas_id(ob);
   fl_get_win_mouse(win, &xloc, &yloc, &keymask);

}


//<--------------------------------------------------------------->

/* SELECT_CHANGE callbacks for form select_change */

/* Handles user input in specified fields
and houses buttons to declare if other
parameters are t or f and then allows
their definition upon selection of true
*/

void assetnameCB(FL_OBJECT *ob, long data)
{
  void Set_Change_Screen();
  const char *get_val;
  int flag = 0;

  get_val =  fl_get_input(fd_select_change->assetname);
  strcpy(Changing.asset_name, get_val);

  if (flag > 0)
  {
     fl_hide_form(fd_select_change->select_change);

     fl_set_input(fd_select_change->assetname, "");

     Set_Change_Screen();

     fl_show_form(fd_select_change->select_change,
                FL_PLACE_CENTER, FL_FULLBORDER, "Location Changes");
  }
}

void assetidCB(FL_OBJECT *ob, long data)
{
  void Set_Change_Screen();
  const char *get_val;
  int flag = 0;

  get_val =  fl_get_input(fd_select_change->assetid);
  Changing.asset_id = atoi(get_val);
  if ((Changing.asset_id < 0) ||
      (Changing.asset_id > 100))
     flag+=1;

  if (flag > 0)

  {
     fl_hide_form(fd_select_change->select_change);

     fl_set_input(fd_select_change->assetid, "");

     Set_Change_Screen();

     fl_show_form(fd_select_change->select_change,
                FL_PLACE_CENTER, FL_FULLBORDER, "Location Changes");
  }
}


void statusCB(FL_OBJECT *ob, long data)
{
  void Set_Change_Screen();
  const char *get_val;
  int flag = 0;

  get_val =  fl_get_input(fd_select_change->status);

  if ((strcmp(get_val, "G") == 0) || (strcmp(get_val, "g") == 0) ||
      (strcmp(get_val, "Y") == 0) || (strcmp(get_val, "y") == 0) ||
      (strcmp(get_val, "R") == 0) || (strcmp(get_val, "r") == 0) ||
      (strcmp(get_val, "") == 0))
      strcpy(Changing.status, get_val);
  else
     flag+=1;

  if (flag > 0)
  {
     fl_hide_form(fd_select_change->select_change);

     strcpy(Changing.status, "");

     Set_Change_Screen();

     fl_show_form(fd_select_change->select_change,
                FL_PLACE_CENTER, FL_FULLBORDER, "Location Changes");
  }
}

void altitudeCB(FL_OBJECT *ob, long data)
{
  void Set_Change_Screen();
  const char *get_val;
  int flag = 0;

  get_val =  fl_get_input(fd_select_change->altitude);
  Changing.altitude = atof(get_val);

  if (flag > 0)
  {
     fl_hide_form(fd_select_change->select_change);

     Changing.altitude = 0.0;

     Set_Change_Screen();

     fl_show_form(fd_select_change->select_change,
                FL_PLACE_CENTER, FL_FULLBORDER, "Location Changes");
  }
}

void iconCB(FL_OBJECT *ob, long data)
{
  void Set_Change_Screen();
  const char *get_val;
  int flag = 0;

  get_val =  fl_get_input(fd_select_change->icon);
  Changing.icon = atoi(get_val);
  if ((Changing.icon < 0) || (Changing.icon > 400))
     flag+=1;

  if (flag > 0)
  {
     fl_hide_form(fd_select_change->select_change);

     Changing.icon = 0;

     Set_Change_Screen();

     fl_show_form(fd_select_change->select_change,
                FL_PLACE_CENTER, FL_FULLBORDER, "Location Changes");
  }
}

void scaleCB(FL_OBJECT *ob, long data)
{
  void Set_Change_Screen();
  const char *get_val;
  int flag = 0;

  get_val =  fl_get_input(fd_select_change->scale);
  Changing.scale = atof(get_val);
  if ((Changing.scale < 0.0) || (Changing.scale > 100))
     flag+=1;

  if (flag > 0)
  {
     fl_hide_form(fd_select_change->select_change);

     Changing.scale = 0.0;

     Set_Change_Screen();

     fl_show_form(fd_select_change->select_change,
                FL_PLACE_CENTER, FL_FULLBORDER, "Location Changes");
  }

}

void locationCB(FL_OBJECT *ob, long data)
{

   char chtemp[15];

   if (Changing.east == 0)
      sprintf(chtemp, "%s", "F");
   else sprintf(chtemp, "%s", "T");
   fl_set_input(fd_location->eastdirection, chtemp);

   if (Changing.north == 0)
      sprintf(chtemp, "%s", "F");
   else sprintf(chtemp, "%s", "T");
   fl_set_input(fd_location->northdirection, chtemp);

   sprintf(chtemp, "%f", Changing.lat_deg);
   fl_set_input(fd_location->deglatchange, chtemp);

   sprintf(chtemp, "%f", Changing.lat_min);
   fl_set_input(fd_location->minlatchange, chtemp);

   sprintf(chtemp, "%f", Changing.lat_sec);
   fl_set_input(fd_location->seclatchange, chtemp);

   sprintf(chtemp, "%f", Changing.lon_deg);
   fl_set_input(fd_location->deglonchange, chtemp);

   sprintf(chtemp, "%f", Changing.lon_min);
   fl_set_input(fd_location->minlonchange, chtemp);

   sprintf(chtemp, "%f", Changing.lon_sec);
   fl_set_input(fd_location->seclonchange, chtemp);

   fl_show_form(fd_location->location,
                FL_PLACE_CENTER, FL_FULLBORDER, "Location Changes");

}

void saveCB(FL_OBJECT *ob, long data)
{
  const char *get_val; 
  void Set_Change_Screen();
  void CheckRepeatedSensors();
   int flag = 0;

  strcpy(ChangingDefense.name, Changing.type);
  strcpy(ChangingSensors.name, Current.sensor_type);

  Current = Changing;
  CurrentSensors = ChangingSensors;
  CurrentDefense = ChangingDefense; 

  if (Current.asset_id > Highest_id)
     Highest_id = Current.asset_id;

  get_val = fl_get_input(fd_select_change->assetname);
  if (strcmp(get_val, "") == 0)
     flag += 1;
  strcpy(Changing.asset_name, get_val);

  get_val = fl_get_input(fd_select_change->assetid);
  if (strcmp(get_val, "") == 0)
     flag += 1;
  Current.asset_id = atoi(get_val);
   if ((Changing.asset_id < 0) ||
      (Changing.asset_id > 100))
   {
     flag+=1;
     fl_set_input(fd_select_change->assetid, "");
   }

  get_val = fl_get_input(fd_select_change->status);
  if (strcmp(get_val, "") == 0)
     flag += 1;
  strcpy(Changing.status, get_val);

   if ((strcmp(get_val, "G") == 0) || (strcmp(get_val, "g") == 0) ||
      (strcmp(get_val, "Y") == 0) || (strcmp(get_val, "y") == 0) ||
      (strcmp(get_val, "R") == 0) || (strcmp(get_val, "r") == 0) ||
      (strcmp(get_val, "") == 0))
      strcpy(Changing.status, get_val);
  else
  {
     flag+=1;
     strcpy(Changing.status, "");
  }

  get_val = fl_get_input(fd_select_change->icon);
  if (strcmp(get_val, "") == 0)
     flag += 1;
  Current.icon = atoi(get_val);
  if ((Changing.icon < 0) || (Changing.icon > 400))
  {
       flag+=1;
       Changing.icon = 0;
  }

  get_val = fl_get_input(fd_select_change->altitude);
  if (strcmp(get_val, "") == 0)
     flag += 1;
  Current.altitude = atof(get_val);

  get_val = fl_get_input(fd_select_change->scale);
  if (strcmp(get_val, "") == 0)
     flag += 1;
  Current.scale = atof(get_val);
  if ((Changing.scale < 0.0) || (Changing.scale > 100))
  {
     flag+=1;
     Changing.scale = 0.0;
  }

  if (flag > 0) 
  {
     fl_hide_form(fd_select_change->select_change);
 
     Set_Change_Screen();
     
     fl_show_form(fd_select_change->select_change,
                FL_PLACE_CENTER, FL_FULLBORDER, "Location Changes");
  }

  else
  {
  Save_Data[array_place] =  Current;

  CheckRepeatedSensors();

  if(Current.area_defense == 1)
  {
     Save_Defense[defense_array] =  CurrentDefense;
     defense_array = defense_array + 1;
  }

  //Save_Data_To_File();

  fl_hide_form(fd_select_change->select_change);

  array_place = array_place + 1;
  }

}

void cancelCB(FL_OBJECT *ob, long data)
{
   fl_hide_form(fd_select_change->select_change);
}

void weaponsCB(FL_OBJECT *ob, long data)
{
   if (fl_get_button(fd_select_change->weapons) == 1)
   {
   Changing.weapons = 1;

   char chtemp[128];

   Changing.gbi_id = 2;
   Changing.n_gbi = 35;
   Changing.n_hold = 5;
   Changing.pkill = 1.0;
   strcpy(Current.gbi_type, "PATRIOT");

   sprintf(chtemp, "%d", Changing.gbi_id);
   fl_set_input(fd_weapons->gbiidchange, chtemp);

   sprintf(chtemp, "%d", Changing.n_gbi);
   fl_set_input(fd_weapons->ngbichange, chtemp);

   sprintf(chtemp, "%d", Changing.n_hold);
   fl_set_input(fd_weapons->nholdchange, chtemp);

   sprintf(chtemp, "%f", Changing.pkill);
   fl_set_input(fd_weapons->pkillchange, chtemp);

   sprintf(chtemp, "%s", Changing.gbi_type);
   fl_set_input(fd_weapons->gbitypechange, chtemp);

   sprintf(chtemp, "%s", Changing.gbi_type);
   fl_set_input(fd_weapons->gbitypechange, chtemp);

   fl_show_form(fd_weapons->weapons,
        FL_PLACE_CENTER, FL_FULLBORDER, "Weapons Changes");
   }

   else 
   {
      Changing.weapons = 0;
   }

}

void sensorCB(FL_OBJECT *ob, long data)
{
   fl_set_input(fd_sensors->sensortypechange, "");

   if (fl_get_button(fd_select_change->sensor) == 1)
   {
      Changing.sensor = 1;
      fl_show_form(fd_selectsensor->selectsensor,
             FL_PLACE_CENTER, FL_FULLBORDER, "Select Sensor");
   }
   else
     Changing.sensor = 0;
}

void defenseCB(FL_OBJECT *ob, long data)
{
   if (fl_get_button(fd_select_change->defense) == 1)
   { 
      fl_set_input(fd_defense->areaname, "");
      Changing.area_defense = 1;
      fl_show_form(fd_selectdefense->selectdefense,
             FL_PLACE_CENTER, FL_FULLBORDER, "Select Defense");
   }
   else
      Changing.area_defense = 0;
}

void unitCB(FL_OBJECT *ob, long data)
{
   if (fl_get_button(fd_select_change->unit) == 1)
   {
      fl_set_input_return(fd_units->unittype, FL_RETURN_END_CHANGED);
      fl_set_input_return(fd_units->unitname, FL_RETURN_END_CHANGED);
      fl_set_input_return(fd_units->type, FL_RETURN_END_CHANGED);
      fl_set_input_return(fd_units->size, FL_RETURN_END_CHANGED);

      fl_addto_choice(fd_units->unittype, "Army");
      fl_addto_choice(fd_units->unittype, "Navy");

      Changing.unit = 1;
      fl_show_form(fd_units->units,
             FL_PLACE_CENTER, FL_FULLBORDER, "Select Defense");
   }
   else
      Changing.unit = 0;
}

void coverageCB(FL_OBJECT *ob, long data)
{
   if (fl_get_button(fd_select_change->coverage) == 1)
      Changing.coverage = 1;
   else 
      Changing.unit = 0;
}


void colorassetCB(FL_OBJECT *ob, long data)
{
  color_call = 1;

  fl_show_form(fd_color->color,
                FL_PLACE_CENTER, FL_FULLBORDER, "Select Color");

}


//<-----------------form select sensor call backs---------------->

/*Select the sensor from a list of choices then call next form*/

void selectsensortypeCB(FL_OBJECT *ob, long data)
{
   void Read_File_Sensor(char *name);

   char chtemp[128];
   int choice = 1;

   char sensor[128]; 

   choice = fl_get_choice(fd_selectsensor->selectsensortype);

   switch (choice)
   {
      case 1: strcpy(Changing.sensor_type, "GBR_SENSOR");
              strcpy(sensor, "GBR_SENSOR");
              break;

      case 2: strcpy(Changing.sensor_type, "GBI_SENSOR");
              strcpy(sensor, "GBI_SENSOR");
              break;

      case 3: strcpy(Changing.sensor_type, "AEGIS_SENSOR");
              strcpy(sensor, "AEGIS_SENSOR");
              break;

      case 4: strcpy(Changing.sensor_type, "PATRIOT_SENSOR");
              strcpy(sensor, "PATRIOT_SENSOR");
              break;

      case 5: strcpy(Changing.sensor_type, "THAAD_SENSOR");
              strcpy(sensor, "THAAD_SENSOR");
              break;

      case 6: strcpy(Changing.sensor_type, "HAWK_SENSOR");
              strcpy(sensor, "HAWK_SENSOR");
              break;

      case 7: strcpy(Changing.sensor_type, "DSP_SENSOR");
              strcpy(sensor, "DSP_SENSOR");
              break;

      case 8: strcpy(Changing.sensor_type, "SBIRS_SENSOR");
              strcpy(sensor, "SBIRS_SENSOR");
              break;
   };

   Read_File_Sensor(sensor);

   sprintf(chtemp, "%f", ChangingSensors.scan_time);
   fl_set_input(fd_sensors->scantimechange, chtemp);

   sprintf(chtemp, "%f", ChangingSensors.rmin);
   fl_set_input(fd_sensors->rminchange, chtemp);

   sprintf(chtemp, "%f", ChangingSensors.rmax);
   fl_set_input(fd_sensors->rmaxchange, chtemp);

   sprintf(chtemp, "%f", ChangingSensors.rmax_low);
   fl_set_input(fd_sensors->rmaxlowchange, chtemp);

   sprintf(chtemp, "%f", ChangingSensors.rdotmin);
   fl_set_input(fd_sensors->rdotminchange, chtemp);

   sprintf(chtemp, "%f", ChangingSensors.signal);
   fl_set_input(fd_sensors->signalchange, chtemp);

   sprintf(chtemp, "%f", ChangingSensors.luminosity);
   fl_set_input(fd_sensors->luminositychange, chtemp);

   sprintf(chtemp, "%f", Changing.elevation);
   fl_set_input(fd_sensors->elevationchange, chtemp);

   sprintf(chtemp, "%f", Changing.azimuth);
   fl_set_input(fd_sensors->azimuthchange, chtemp);

   sprintf(chtemp, "%f", ChangingSensors.fov_high);
   fl_set_input(fd_sensors->fovhighchange, chtemp);

   sprintf(chtemp, "%f", ChangingSensors.fov_low);
   fl_set_input(fd_sensors->fovlowchange, chtemp);

   if(ChangingSensors.fixed == 0)
      sprintf(chtemp, "%s", "F");
   else sprintf(chtemp, "%s", "T");
      fl_set_input(fd_sensors->fixedchange, chtemp);

   if(ChangingSensors.los == 0)
      sprintf(chtemp, "%s", "F");
   else sprintf(chtemp, "%s", "T");
      fl_set_input(fd_sensors->loschange, chtemp);

   sprintf(chtemp, "%f", ChangingSensors.error);
   fl_set_input(fd_sensors->errorchange, chtemp);

   sprintf(chtemp, "%d", ChangingSensors.icon);
   fl_set_input(fd_sensors->iconchange, chtemp);

   sprintf(chtemp, "%f", ChangingSensors.scale);
   fl_set_input(fd_sensors->scalechange, chtemp);

   fl_show_form(fd_sensors->sensors,
             FL_PLACE_CENTER, FL_FULLBORDER, "Sensor Changes");

   fl_hide_form(fd_selectsensor->selectsensor);
}


//<-----------------Select Defense Type--------------------------->

/*Select type of area defense from menu choices and move to
definition screen*/

void selectdefensetypeCB(FL_OBJECT *ob, long data)
{
   void Read_File_Defense(char *name);

   int choice;
   char chtemp[128];

   char defense[128];

   choice = fl_get_choice(fd_selectdefense->selectdefensetype);

   switch(choice)
   {
      case 1: strcpy(Changing.type, "AREA_1");
              strcpy(defense, "AREA_1");
              break;
      case 2: strcpy(Changing.type, "AREA_2");
              strcpy(defense, "AREA_2");
              break;
   };

      Read_File_Defense(defense);

      if(ChangingDefense.area_label == 0)
      sprintf(chtemp, "%s", "F");
      else sprintf(chtemp, "%s", "T");
      fl_set_input(fd_defense->arealabel, chtemp);

      if(ChangingDefense.area_north == 0)
      sprintf(chtemp, "%s", "F");
      else sprintf(chtemp, "%s", "T");
      fl_set_input(fd_defense->areanorth, chtemp);

      if(ChangingDefense.area_east == 0)
      sprintf(chtemp, "%s", "F");
      else sprintf(chtemp, "%s", "T");
      fl_set_input(fd_defense->areaeast, chtemp);

      sprintf(chtemp, "%f", ChangingDefense.area_lat_deg);
      fl_set_input(fd_defense->arealatdeg, chtemp);

      sprintf(chtemp, "%f", ChangingDefense.area_lat_min);
      fl_set_input(fd_defense->arealatmin, chtemp);

      sprintf(chtemp, "%f", ChangingDefense.area_lat_sec);
      fl_set_input(fd_defense->arealatsec, chtemp);

      sprintf(chtemp, "%f", ChangingDefense.area_lon_deg);
      fl_set_input(fd_defense->arealondeg, chtemp);

      sprintf(chtemp, "%f", ChangingDefense.area_lon_min);
      fl_set_input(fd_defense->arealonmin, chtemp);

      sprintf(chtemp, "%f", ChangingDefense.area_lon_sec);
      fl_set_input(fd_defense->arealonsec, chtemp);

      sprintf(chtemp, "%f", ChangingDefense.area_major);
      fl_set_input(fd_defense->areamajor, chtemp);

      sprintf(chtemp, "%f", ChangingDefense.area_minor);
      fl_set_input(fd_defense->areaminor, chtemp);

      sprintf(chtemp, "%f", ChangingDefense.area_orient);
      fl_set_input(fd_defense->areaorient, chtemp);

      sprintf(chtemp, "%s", ChangingDefense.area_file);
      fl_set_input(fd_defense->areafile, chtemp);

      fl_show_form(fd_defense->defense,
                FL_PLACE_CENTER, FL_FULLBORDER, "Defense Changes");

      fl_hide_form(fd_selectdefense->selectdefense);
}

//<-------------------Location form call backs-------------------->

/* Various input fields that hold location data of the asset
specified by user input and placement on the opengl screen
*/

/* LOCATION callbacks for form location */

void northdirectionCB(FL_OBJECT *ob, long data)
{
   fl_set_input_return(fd_location->donelocation, FL_RETURN_END_CHANGED);
}

void eastdirectionCB(FL_OBJECT *ob, long data)
{
   fl_set_input_return(fd_location->donelocation, FL_RETURN_END_CHANGED);
}

void deglatchangeCB(FL_OBJECT *ob, long data)
{
   fl_set_input_return(fd_location->donelocation, FL_RETURN_END_CHANGED);
}

void minlatchangeCB(FL_OBJECT *ob, long data)
{
   fl_set_input_return(fd_location->donelocation, FL_RETURN_END_CHANGED);
}

void seclatchangeCB(FL_OBJECT *ob, long data)
{
   fl_set_input_return(fd_location->donelocation, FL_RETURN_END_CHANGED);
}

void seclonchangeCB(FL_OBJECT *ob, long data)
{
   fl_set_input_return(fd_location->donelocation, FL_RETURN_END_CHANGED);
}

void deglonchangeCB(FL_OBJECT *ob, long data)
{
   fl_set_input_return(fd_location->donelocation, FL_RETURN_END_CHANGED);
}

void minlonchangeCB(FL_OBJECT *ob, long data)
{
   fl_set_input_return(fd_location->donelocation, FL_RETURN_END_CHANGED);
}

void donelocationCB(FL_OBJECT *ob, long data)
{
  void Set_Change_Screen();

   FL_OBJECT *form;
   const char *get_new_value;
   char chtemp[15];
   float newdegree;
   int flag = 0;

//north change
   get_new_value = fl_get_input(fd_location->northdirection);

   if ((strcmp(get_new_value, "T") == 0) || (strcmp(get_new_value, "t") == 0))
   {
      Changing.north = 1;
   }
   else if ((strcmp(get_new_value, "F") == 0) || (strcmp(get_new_value, "f") == 0))
   {
      Changing.north = 0;
   }

   else
     flag+=1;

//east change
   get_new_value = fl_get_input(fd_location->eastdirection);

   if ((strcmp(get_new_value, "T") == 0) || (strcmp(get_new_value, "t") == 0))
   {
      Changing.east =  1;
   }
   else if ((strcmp(get_new_value, "F") == 0) || (strcmp(get_new_value, "f") == 0))
   {
      Changing.east = 0;
   }

   else
     flag+=1;



//lat deg change
   get_new_value = fl_get_input(fd_location->deglatchange);
   newdegree = atof(get_new_value);

   if ((newdegree >= -360) && (newdegree <= 360))
   {
      Changing.lat_deg = newdegree;
   }
   else
   {
      Changing.lat_deg = Current.lat_deg;
      flag += 1;
   }

//lat min change
   get_new_value = fl_get_input(fd_location->minlatchange);
   newdegree = atof(get_new_value);

   if ((newdegree >= -360) && (newdegree <= 360))
   {
      Changing.lat_min = newdegree;
   }
   else
   {
      Changing.lat_min = Current.lat_min;
      flag += 1;
   }

//lat sec change
   get_new_value = fl_get_input(fd_location->seclatchange);
   newdegree = atof(get_new_value);

   if ((newdegree >= -360) && (newdegree <= 360))
   {
      Changing.lat_sec = newdegree;
   }
   else
   {
      Changing.lat_sec = Current.lat_sec;
      flag += 1;
   }

//long deg change
   get_new_value = fl_get_input(fd_location->deglonchange);
   newdegree = atof(get_new_value);

   if ((newdegree >= -360) && (newdegree <= 360))
   {
      Changing.lon_deg = newdegree;
   }
   else
   {
      Changing.lon_deg = Current.lon_deg;
      flag += 1;
   }

//long min change
   get_new_value = fl_get_input(fd_location->minlonchange);
   newdegree = atof(get_new_value);

   if ((newdegree >= -360) && (newdegree <= 360))
   {
      Changing.lon_min = newdegree;
   }
   else
   {
      Changing.lon_min = Current.lon_min;
      flag += 1;
   }

//long sec change
   get_new_value = fl_get_input(fd_location->seclonchange);
   newdegree = atof(get_new_value);

   if ((newdegree >= -360) && (newdegree <= 360))
   {
      Changing.lon_sec = newdegree;
   }
   else
   {
      Changing.lon_sec = Current.lon_sec;
      flag += 1;
   }

//if user messed up, redo
   if (flag > 0)
   {   

      fl_hide_form(fd_location->location);

      sprintf(chtemp, "%f", Current.north);
      fl_set_input(fd_location->northdirection, chtemp);

      sprintf(chtemp, "%f", Current.east);
      fl_set_input(fd_location->eastdirection, chtemp);

      sprintf(chtemp, "%f", Current.lat_deg);
      fl_set_input(fd_location->deglatchange, chtemp);

      sprintf(chtemp, "%f", Current.lat_min);
      fl_set_input(fd_location->minlatchange, chtemp);
      sprintf(chtemp, "%f", Current.lat_sec);
      fl_set_input(fd_location->seclatchange, chtemp);

      sprintf(chtemp, "%f", Current.lon_deg);
      fl_set_input(fd_location->deglonchange, chtemp);

      sprintf(chtemp, "%f", Current.lon_min);
      fl_set_input(fd_location->minlonchange, chtemp);

      sprintf(chtemp, "%f", Current.lon_sec);
      fl_set_input(fd_location->seclonchange, chtemp);

      fl_show_form(fd_location->location,
                   FL_PLACE_CENTER, FL_FULLBORDER, "Location Changes");


   }

//if all okay, move on
   else
   {
      fl_hide_form(fd_location->location);
      Set_Change_Screen();
   }

}

//<------------------Weapons form call backs------------------------->

/* For now weapons forms hold "defined" weapons parameters
read from file or assigned by variabls...can be changed by
user*/


/* WEAPONS callbacks for form weapons */

void nholdchangeCB(FL_OBJECT *ob, long data)
{
   fl_set_input_return(fd_weapons->weaponsdone, FL_RETURN_END_CHANGED);
}

void pkillchangeCB(FL_OBJECT *ob, long data)
{
   fl_set_input_return(fd_weapons->weaponsdone, FL_RETURN_END_CHANGED);
}

void ngbichangeCB(FL_OBJECT *ob, long data)
{
   fl_set_input_return(fd_weapons->weaponsdone, FL_RETURN_END_CHANGED);
}

void gbiidchangeCB(FL_OBJECT *ob, long data)
{
   fl_set_input_return(fd_weapons->weaponsdone, FL_RETURN_END_CHANGED);
}

void gbitypechangeCB(FL_OBJECT *ob, long data)
{
   fl_set_input_return(fd_weapons->weaponsdone, FL_RETURN_END_CHANGED);
}

void weaponsdoneCB(FL_OBJECT *ob, long data)
{
   void Set_Change_Screen();

   const char *get_new_value;
   char chtemp[128];
   int newvalue;
   float othernew;


   get_new_value = fl_get_input(fd_weapons->gbiidchange);
   newvalue = atoi(get_new_value);
   Changing.gbi_id = newvalue;

   get_new_value = fl_get_input(fd_weapons->ngbichange);
   newvalue = atoi(get_new_value);
   Changing.n_gbi = newvalue;

   get_new_value = fl_get_input(fd_weapons->nholdchange);
   newvalue = atoi(get_new_value);
   Changing.n_hold = newvalue;

   get_new_value = fl_get_input(fd_weapons->pkillchange);
   othernew = atof(get_new_value);
   Changing.pkill = newvalue;

   get_new_value = fl_get_input(fd_weapons->gbitypechange);
   strcpy(Changing.gbi_type, get_new_value);

   fl_hide_form(fd_weapons->weapons);

   Set_Change_Screen();

/*
//redo the form if user entered wrong data

   else
   {
      fl_hide_form(fd_weapons->weapons);

      sprintf(chtemp, "%d", Current.gbi_id);
      fl_set_input(fd_weapons->gbiidchange, chtemp);

      sprintf(chtemp, "%d", Current.n_gbi);
      fl_set_input(fd_weapons->ngbichange, chtemp);

      sprintf(chtemp, "%d", Current.n_hold);
      fl_set_input(fd_weapons->nholdchange, chtemp);

      sprintf(chtemp, "%f", Current.pkill);
      fl_set_input(fd_weapons->pkillchange, chtemp);

      sprintf(chtemp, "%s", Current.gbi_type);
      fl_set_input(fd_weapons->gbitypechange, chtemp);

      fl_show_form(fd_weapons->weapons,
                   FL_PLACE_CENTER, FL_FULLBORDER, "Weapons Changes");

   }
*/
}


//<----------------Defense form call backs-------------------------->

/* DEFENSE callbacks for form defense */

/* Form that has several input fields allowing user changes
in defense parameters*/

void donedefenseCB(FL_OBJECT *ob, long data)
{
   const char *get_new_value;

   get_new_value = fl_get_input(fd_defense->arealabel);
   ChangingDefense.area_label = atoi(get_new_value);

   get_new_value = fl_get_input(fd_defense->areanorth);
   ChangingDefense.area_north = atoi(get_new_value);

   get_new_value = fl_get_input(fd_defense->areaeast);
   ChangingDefense.area_east = atoi(get_new_value);

   get_new_value = fl_get_input(fd_defense->arealatdeg);
   ChangingDefense.area_lat_deg = atof(get_new_value);

   get_new_value = fl_get_input(fd_defense->arealatmin);
   ChangingDefense.area_lat_min = atof(get_new_value);

   get_new_value = fl_get_input(fd_defense->arealatsec);
   ChangingDefense.area_lat_sec = atof(get_new_value);

   get_new_value = fl_get_input(fd_defense->arealondeg);
   ChangingDefense.area_lon_deg = atof(get_new_value);

   get_new_value = fl_get_input(fd_defense->arealonmin);
   ChangingDefense.area_lon_min = atof(get_new_value);

   get_new_value = fl_get_input(fd_defense->arealonsec);
   ChangingDefense.area_lon_sec = atof(get_new_value);

   get_new_value = fl_get_input(fd_defense->areamajor);
   ChangingDefense.area_major = atof(get_new_value);

   get_new_value = fl_get_input(fd_defense->areaminor);
   ChangingDefense.area_minor = atof(get_new_value);

   get_new_value = fl_get_input(fd_defense->areaorient);
   ChangingDefense.area_orient = atof(get_new_value);

   get_new_value = fl_get_input(fd_defense->areafile);
   strcpy(ChangingDefense.area_file, get_new_value); 
   
   get_new_value = fl_get_input(fd_defense->areaname);
   strcpy(Changing.type, get_new_value); 
   
   fl_hide_form(fd_defense->defense);
}

void areanameCB(FL_OBJECT *ob, long data)
{
   fl_set_input_return(fd_defense->donedefense, FL_RETURN_END_CHANGED);
}

void arealabelCB(FL_OBJECT *ob, long data)
{
   fl_set_input_return(fd_defense->donedefense, FL_RETURN_END_CHANGED);
}

void areanorthCB(FL_OBJECT *ob, long data)
{
   fl_set_input_return(fd_defense->donedefense, FL_RETURN_END_CHANGED);
}

void areaeastCB(FL_OBJECT *ob, long data)
{
   fl_set_input_return(fd_defense->donedefense, FL_RETURN_END_CHANGED);
}

void arealatdegCB(FL_OBJECT *ob, long data)
{
   fl_set_input_return(fd_defense->donedefense, FL_RETURN_END_CHANGED);
}

void arealatminCB(FL_OBJECT *ob, long data)
{
   fl_set_input_return(fd_defense->donedefense, FL_RETURN_END_CHANGED);
}

void arealatsecCB(FL_OBJECT *ob, long data)
{
   fl_set_input_return(fd_defense->donedefense, FL_RETURN_END_CHANGED);
}

void arealongdegCB(FL_OBJECT *ob, long data)
{
   fl_set_input_return(fd_defense->donedefense, FL_RETURN_END_CHANGED);
}

void arealonminCB(FL_OBJECT *ob, long data)
{
   fl_set_input_return(fd_defense->donedefense, FL_RETURN_END_CHANGED);
}

void arealonsecCB(FL_OBJECT *ob, long data)
{
   fl_set_input_return(fd_defense->donedefense, FL_RETURN_END_CHANGED);
}

void areatypeCB(FL_OBJECT *ob, long data)
{
   fl_set_input_return(fd_defense->donedefense, FL_RETURN_END_CHANGED);
}

void areamajorCB(FL_OBJECT *ob, long data)
{
   fl_set_input_return(fd_defense->donedefense, FL_RETURN_END_CHANGED);
}

void areaminorCB(FL_OBJECT *ob, long data)
{
   fl_set_input_return(fd_defense->donedefense, FL_RETURN_END_CHANGED);
}

void areaorientCB(FL_OBJECT *ob, long data)
{
   fl_set_input_return(fd_defense->donedefense, FL_RETURN_END_CHANGED);
}

void areafileCB(FL_OBJECT *ob, long data)
{
   fl_set_input_return(fd_defense->donedefense, FL_RETURN_END_CHANGED);
}

void colorCB(FL_OBJECT *ob, long data)
{
  color_call = 0;

  fl_show_form(fd_color->color,
                FL_PLACE_CENTER, FL_FULLBORDER, "Select Color");

}

//<---------------Sensor Call Backs------------------------->

/* SENSORS callbacks for form sensors */

/* Form with input fields for sensor parameters.  Origianl
values taken from "readfile", user changes are
allowed*/

void sensortypechangeCB(FL_OBJECT *ob, long data)
{
   fl_set_input_return(fd_sensors->donesensor, FL_RETURN_END_CHANGED);
}

void scantimechangeCB(FL_OBJECT *ob, long data)
{
   fl_set_input_return(fd_sensors->donesensor, FL_RETURN_END_CHANGED);
}

void rminchangeCB(FL_OBJECT *ob, long data)
{
   fl_set_input_return(fd_sensors->donesensor, FL_RETURN_END_CHANGED);
}

void rmaxchangeCB(FL_OBJECT *ob, long data)
{
   fl_set_input_return(fd_sensors->donesensor, FL_RETURN_END_CHANGED);
}

void rmaxlowchangeCB(FL_OBJECT *ob, long data)
{
   fl_set_input_return(fd_sensors->donesensor, FL_RETURN_END_CHANGED);
}

void rdotminchangeCB(FL_OBJECT *ob, long data)
{
   fl_set_input_return(fd_sensors->donesensor, FL_RETURN_END_CHANGED);
}

void signalchangeCB(FL_OBJECT *ob, long data)
{
   fl_set_input_return(fd_sensors->donesensor, FL_RETURN_END_CHANGED);
}

void luminositychangeCB(FL_OBJECT *ob, long data)
{
   fl_set_input_return(fd_sensors->donesensor, FL_RETURN_END_CHANGED);
}

void elevationchangeCB(FL_OBJECT *ob, long data)
{
   fl_set_input_return(fd_sensors->donesensor, FL_RETURN_END_CHANGED);
}

void azimuthchangeCB(FL_OBJECT *ob, long data)
{
   fl_set_input_return(fd_sensors->donesensor, FL_RETURN_END_CHANGED);
}

void fovhighchange(FL_OBJECT *ob, long data)
{
   fl_set_input_return(fd_sensors->donesensor, FL_RETURN_END_CHANGED);
}

void fovlowchangeCB(FL_OBJECT *ob, long data)
{
   fl_set_input_return(fd_sensors->donesensor, FL_RETURN_END_CHANGED);
}

void fixedchangeCB(FL_OBJECT *ob, long data)
{
   fl_set_input_return(fd_sensors->donesensor, FL_RETURN_END_CHANGED);
}

void loschangeCB(FL_OBJECT *ob, long data)
{
   fl_set_input_return(fd_sensors->donesensor, FL_RETURN_END_CHANGED);
}

void errorchangeCB(FL_OBJECT *ob, long data)
{
   fl_set_input_return(fd_sensors->donesensor, FL_RETURN_END_CHANGED);
}

void iconchangeCB(FL_OBJECT *ob, long data)
{
   fl_set_input_return(fd_sensors->donesensor, FL_RETURN_END_CHANGED);
}

void scalechangeCB(FL_OBJECT *ob, long data)
{
   fl_set_input_return(fd_sensors->donesensor, FL_RETURN_END_CHANGED);
}

void donesensorCB(FL_OBJECT *ob, long data)
{
   void Set_Change_Screen();

   const char *get_new_value;

   get_new_value = fl_get_input(fd_sensors->sensortypechange);
   ChangingSensors.sensor_type = atoi(get_new_value);

   get_new_value = fl_get_input(fd_sensors->scantimechange);
   ChangingSensors.scan_time = atof(get_new_value);

   get_new_value = fl_get_input(fd_sensors->rminchange);
   ChangingSensors.rmin = atof(get_new_value);

   get_new_value = fl_get_input(fd_sensors->rmaxchange);
   ChangingSensors.rmax = atof(get_new_value);

   get_new_value = fl_get_input(fd_sensors->rmaxlowchange);
   ChangingSensors.rmax_low = atof(get_new_value);

   get_new_value = fl_get_input(fd_sensors->rdotminchange);
   ChangingSensors.rdotmin = atof(get_new_value);

   get_new_value = fl_get_input(fd_sensors->signalchange);
   ChangingSensors.signal = atof(get_new_value);

   get_new_value = fl_get_input(fd_sensors->luminositychange);
   ChangingSensors.luminosity = atof(get_new_value);

//stored in Asset_Vars struct not Sensor_Vars struct

   get_new_value = fl_get_input(fd_sensors->azimuthchange);
   Changing.azimuth = atof(get_new_value);

   get_new_value = fl_get_input(fd_sensors->elevationchange);
   Changing.elevation = atof(get_new_value);

//end of values store in other structure

   get_new_value = fl_get_input(fd_sensors->fovhighchange);
   ChangingSensors.fov_high = atof(get_new_value);

   get_new_value = fl_get_input(fd_sensors->fovlowchange);
   ChangingSensors.fov_low = atof(get_new_value);

   get_new_value = fl_get_input(fd_sensors->fixedchange);
   ChangingSensors.fixed = atoi(get_new_value);

   get_new_value = fl_get_input(fd_sensors->loschange);
   ChangingSensors.los = atoi(get_new_value);

   get_new_value = fl_get_input(fd_sensors->errorchange);
   ChangingSensors.error = atof(get_new_value);

   get_new_value = fl_get_input(fd_sensors->iconchange);
   ChangingSensors.icon = atoi(get_new_value);

   get_new_value = fl_get_input(fd_sensors->scalechange);
   ChangingSensors.scale = atof(get_new_value);


   fl_hide_form(fd_sensors->sensors);
}

//<------------------Color Button info----------------------->

/* COLOR callbacks for form color */

//Call backs for selection of a particular color button on
//color form.  Defines r, g, and b with the appropriate
//value for the appropriate form.

void blackCB(FL_OBJECT *ob, long data)
{
   FL_OBJECT *obj;

   if (color_call == 1)
   {
      Changing.r = 0;
      Changing.g = 0;
      Changing.b = 0;

      obj = fd_select_change->colorasset;
   }

   else
   {
      ChangingDefense.r = 0;
      ChangingDefense.g = 0;
      ChangingDefense.b = 0;
      
      obj = fd_defense->color;
   }   

   fl_set_object_color(obj,FL_BLACK,FL_COL1);
   fl_hide_form(fd_color->color);
}

void redCB(FL_OBJECT *ob, long data)
{
   FL_OBJECT *obj;
   if (color_call == 1)
   {
      Changing.r = 255;
      Changing.g = 0;
      Changing.b = 0;

      obj = fd_select_change->colorasset;
   }

   else
   {
      ChangingDefense.r = 255;
      ChangingDefense.g = 0;
      ChangingDefense.b = 0;
      
      obj = fd_defense->color;
   }   
   
   fl_set_object_color(obj,FL_RED,FL_COL1);
   fl_hide_form(fd_color->color);
}

void greenCB(FL_OBJECT *ob, long data)
{
   FL_OBJECT *obj;
   if (color_call == 1)
   {
      Changing.r = 0;
      Changing.g = 255;
      Changing.b = 0;

      obj = fd_select_change->colorasset;
   }

   else
   {
      ChangingDefense.r = 0;
      ChangingDefense.g = 255;
      ChangingDefense.b = 0;
      
      obj = fd_defense->color;
   }   
   
   fl_set_object_color(obj,FL_GREEN,FL_COL1);
   fl_hide_form(fd_color->color);
}

void yellowCB(FL_OBJECT *ob, long data)
{
   FL_OBJECT *obj;
   if (color_call == 1)
   {
      Changing.r = 255;
      Changing.g = 255;
      Changing.b = 0;

      obj = fd_select_change->colorasset;
   }

   else
   {
      ChangingDefense.r = 255;
      ChangingDefense.g = 255;
      ChangingDefense.b = 0;
      
      obj = fd_defense->color;
   }   
   
   fl_set_object_color(obj,FL_YELLOW,FL_COL1);
   fl_hide_form(fd_color->color);
}

void blueCB(FL_OBJECT *ob, long data)
{
   FL_OBJECT *obj;
   if (color_call == 1)
   {
      Changing.r = 0;
      Changing.g = 0;
      Changing.b = 255;

      obj = fd_select_change->colorasset;
   }

   else
   {
      ChangingDefense.r = 0;
      ChangingDefense.g = 0;
      ChangingDefense.b = 255;
      
      obj = fd_defense->color;
   }   
   
   fl_set_object_color(obj,FL_BLUE,FL_COL1);
   fl_hide_form(fd_color->color);
}

void pinkCB(FL_OBJECT *ob, long data)
{
   FL_OBJECT *obj;
   if (color_call == 1)
   {
      Changing.r = 255;
      Changing.g = 20;
      Changing.b = 147;

      obj = fd_select_change->colorasset;
   }

   else
   {
      ChangingDefense.r = 255;
      ChangingDefense.g = 20;
      ChangingDefense.b = 147;
      
      obj = fd_defense->color;
   }   
   
   fl_set_object_color(obj,FL_MAGENTA,FL_COL1);
   fl_hide_form(fd_color->color);
}

void lightblueCB(FL_OBJECT *ob, long data)
{
   FL_OBJECT *obj;
   if (color_call == 1)
   {
      Changing.r = 0;
      Changing.g = 245;
      Changing.b = 255;

      obj = fd_select_change->colorasset;
   }

   else
   {
      ChangingDefense.r = 0;
      ChangingDefense.g = 245;
      ChangingDefense.b = 255;
      
      obj = fd_defense->color;
   }   
   
   fl_set_object_color(obj,FL_CYAN,FL_COL1);
   fl_hide_form(fd_color->color);
}

void whiteCB(FL_OBJECT *ob, long data)
{
   FL_OBJECT *obj;
   if (color_call == 1)
   {
      Changing.r = 255;
      Changing.g = 255;
      Changing.b = 255;

      obj = fd_select_change->colorasset;
   }

   else
   {
      ChangingDefense.r = 255;
      ChangingDefense.g = 255;
      ChangingDefense.b = 255;
      
      obj = fd_defense->color;
   }   
   
   fl_set_object_color(obj,FL_WHITE,FL_COL1);
   fl_hide_form(fd_color->color);
}

void orangeCB(FL_OBJECT *ob, long data)
{
   FL_OBJECT *obj;
   if (color_call == 1)
   {
      Changing.r = 255;
      Changing.g = 69;
      Changing.b = 0;

      obj = fd_select_change->colorasset;
   }

   else
   {
      ChangingDefense.r = 255;
      ChangingDefense.g = 69;
      ChangingDefense.b = 0;
      
      obj = fd_defense->color;
   }   
   
   fl_set_object_color(obj,FL_DARKORANGE,FL_COL1);
   fl_hide_form(fd_color->color);
}

void purpleCB(FL_OBJECT *ob, long data)
{
   FL_OBJECT *obj;
   if (color_call == 1)
   {
      Changing.r = 148;
      Changing.g = 0;
      Changing.b = 211;

      obj = fd_select_change->colorasset;
   }

   else
   {
      Changing.r = 148;
      Changing.g = 0;
      Changing.b = 211;
      
      obj = fd_defense->color;
   }   
   
   fl_set_object_color(obj,FL_DARKVIOLET,FL_COL1);
   fl_hide_form(fd_color->color);
}

void greyCB(FL_OBJECT *ob, long data)
{
   FL_OBJECT *obj;
   if (color_call == 1)
   {
      Changing.r = 99;
      Changing.g = 99;
      Changing.b = 99;

      obj = fd_select_change->colorasset;
   }

   else
   {
      ChangingDefense.r = 99;
      ChangingDefense.g = 99;
      ChangingDefense.b = 99;
      
      obj = fd_defense->color;
   }   
   
   fl_set_object_color(obj,FL_BOTTOM_BCOL,FL_COL1);
   fl_hide_form(fd_color->color);
}

void brownCB(FL_OBJECT *ob, long data)
{
   FL_OBJECT *obj;
   if (color_call == 1)
   {
      Changing.r = 165;
      Changing.g = 42;
      Changing.b = 42;

      obj = fd_select_change->colorasset;
   }

   else
   {
      ChangingDefense.r = 165;
      ChangingDefense.g = 42; 
      ChangingDefense.b = 42;
      
      obj = fd_defense->color;
   }   
   
   fl_set_object_color(obj,FL_DARKTOMATO,FL_COL1);
   fl_hide_form(fd_color->color);
}

//<------------------ Unit form call backs-------------------->

//sets up user entered parameters for unit information

void unittypeCB(FL_OBJECT *ob, long data)
{
   int flag = 0;
   int choice = 0;
   const char *get_val;

   choice = fl_get_choice(fd_units->unittype);
   
   switch (choice)
   {
      case 1: strcpy(Changing.unit_type, "Army");
              break;
      case 2: strcpy(Changing.unit_type, "Navy");
              break;
   };

}

void unitnameCB(FL_OBJECT *ob, long data)
{
   int flag = 0;
   const char *get_val;

   get_val = fl_get_input(fd_units->unitname);
   if (strcmp(get_val, "") == 0)
      flag+=1;

   strcpy(Changing.unit_name, get_val);

  if (flag > 0)
  {
     fl_hide_form(fd_units->units);

     fl_set_input(fd_units->unitname, "");

     fl_show_form(fd_units->units,
                FL_PLACE_CENTER, FL_FULLBORDER, "Unit Changes");
  }
  

}

void typeCB(FL_OBJECT *ob, long data)
{
   int flag = 0;
   const char *get_val;

   get_val = fl_get_input(fd_units->type);   
   if (strcmp(get_val, "") == 0)
      flag+=1;

   Changing.unit_int_type = atoi(get_val);

  if (flag > 0)
  {
     fl_hide_form(fd_units->units);

     fl_set_input(fd_units->type, "");

     fl_show_form(fd_units->units,
                FL_PLACE_CENTER, FL_FULLBORDER, "Unit Changes");
  }
  

}

void sizeCB(FL_OBJECT *ob, long data)
{
   int flag = 0;
   const char *get_val;

   get_val = fl_get_input(fd_units->size);   
   if (strcmp(get_val, "") == 0)
      flag+=1;

   Changing.size = atof(get_val);

  if (flag > 0)
  {
     fl_hide_form(fd_units->units);

     fl_set_input(fd_units->size, "");


     fl_show_form(fd_units->units,
                FL_PLACE_CENTER, FL_FULLBORDER, "Unit Changes");
  }
  

}

void unitdoneCB(FL_OBJECT *ob, long data)
{

   int flag = 0;
   const char *get_val;

   get_val = fl_get_input(fd_units->type);   
   if (strcmp(get_val, "") == 0)
      flag+=1;
   else Changing.unit_int_type = atoi(get_val);
   get_val = fl_get_input(fd_units->unitname);   
   if (strcmp(get_val, "") == 0)
      flag+=1;
   else strcpy(Changing.unit_name, get_val);
   get_val = fl_get_input(fd_units->size);   
   if (strcmp(get_val, "") == 0)
      flag+=1;
   else Changing.size = atof(get_val);

  if (flag > 0)
  {
     fl_hide_form(fd_units->units);
     fl_show_form(fd_units->units,
                FL_PLACE_CENTER, FL_FULLBORDER, "Unit Changes");
  }
   
   else
      fl_hide_form(fd_units->units);

}


//<------------------ Set Change Screen ----------------------->

//SET_CHANGE

/* Places most current information about the asset being created
   on the select_form screen every time it is updated (returned to)
*/

void Set_Change_Screen()
{
   char chtemp[128];

   sprintf(chtemp, "%d", Changing.icon);
   fl_set_input(fd_select_change->icon, chtemp);

   sprintf(chtemp, "%f", Changing.scale);
   fl_set_input(fd_select_change->scale, chtemp);
    
   sprintf(chtemp, "%f", Changing.altitude);
   fl_set_input(fd_select_change->altitude, chtemp);

  if ((strcmp(Changing.status, "G")==0)||(strcmp(Changing.status, "g")==0) ||
      (strcmp(Changing.status, "Y")==0)||(strcmp(Changing.status, "y")==0) ||
      (strcmp(Changing.status, "R") == 0)||(strcmp(Changing.status, "r")==0))
   sprintf(chtemp, "%s", Changing.status);
  else 
   sprintf(chtemp, "%s", "G");
   
   fl_set_input(fd_select_change->status, chtemp);
   
   if (Changing.north == 0)
      sprintf(chtemp, "%s", "F");
   else sprintf(chtemp, "%s", "T");
   fl_set_object_label(fd_select_change->northtext, chtemp);

   if (Changing.east == 0)
      sprintf(chtemp, "%s", "F");
   else sprintf(chtemp, "%s", "T");
   fl_set_object_label(fd_select_change->easttext, chtemp);

   sprintf(chtemp, "%f", Changing.lat_deg);
   fl_set_object_label(fd_select_change->latdegtext, chtemp);

   sprintf(chtemp, "%f", Changing.lat_min);
   fl_set_object_label(fd_select_change->latmintext, chtemp);

   sprintf(chtemp, "%f", Changing.lat_sec);
   fl_set_object_label(fd_select_change->latsectext, chtemp);

   sprintf(chtemp, "%f", Changing.lon_deg);
   fl_set_object_label(fd_select_change->londegtext, chtemp);

   sprintf(chtemp, "%f", Changing.lon_min);
   fl_set_object_label(fd_select_change->lonmintext, chtemp);
   sprintf(chtemp, "%f", Changing.lon_sec);
   fl_set_object_label(fd_select_change->lonsectext, chtemp);

   fl_show_form(fd_select_change->select_change,
                FL_PLACE_CENTER, FL_FULLBORDER, "Select Change");

}

//<--------------Check repeated sensors-------------------------------->

/* CHECKED Looks though sensor array and makes sure
there is only one of each type of sensor and it contains
the most current information if the user has made changes
*/

void CheckRepeatedSensors()
{
   int i = 0;
   int pre_found = 0;

   if (Current.sensor == 1)
   {
   while (i < array_place)
   {
      if (strcmp(Save_Data[i].sensor_type, Current.sensor_type) == 0)
      {
         Save_Sensors[i] = CurrentSensors;
         pre_found = 1;
      }

      i = i + 1;
   }

   if (pre_found != 1)
   {
      Save_Sensors[array_place] = CurrentSensors;
      sensor_array = sensor_array + 1;
   }
   }
}
